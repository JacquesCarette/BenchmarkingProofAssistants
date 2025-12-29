// Shims for getting resource usage of commands.
// [TODO: Reed M, 17/06/2025] Windows support.

#include <errno.h>
#include <fcntl.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <unistd.h>

#include <Rts.h>

/**
@brief Benchmarking statistics.
*/
typedef struct {
  Time bench_user_time; /**< Amount of time we spent in user code, measured in nanoseconds. */
  Time bench_system_time; /**< Amount of time we spent in system code, measured in nanoseconds. */
  int64_t bench_max_rss; /**< Max resident set size, measured in bytes.*/
  int64_t bench_exit_code; /**< Exit code of benchmarked executable. */
} BenchmarkStats;

/**
@brief Pause the GHC RTS, run an executable, record runtime statistics, and
resume the RTS.

If this function fails for any reason (couldn't fork, couldn't find the
executable, etc.), we return -1, keep {@code errno} set, and resume the RTS.

@param[in] path       The path to the executable to run.
@param[in] argv       The arguments to pass to the executable.
@param[in] envp       The environment variables to pass to the executable.
@param[in] timeout    A timeout, specified in seconds.
@param[out] bench The results of a benchmarking run.
@return 0 if we were able to run the executable, -1 if there was some fatal error.
*/
int c_benchmark(const char *path, char *const argv[], char *const envp[], uint64_t timeout, BenchmarkStats *const bench) {
  // Pause the entire GHC runtime system.
  // This includes:
  // * All haskell threads.
  // * Garbage collection.
  //
  // Notably, this does *not* include any safe FFI calls, which
  // includes this very function! This means that there is a *tiny* window where
  // we could be in the process of starting the FFI call where another
  // thread could also call `c_benchmark`. Lucikly, the GHC developers have
  // our backs, and calls to `rts_pause` on other threads will block if the RTS is already paused.
  //
  // See Note [Locking and Pausing the RTS] in rts/include/RtsAPI.h
  PauseToken *rts_tok = rts_pause();

  // We want to be able to communicate errors between our child and parent
  // process in the event that `execve` fails.
  int fork_fds[2];
  if (pipe(fork_fds)) {
    // Just bail out immediately: we probably ran out of file descriptors,
    // so trying to reopen a pipe is a bad idea. Let's just keep `errno` set,
    // unpause the RTS and hope for the best.
    rts_resume(rts_tok);
    return -1;
  };

  // Fork and try to `execve` on the child process.
  // We avoid using `execvpe` for two reasons:
  // * We'd like to use `wait4` to get the resource usage statistics of just
  //   this one process.
  // * `execvpe` is not available on MacOS.
  pid_t pid = fork();
  if (pid == -1) {
    // Couldn't fork: we probably have too many processes or ran out of swap
    // space. Trying to proceed is probably a bad idea, so we again keep `errno`
    // set and unpause the RTS.
    rts_resume(rts_tok);
    return -1;
  } else if (pid == 0) {
    // Child process.
    //
    // Before we let the benchmark rip, we have some housekeeping we need to do.
    // Close the read site of the pipe, and ensure the write side closes after
    // a successful `execve`.
    close(fork_fds[0]);
    fcntl(fork_fds[1], F_SETFD, FD_CLOEXEC);

    // Finally, we set a timeout.
    if (setrlimit(RLIMIT_CPU, &((struct rlimit) { .rlim_cur = timeout, .rlim_max = timeout })) == -1) {
      goto child_fail;
    }

    // [FIXME: Reed M, 17/06/2025] Set up stdin, stderr, and stdout descriptors
    // for the child process. Right now, will just inherit from the parent, and
    // we probably want to send them to a string?

    if (execve(path, argv, envp) == -1) {
      // If `execve` is successful, it doesn't return.
      // On failure, it will set `errno`, which we then ship over
      // to the parent process.
      goto child_fail;
    }
  child_fail:
      // If this write fails then there's literally nothing we can do.
      // The `if` here lets us locally ignore -Wunused-result.
      if(write(fork_fds[1], &errno, sizeof(int))){};
      // Don't bother with the usual exit cleanup: we've got no buffers to flush.
      _exit(127);
  }
  // Parent process.
  //
  // Usual pipe housekeeping: close write end.
  close(fork_fds[1]);
  // Linux considers `wait4` to be a legacy syscall and suggests that you use
  // `waitpid` along with `getrusage`, but that only lets you get resouce
  // usage of ALL children if you pass `RUSAGE_CHILDREN`.
  int status = 0;
  struct rusage usage;
  if (wait4(pid, &status, 0, &usage) == -1) {
    // Couple of possibilities here:
    // * There are no children we can await.
    //   This *should* be impossible, as the fork succeeded.
    // * Some signal was caught that aborted the call.
    // * The PID file descriptor is nonblocking, which should also be
    //   impossible: there isn't a `PIDFD_NONBLOCK` in sight.
    //
    // Either way: something terrible went wrong, so our best bet
    // is to keep `errno` set and unpause the RTS.
    rts_resume(rts_tok);
    return -1;
  }
  // At this point we know that the child process started, but
  // we have no idea if the `execve` was successful. Let's try
  // to read from the pipe and see if we get anything.
  int child_errno = 0;
  int r = read(fork_fds[0], &child_errno, sizeof(int));
  if (r == -1) {
    // There's really nothing here that could cause us to fail,
    // so things have gone pretty horribly wrong if we hit this
    // branch. Following the general theme, we preserve `errno`
    // and resume the RTS.
    rts_resume(rts_tok);
    return -1;
  } else if (r != 0) {
    // If we `read` from a closed pipe, it will return `0`. We actually
    // got something here, so the call to `execve` somehow: let's copy it's
    // errno to our errno and unpause the RTS.
    errno = child_errno;
    rts_resume(rts_tok);
    return -1;
  }
  // Everything is good!
  // Let's set up our return value, unpause the RTS, and return.
  // We still need to close the pipe ourselves though.
  close(fork_fds[0]);
  bench->bench_user_time = SecondsToTime(usage.ru_utime.tv_sec) + USToTime(usage.ru_utime.tv_usec);
  bench->bench_system_time = SecondsToTime(usage.ru_stime.tv_sec) + USToTime(usage.ru_stime.tv_usec);
  bench->bench_max_rss = usage.ru_maxrss;
  bench->bench_exit_code = status;

  // Linux reports memory usage in kb, not bytes!
#ifdef __linux__
  bench->bench_max_rss *= 1024;
#endif
  rts_resume(rts_tok);
  return 0;
}
