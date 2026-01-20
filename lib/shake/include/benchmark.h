#pragma once

#include <sys/types.h>
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


// GHC can't handle const pointers so let's just discard them.
extern int c_benchmark(const char *path, char *const argv[], char *const envp[], uint64_t timeout, BenchmarkStats *const bench);
