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

typedef struct {
  int64_t resource; /**< The resource to limit, see <sys/resource.h> */
  uint64_t resource_limit; /**< The limit of the resource. */
} ResourceLimit;

/*
Constants for use with <sys/resource.h>

These have to be re-exported in a somewhat roundabout way so that we can
use them inside of the Haskell FFI. We can't directly include <sys/resource.h>
to get the #defines of RLIMIT_CPU et al, as this breaks HLS
(see https://github.com/haskell/haskell-language-server/issues/689)

To work around this, we use the FFI to access these constants indirectly.
*/
extern const uint64_t rlimit_cpu;
extern const uint64_t rlimit_rss;

// GHC can't handle const pointers so let's just discard them.
extern int c_benchmark(const char *path, char *const argv[], char *const envp[], ResourceLimit rlimits[], size_t nlimits, BenchmarkStats *const bench);
