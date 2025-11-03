# Panbench

Panbench is a comparative benchmarking tool for dependently-typed programming languages.
It consists of two portions:

1. A DSL that defines a common grammar for dependently-typed programming languages.
2. A benchmarking harness for installing tools, running tests, and producing reports.

To date, panbench supports the following languages:

- Agda 2
- Idris 2
- Lean 4
- Rocq 9

The current benchmarking results are available at <https://jacquescarette.github.io/BenchmarkingProofAssistants/.

# Using Panbench

## Installing Panbench

### Linux and MacOS

To install panbench, you will first need a working Haskell toolchain. The suggested
way to do this is via [ghcup](https://jacquescarette.github.io/BenchmarkingProofAssistants/. "ghcup").
Note that `panbench` requires a version of GHC that is at least `9.10.1`, and a version of Cabal
that is at least `3.14.2.0`.

Once you have a working copy of GHC and cabal, you can compile panbench with

```sh
cabal build all
```

### Windows

Panbench does not currently support windows, though we are interested in fixing this!

## Running Panbench

Once you've installed panbench, you can create a full benchmarking report by running

```sh
cabal run panbench-site -- +RTS -A1M -c -RTS _build/site/index.html
```

This will do a sandboxed build of all of the tools, generate all of our test files,
and run all the benchmarks and generate a standalone HTML report file. You can view
this file by just opening it in your browser with

```sh
open _build/site/index.html
```

> [!WARNING]  
> Doing a full sandboxed builds of all of the tools can take upwards of 4 hours.

To see the other build options available, simply run

```sh
cabal run panbench-site -- --help
```

### Benchmarking against different versions of tools

The easiest way to change the versions of the tools being benchmarked is
to use the `${LANG}_VERSION` environment variables, which allow you to specify an arbitrary git
revision to benchmark against. For instance the following command will clone, build, and benchmark
the latest `main` of Agda and the commit `b8bd91d92b4f0ec99e5ee08701ac18f956d2c316` of Lean 4.

```sh
AGDA_VERSION=main LEAN_VERSION=b8bd91d92b4f0ec99e5ee08701ac18f956d2c316 cabal run panbench-site -- +RTS -A1M -c -RTS _build/site/index.html
```

For a full list of options, run `cabal run panbench-site -- --help`.

# Developing Panbench

For information on developing new benchmarks or backends, see [CONTRIBUTING.md](https://github.com/JacquesCarette/BenchmarkingProofAssistants/blob/main/CONTRIBUTING.md).

# Acknowledgements

Panbench started its life as the McMaster CS4ZP6A capstone project. The
original development team consisted of:

- Proyetei Akanda
- Esha Pisharody
- Zainab Abdulsada
- Grace Croome
- Marie Hollington
- Emma Willson

