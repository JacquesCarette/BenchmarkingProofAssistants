# Running the test suite

You can run the test suite with

```sh
cabal test panbench-generators
```

This will output files to `translator/test/staging/`, which are then
diffed against the corresponding file in `translator/test/snapshot`.

To promote output file in the staging area to snapshots, run

```sh
cabal test panbench-generators --test-options="--accept"
```

If you add a new golden test, the first run of the test suite
will create a new snapshot file. Make sure to commit this 
file as part of your PR!

# Profiling

The easiest way to create a profiled build of `panbench` is by running the following command.

```sh
cabal build all --project-file cabal.project.profile --builddir=dist-prof
```

We can then generate a heap profile using the following command.

```sh
cabal run panbench-site --project-file cabal.project.profile --builddir=dist-prof -- +RTS -l-aug -pj -RTS _build/site/index.html
```

The best way to view these heap profiles is to use [eventlog2html](github.com/mpickering/eventlog2html).
