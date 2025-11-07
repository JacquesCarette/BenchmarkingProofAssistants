# Adding new tests

Below all paths are relative to where you installed PanBench.

## Where?

You will want to add your tests to `panbench/generators/src/Panbench/Generator/` directory (or in
a sub-directory of that). To plug it in as a generator, you'll need to add it to
`panbench/generators/panbench-generators.cabal`.

Once that's done (see below for the details of creating an actual test), you should then "plug it in"
as a test. There are actually two places to do that:
1. as a benchmark test
2. as a golden test

As a benchmark, your test is supposed to generate things of various sizes. A "golden" test is a
test of the pretty-printers: it is to make sure that your test "looks" as you expect it to in all
supported languages. You should pick a small size for that purpose (we uniformly pick '5').

### Benchmark

The benchmark test needs to be added to `panbench/site/app/Main.hs`. For our example test, we add
the import
```haskell
import Panbench.Generator.IdChain qualified as IdChain
```
Note that our convention is to keep this list of imports sorted alphabetically. This then lets
us add a new block to the `benchmarks` list
```haskell
  , BenchmarkMatrix "IdChain" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow Agda IdChain.generator defaultTimeout
    , benchmarkMatrixRow Idris IdChain.generator defaultTimeout
    , benchmarkMatrixRow Lean IdChain.generator defaultTimeout
    , benchmarkMatrixRow Rocq IdChain.generator defaultTimeout
    ]
```
Most tests naturally want doubling in size to show interesting behaviour. But some
tests will be different and need even fast growth or slower (ex: linear) growth.
Similarly, the upper bound should be tweaked until most systems show non-trivial
behaviour.

### Golden

The golden test needs to be hooked in to `panbench/generators/test/golden/Main.hs` with the
imports as above. Unlike the benchmarks, the golden tests are arranged by system, and so
one line for each system needs to be added to the right block. For our case, the Agda addition
looks like
```haskell
   , agdaModuleTest IdChain.generator 5
```
with the other languages being similar.

## What?

Let us pick a "chain of calls to the identity function" as a test. In Agda, doing this by hand,
this would look like
```agda
id : {A : Type} -> A -> A
id x = x

test : {A : Type} -> A -> A
test = id id id id id
```

Now we want to do the same in Panbench. The structure of the code above has:
- no imports
- two definitions (one function, one test)

To get started, our new test, `IdChain.hs` gets things ready:
```haskell
module Panbench.Generator.IdChain where

import Numeric.Natural

import Panbench.Generator
import Panbench
```

`Numeric.Natural` is needed because that is the type of the size parameter, and the other
two contain all the basic infrastructure for writing Panbench programs. Without further ado,
let's look at the resulting code, and then we'll explain all the pieces.

```haskell
generator :: (Constant tm "Type",
  Definition defns lhs tm, TelescopeLhs lhs hd cell,
  Chk nm tm cell, Chk nm tm hd,
  Name nm, Implicit cell, Name tm, App tm) => GenModule hdr defns Natural
generator =
    GenModule "IdChain"
      [
      ] \size ->
      [ ([implicit ("A" .: builtin "Type"), ("x" .: "A")] |- ("id" .: "A") .= "x")
      ,
        [implicit ("A" .: builtin "Type"), "x" .: "A"] |- "test" .: "A" .=
           foldr (\_ tm -> app "id" [tm]) "id" [1..size]
      ]
```

The 'plug in' code in the previous section already assumed that the main export of a generator
is a function called `generator`. Its type will always be `GenModule hdr defns xxx` where
`xxx` is going to be the type of inputs of the generator -- usually `Natural`, as here.

But as is visible above, the `generator` function has a complex set of constraints. This is because
the Panbench language is written in modern finally tagless style:
1. A collection of classes, one class per feature of the language
2. Classes has multiple parameters, which relates the various parts of the language
3. A language contains multiple sub-languages that need to be coordinated.

The full details can be seen in `panbench/grammar/src/Panbench/Grammar.hs`, but we will here
illustrate all that is needed for this example.

The way to understand these constraints is as a *grammar definition*. In
general, a module declaration has a header `hdr` language (usually a list of
imports, here empty) and a collection of definitions `defns`.  The language of
definitions is constrained by `Definition defns lhs tm` which says that
definitions are made up of the sub-languages `lhs` of left hand sides and `tm`
of terms.  Terms themselves must support a number of features: being able to
define names `Name tm`, being able to form applications `App tm`, and having a
constant (named "Type"). These left hand sides, in turn, must implement the
`TelescopeLhs lhs hd cell` feature, which consists of a head `hd` (the thing
we're defining) and some binding cells `cell` (the parameters). Here we use
annotated single-binding cells via the `Chk nm tm cell` and `Check nm tm hd`
constraints. The representation of the `cell` itself itself will link a name
`nm` with a term `tm`. We see that the only required feature of the `nm`
language is that it lets us have `Name`.

Like in the ordinary (single `repr`, untyped) finally tagless, all of the type variables
`tm`, `defns`, `hd`, `cell` and so on are placeholders for *representations* of the aspect of the
(syntax) of the language.

A module then has three parts:
1. a name (as a String)
2. a header (as a list of things, here empty)
3. a function from size to a list of definitions.

Both definitions here have the same shape: ` [a, b, c] |- (x .: y) .= z ` .
The `.=` introduces a definition, with `z` the body, and the rest defining the name of what
we are defining (here `x`), its result type (`y`) and the telescope / context in which it
makes sense, i.e. the declarations `[a, b, c]`.

In more detail, the "id" function works under the assumption of an (implicit) binding of "A"
of 'type' the builtin "Type", and "x" of type "A".

The definition of "test" much looks the same, except that its body is a generator of nested
'app' applications (where `app` is the main method of the `App` class that forms a term from
a term and a list of terms).

It is worthwhile to remember that Panbench is about the *surface syntax* of dependently typed
languages. So Panbench ensures that its programs are *structurally sound* as far as the 'shape'
of the syntax is concerned, but makes no other promises. In other words, Panbench promises to
make the parsers of the target languages happy, and makes no promises whatsoever about their
type checkers. 

---

Everything below here is potentially out-of-date and should not be considered correct.

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

# Changing the test harness


Be aware that if you change the test harness, you can cause [Max RSS usage to beinaccurate on Linux](https://github.com/JacquesCarette/BenchmarkingProofAssistants/issues/196)
if the `panbench-site` process goes over 40MB Max RSS.

