-- Use multiple cores to build, but don't fork bomb our system.
jobs: $ncpus
semaphore: true

-- Need a more recent version of shake to work around deprecation
-- of ErrorCallWithLocation in GHC 9.12.0.
if(impl(ghc >= 9.12.0))
  source-repository-package
      type: git
      location: https://github.com/ndmitchell/shake.git
      tag: c69037e5e2d150bcb79be366d081688ad00e4c71

packages:
    lib/grammar/panbench-grammar.cabal
    lib/generators/panbench-generators.cabal
    lib/shake/panbench-shake.cabal
    app/site/panbench-site.cabal

 -- Only profile local packages to avoid blowing up the profile size.
profiling: True
profiling-detail: late-toplevel
program-options
    -- We use a lot of typeclasses throughout panbench, so aggressive specialization
    -- can really help performance.
    ghc-options: -fspecialise-aggressively -fexpose-all-unfoldings

package panbench-site
    -- Annoying workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/18320
    ghc-options: -fexternal-interpreter

package *
    optimization: 2
    -- We may as well add info tables to all packages so that -hi profiling is
    -- more informative.
    ghc-options: -finfo-table-map -fdistinct-constructor-tables
