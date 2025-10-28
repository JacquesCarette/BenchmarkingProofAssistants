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
    panbench/grammar/panbench-grammar.cabal
    panbench/generators/panbench-generators.cabal
    panbench/site/panbench-site.cabal

profiling: True
profiling-detail: late-toplevel

-- Don't profile libraries, as this blows up the profile size.
package *
    profiling: True
    profiling-detail: late-toplevel
    ghc-options: -finfo-table-map -fdistinct-constructor-tables

