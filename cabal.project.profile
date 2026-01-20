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

profiling: True
profiling-detail: late-toplevel

package panbench-site
    -- Annoying workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/18320
    ghc-options: -fexternal-interpreter


-- Don't profile libraries, as this blows up the profile size.
package *
    profiling: True
    profiling-detail: late-toplevel
    ghc-options: -finfo-table-map -fdistinct-constructor-tables

