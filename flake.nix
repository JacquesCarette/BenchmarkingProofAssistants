{
  description = "A framework for benchmarking proof assistants";

  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/release-25.05"; # This is known to work on x86_64 darwin.
    };

    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
    };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { system, ... }: let
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            inputs.haskellNix.overlay (final: prev: {
              # This fixes libtinfo, which expects a version of ncurses built
              # without unicode support.
              #
              # See https://github.com/NixOS/nixpkgs/pull/390887
              libtinfo = prev.libtinfo.overrideAttrs (old: {
                configureFlags = ["--disable-widec"] ++ old.configureFlags;
              });
            })
          ];
          inherit (inputs.haskellNix) config;
        };
        panbench = pkgs.haskell-nix.cabalProject {
          compiler-nix-name = "ghc912";
          src = ./.;
          shell = {
            tools = {
              cabal = "3.14.2.0";
              haskell-language-server = "latest";
              eventlog2html = "latest";
            };
            nativeBuildInputs = [
              pkgs.coreutils # The idris2 binary is actually a shell script, and needs uname, readlink, and dirname.
              pkgs.chez
              pkgs.ccache
              pkgs.clang
              pkgs.cmake
              pkgs.libtinfo
              pkgs.libuv
              pkgs.gmp
              pkgs.gnumake
              pkgs.opam
              pkgs.pkg-config
              pkgs.python314 # Needed for some lean test suites
              pkgs.zlib
            ];
            exactDeps = true;
          };

          flake.variants = {
            # Alias for the defaut configuration.
            default = {};
            # Profiled builds
            profiled = {
              modules = [{
                # Make sure to install profiling libraries
                # See https://github.com/input-output-hk/haskell.nix/issues/887
                enableLibraryProfiling = true;
                # Similarly, see https://github.com/input-output-hk/haskell.nix/issues/1149
                ghcOptions = [
                  "-finfo-table-map"
                  "-fdistinct-constructor-tables"
                ];
              }];
            };
          };
        };
        panbench-tex = pkgs.texliveBasic.withPackages (ps: with ps; [
          # Binaries
          latexmk
          # TeX packages
          caption
          cleveref
          comment
          fontawesome5
          lineno
          listings
          microtype
          multirow
          soul
          everyshi
          mdwtools
          thmtools
          totpages
          threeparttable
          todonotes
          urlbst
          xstring
        ]);
      in {
        packages = {
          default = (panbench.projectVariants.default.flake {}).packages."panbench-site:exe:site";
          paper = pkgs.stdenvNoCC.mkDerivation rec {
            name = "panbench";
            src = ./paper;
            buildInputs = [ pkgs.coreutils panbench-tex ];
            phases = [ "unpackPhase" "buildPhase" "installPhase" ];
            # We pass -pretex="\pdftrailerid{}" to avoid calculating the PDF id from the system time.
            buildPhase = ''
              export PATH="${pkgs.lib.makeBinPath buildInputs}";
              mkdir -p .cache/texmf-var
              env TEXMFHOME=.cache TEXMFVAR=.cache/texmf-var \
              latexmk -interaction=nonstopmode -pdf -Werror \
              -pretex="\pdftrailerid{}" \
              panbench.tex
            '';
            installPhase = ''
              mkdir -p $out
              cp panbench.pdf $out/
            '';
          };
        };
        devShells = {
          default = panbench.projectVariants.default.shellFor {};
          profiled = panbench.projectVariants.profiled.shellFor {};
          paper = panbench.projectVariants.default.shellFor {
            nativeBuildInputs = [
              panbench-tex
            ];
          };
        };
      };
    };
}
