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

    systems = {
      url = "github:nix-systems/default";
    };
  };

  outputs = { self, nixpkgs, systems, haskellNix }:
    let
      eachSystem = cont: nixpkgs.lib.genAttrs (import systems) (system:
        let
          overlays = [
            haskellNix.overlay (final: prev: {
              # This fixes libtinfo, which expects a version of ncurses built
              # without unicode support.
              #
              # See https://github.com/NixOS/nixpkgs/pull/390887
              libtinfo = prev.libtinfo.overrideAttrs (old: {
                configureFlags = ["--disable-widec"] ++ old.configureFlags;
              });
              # This overlay adds our project to pkgs
              panbench =
                final.haskell-nix.cabalProject {
                  src = ./.;
                  compiler-nix-name = "ghc912";
                };
            })
          ];
        in cont rec {
          inherit system;
          pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
          flake = pkgs.panbench.flake {};
        });
    in {
      packages = eachSystem({system, pkgs, flake, ...}: {
        default = flake.packages."panbench-site:exe:site";
      });
      devShells = eachSystem({system, pkgs, ...}:
        {
          default = pkgs.panbench.shellFor {
            tools = {
              cabal = "3.14.2.0";
              haskell-language-server = "latest";
            };
            nativeBuildInputs = [
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
        }
      );
    };
}
