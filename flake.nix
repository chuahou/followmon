{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  # haskell.nix binary cache
  nixConfig = {
    extra-substituters = "https://cache.iog.io";
    extra-trusted-public-keys = "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=";
  };

  outputs = inputs@{ self, nixpkgs, haskellNix, ... }:
  let
    system = "x86_64-linux";
    overlays = [
      haskellNix.overlay
      (self: super: {
        followmon = self.haskell-nix.project' {
          src = self.haskell-nix.haskellLib.cleanSourceWith {
            name = "followmon-source";
            src = ./.;
          };
          compiler-nix-name = "ghc925"; # Keep synced with cabal.project.
        };
      })
    ];
    pkgs = import nixpkgs {
      inherit system overlays;
      inherit (haskellNix) config;
    };
    flake = pkgs.followmon.flake {};

  in flake // {
    defaultPackage.${system} = flake.packages."followmon:exe:followmon";
    devShell.${system} = pkgs.followmon.shellFor {
      tools = {
        haskell-language-server = {
          version = "1.9.0.0";
          index-state = "2023-01-22T00:00:00Z";
          materialized = ./materialized/haskell-language-server;
        };
        hoogle = {
          version = "5.0.18.3";
          index-state = "2023-01-22T00:00:00Z";
          materialized = ./materialized/hoogle;
        };
      };
      nativeBuildInputs = with pkgs; [
        cabal-install
        hpack
      ];
      exactDeps = true;
    };
  };
}
