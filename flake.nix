{
  inputs.nixpkgs.url = "nixpkgs/nixpkgs-unstable";

  outputs = inputs@{ self, nixpkgs, ... }:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};

    project = returnShellEnv: pkgs.haskellPackages.developPackage {
      root = ./.;

      # Add build tools for shell env.
      modifier = drv:
        pkgs.haskell.lib.addBuildTools drv
          (pkgs.lib.optionals returnShellEnv (with pkgs.haskellPackages; [
            cabal-install
            hpack
            haskell-language-server
          ]));
      inherit returnShellEnv;
    };

    # Wrap static executable with runtime deps.
    static = pkgs.haskell.lib.justStaticExecutables (project false);

  in {
    packages.${system}.default = static;
    devShells.${system}.default = project true;
  };
}
