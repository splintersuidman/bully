{
  description = "A very basic flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        compiler = "ghc9103";
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.${compiler};
      in {
        packages.bully = haskellPackages.callPackage ./bully.nix { };

        defaultPackage = self.packages.${system}.bully;

        devShell = self.packages.${system}.bully.env.overrideAttrs
          (oldAttrs: {
            buildInputs = (oldAttrs.buildInputs or [ ])
              ++ (with haskellPackages; [
                cabal2nix
                haskell-language-server
                fourmolu
              ]);
          });
      });
}
