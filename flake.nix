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
        packages.default = pkgs.symlinkJoin {
          name = "bully-with-programs";
          paths = [ self.packages.${system}.bully-server ];
          nativeBuildInputs = [ pkgs.makeWrapper ];

          postBuild = ''
            wrapProgram $out/bin/bully-server \
              --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.typst ]}
          '';
        };

        packages.bully = haskellPackages.callCabal2nix "bully" ./. { };
        packages.bully-server = self.packages.${system}.bully.overrideAttrs
          (oldAttrs: { meta.mainProgram = "bully-server"; });
        packages.bully-example = self.packages.${system}.bully.overrideAttrs
          (oldAttrs: { meta.mainProgram = "bully-example"; });

        defaultPackage = self.packages.${system}.bully-server;

        devShell = self.packages.${system}.bully.env.overrideAttrs (oldAttrs: {
          buildInputs = (oldAttrs.buildInputs or [ ])
            ++ (with haskellPackages; [
              cabal2nix
              haskell-language-server
              fourmolu
              cabal-fmt
            ]);
        });
      });
}
