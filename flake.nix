{
  description = "A Haskell development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        project = import ./project.nix;
        projectName = project.name;

        haskellOverlay = final: prev: {
          myHaskellPackages = final.haskellPackages.override (oldHaskellPkgs: {
            overrides = final.lib.composeExtensions
              (oldHaskellPkgs.overrides or (_: _: { }))
              (hfinal: hprev: {
                ${projectName} = hfinal.callCabal2nix projectName ./. { };
              });
          });
        };

        pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskellOverlay ];
        };

        myDevTools = with pkgs; [
          cabal-install
          haskellPackages.ghcid
          haskell-language-server
          hlint
          ormolu
        ];
      in
      {
        packages.default = pkgs.myHaskellPackages.${projectName};

        devShells.default = pkgs.myHaskellPackages.${projectName}.env.overrideAttrs (oldEnv: {
          buildInputs = (oldEnv.buildInputs or [ ]) ++ myDevTools;
          shellHook = ''
            echo "Developing ${projectName} project..."
            alias repl="cabal new-repl"
          '';
        });
      });
}
