{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    hdeps = {
      url = "github:LightAndLight/hdeps";
      inputs = {
        flake-utils.follows = "flake-utils";
      };
    };
  };
  outputs = { self, nixpkgs, flake-utils, hdeps }:
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = import nixpkgs { inherit system; };
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.ghc
            cabal-install
            haskell-language-server

            just
            haskellPackages.fourmolu
            haskellPackages.implicit-hie
            hdeps.packages.${system}.default
            cabal2nix
            fd

            zlib
            openssl
          ];
        };
      }
    );
}
