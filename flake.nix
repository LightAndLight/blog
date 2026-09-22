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

        pkgsHs = import nixpkgs {
          inherit system;
          overlays = [
            (final: prev: {
              haskellPackages =
                let
                  generated = prev.haskellPackages.extend (import ./nix/generated/overlay.nix);
                in
                  generated.extend (hfinal: hprev: {
                    blog-lib = hprev.callPackage ./lib/blog-lib.nix {};
                  });
            })
          ];
        };
      in {
        packages.haskellPackages = pkgsHs.haskellPackages;
        packages.blog-cli = pkgsHs.haskellPackages.callPackage ./cli/blog-cli.nix {};
        packages.blog-server = (pkgsHs.haskellPackages.callPackage ./server/blog-server.nix {}).overrideAttrs (old: {
          # TODO: enable tests
          #
          # The test suite uses Cabal to build `blog-server`, and then runs the
          # state machine tests against a `blog-server` process. It's not obvious
          # to me how to make that work in Nix's `checkPhase`. I think the easiest
          # solution would be to expose the `blog-server` entrypoint as a Haskell
          # value and run that in a separate thread.
          doCheck = false;
        });

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
