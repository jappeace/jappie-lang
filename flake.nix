# I used chatgpt to generate this template and then just
# modified to how I normally use these things.
{
  description = "My Haskell project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-compat }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      hpkgs = pkgs.haskell.packages.ghc944.override {
        overrides = hnew: hold: {
          jappie-lang  = hnew.callCabal2nix "jappie-lang" ./. { };
        };
      };
    in
    {
      defaultPackage.x86_64-linux =  hpkgs.jappie-lang;
      inherit pkgs;
      devShell.x86_64-linux = hpkgs.shellFor {
        packages = ps : [ ps."jappie-lang" ];
        withHoogle = true;

        buildInputs = [
          pkgs.ghcid
          pkgs.haskellPackages.hasktags
          pkgs.cabal-install
        ];
      };
    };
}
