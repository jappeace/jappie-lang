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
        overrides = hnew: hold: let
          llvmSrcPure =
            (builtins.fetchGit {
                url = "https://github.com/jappeace/llvm-hs";
                ref = "jappie-patch";
                rev = "3c9a5ff0c78bce9925eb3c6a2360f5d660f0047f";
            })
          ;
          llvmSrc2 =
            (builtins.fetchGit {
                url = "https://github.com/jappeace/llvm-hs";
                ref = "jappie-patch";
                rev = "ed5a28b45e9aefd9369d8baab2ff578178ba8744";
            })
          ;
        in {
          jappie-lang  = hnew.callCabal2nix "jappie-lang" ./. { };
          llvm-hs =
            (pkgs.haskell.lib.overrideCabal
              (hnew.callCabal2nix "llvm-hs" "${llvmSrc2}/llvm-hs" {}) { libraryToolDepends = [ pkgs.llvmPackages_12.libllvm ]; });
          llvm-hs-pure = (hnew.callCabal2nix "llvm-hs-pure" "${llvmSrcPure}/llvm-hs-pure" {});
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
