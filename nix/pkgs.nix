import ./pin.nix {
  config = {

    packageOverrides = pkgs: {
        haskell = pkgs.lib.recursiveUpdate pkgs.haskell {
        packageOverrides = hpNew: hpOld: {
            jappie-lang = hpNew.callPackage ../default.nix {};
            };
        };
    };
  };
}
