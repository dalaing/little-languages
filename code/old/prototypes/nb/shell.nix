with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        nb = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.nb.env
