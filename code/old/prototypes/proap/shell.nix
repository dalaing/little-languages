with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        proap = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.proap.env
