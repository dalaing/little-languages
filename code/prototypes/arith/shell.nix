with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        arith = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.arith.env
