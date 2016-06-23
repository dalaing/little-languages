with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        prod = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.prod.env
