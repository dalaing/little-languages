with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        classy = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.classy.env
