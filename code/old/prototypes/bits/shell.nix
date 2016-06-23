with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        bits = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.bits.env
