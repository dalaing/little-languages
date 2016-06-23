with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        lc-full = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.lc-full.env
