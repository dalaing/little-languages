with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        lc = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.lc.env
