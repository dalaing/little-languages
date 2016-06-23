with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        lc-named = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.lc-named.env
