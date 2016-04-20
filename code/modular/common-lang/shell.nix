with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        common-lang = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.common-lang.env

