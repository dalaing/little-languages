with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        b-lang = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.b-lang.env

