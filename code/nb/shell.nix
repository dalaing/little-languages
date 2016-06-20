with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        n-lang = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.n-lang.env

