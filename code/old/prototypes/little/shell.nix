with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        little = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.little.env
