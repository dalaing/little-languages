with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        type = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.type.env
