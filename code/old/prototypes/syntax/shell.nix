with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        syntax = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.syntax.env
