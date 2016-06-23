with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        frag = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.frag.env
