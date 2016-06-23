with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        common-lang = self.callPackage ../common-lang {};
        b-lang = self.callPackage ./. {};
      };
    };
in pkgs.myEnvFun {
     name = modifiedHaskellPackages.b-lang.name;
     buildInputs = [
       (modifiedHaskellPackages.ghcWithPackages (hs: ([
         hs.cabal-install
         hs.hscolour
         hs.ghc-mod
       ] ++ hs.b-lang.propagatedNativeBuildInputs)))
     ];
   }

