{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, ansi-wl-pprint, base, lens, mtl, parsers
      , QuickCheck, stdenv, tasty, tasty-quickcheck, trifecta
      , unordered-containers
      }:
      mkDerivation {
        pname = "nb-srcloc";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          ansi-wl-pprint base lens mtl parsers QuickCheck trifecta
          unordered-containers
        ];
        testHaskellDepends = [
          base mtl QuickCheck tasty tasty-quickcheck trifecta
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
