{ mkDerivation, ansi-wl-pprint, base, bound, containers, lens, mtl
, parsers, prelude-extras, QuickCheck, stdenv, tasty
, tasty-quickcheck, trifecta, unordered-containers
}:
mkDerivation {
  pname = "stlc-b";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    ansi-wl-pprint base bound containers lens mtl parsers
    prelude-extras QuickCheck trifecta unordered-containers
  ];
  testHaskellDepends = [
    base mtl QuickCheck tasty tasty-quickcheck trifecta
  ];
  license = stdenv.lib.licenses.bsd3;
}
