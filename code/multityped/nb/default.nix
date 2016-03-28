{ mkDerivation, ansi-wl-pprint, base, lens, mtl, parsers
, QuickCheck, stdenv, tasty, tasty-quickcheck, trifecta
, unordered-containers
}:
mkDerivation {
  pname = "nb";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    ansi-wl-pprint base lens mtl parsers QuickCheck trifecta
    unordered-containers
  ];
  testHaskellDepends = [ base QuickCheck tasty tasty-quickcheck ];
  license = stdenv.lib.licenses.bsd3;
}
