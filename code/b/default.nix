{ mkDerivation, ansi-wl-pprint, base, doctest, doctest-discover
, haskeline, mtl, parsers, QuickCheck, stdenv, tasty
, tasty-quickcheck, transformers, trifecta, unordered-containers
}:
mkDerivation {
  pname = "b";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base mtl parsers QuickCheck trifecta
    unordered-containers
  ];
  executableHaskellDepends = [
    ansi-wl-pprint base haskeline transformers
  ];
  testHaskellDepends = [
    base doctest doctest-discover QuickCheck tasty tasty-quickcheck
  ];
  license = stdenv.lib.licenses.bsd3;
}
