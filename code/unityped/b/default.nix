{ mkDerivation, ansi-wl-pprint, base, haskeline, lens, parsers
, QuickCheck, stdenv, tasty, tasty-quickcheck, trifecta
, unordered-containers
}:
mkDerivation {
  pname = "b";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base lens parsers QuickCheck trifecta
    unordered-containers
  ];
  executableHaskellDepends = [ base haskeline ];
  testHaskellDepends = [ base QuickCheck tasty tasty-quickcheck ];
  license = stdenv.lib.licenses.bsd3;
}
