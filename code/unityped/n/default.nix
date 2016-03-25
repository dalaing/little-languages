{ mkDerivation, ansi-wl-pprint, base, haskeline, lens, parsers
, QuickCheck, stdenv, tasty, tasty-quickcheck, transformers
, trifecta, unordered-containers
}:
mkDerivation {
  pname = "n";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base lens parsers QuickCheck trifecta
    unordered-containers
  ];
  executableHaskellDepends = [
    ansi-wl-pprint base haskeline transformers
  ];
  testHaskellDepends = [ base QuickCheck tasty tasty-quickcheck ];
  license = stdenv.lib.licenses.bsd3;
}
