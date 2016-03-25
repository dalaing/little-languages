{ mkDerivation, ansi-wl-pprint, base, bound, haskeline, lens
, parsers, prelude-extras, QuickCheck, stdenv, tasty
, tasty-quickcheck, transformers, trifecta, unordered-containers
}:
mkDerivation {
  pname = "lc";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base bound lens parsers prelude-extras QuickCheck
    trifecta unordered-containers
  ];
  executableHaskellDepends = [
    ansi-wl-pprint base haskeline transformers
  ];
  testHaskellDepends = [ base QuickCheck tasty tasty-quickcheck ];
  license = stdenv.lib.licenses.bsd3;
}
