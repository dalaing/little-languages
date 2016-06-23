{ mkDerivation, ansi-wl-pprint, base, bifunctors, bound, containers
, haskeline, lens, mtl, parsers, prelude-extras, QuickCheck, stdenv
, tasty, tasty-quickcheck, transformers, trifecta
, unordered-containers
}:
mkDerivation {
  pname = "stlc-b";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base bifunctors bound containers lens mtl parsers
    prelude-extras QuickCheck trifecta unordered-containers
  ];
  executableHaskellDepends = [
    ansi-wl-pprint base haskeline transformers
  ];
  testHaskellDepends = [
    base mtl QuickCheck tasty tasty-quickcheck trifecta
  ];
  license = stdenv.lib.licenses.bsd3;
}
