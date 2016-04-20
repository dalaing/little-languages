{ mkDerivation, ansi-wl-pprint, base, lens, mtl, parsers
, QuickCheck, stdenv, trifecta, unordered-containers
}:
mkDerivation {
  pname = "common-lang";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    ansi-wl-pprint base lens mtl parsers QuickCheck trifecta
    unordered-containers
  ];
  license = stdenv.lib.licenses.bsd3;
}
