{ mkDerivation, ansi-wl-pprint, base, common-lang, lens, mtl
, parsers, QuickCheck, stdenv, trifecta, unordered-containers
}:
mkDerivation {
  pname = "b-lang";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    ansi-wl-pprint base common-lang lens mtl parsers QuickCheck
    trifecta unordered-containers
  ];
  license = stdenv.lib.licenses.bsd3;
}
