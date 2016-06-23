{ mkDerivation, ansi-wl-pprint, base, bound, containers, mtl
, parsers, prelude-extras, QuickCheck, stdenv, trifecta
, unordered-containers
}:
mkDerivation {
  pname = "frag";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    ansi-wl-pprint base bound containers mtl parsers prelude-extras
    QuickCheck trifecta unordered-containers
  ];
  license = stdenv.lib.licenses.bsd3;
}
