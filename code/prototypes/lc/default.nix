{ mkDerivation, ansi-wl-pprint, base, bound, prelude-extras
, QuickCheck, stdenv
}:
mkDerivation {
  pname = "lc";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    ansi-wl-pprint base bound prelude-extras QuickCheck
  ];
  license = stdenv.lib.licenses.bsd3;
}
