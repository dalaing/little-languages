{ mkDerivation, base, bound, prelude-extras, QuickCheck, stdenv }:
mkDerivation {
  pname = "lc-named";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bound prelude-extras QuickCheck ];
  license = stdenv.lib.licenses.bsd3;
}
