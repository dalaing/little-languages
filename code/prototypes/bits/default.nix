{ mkDerivation, base, bound, prelude-extras, stdenv }:
mkDerivation {
  pname = "bits";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bound prelude-extras ];
  license = stdenv.lib.licenses.bsd3;
}
