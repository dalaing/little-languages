{ mkDerivation, ansi-wl-pprint, base, bifunctors, mtl, stdenv
, trifecta
}:
mkDerivation {
  pname = "ll-util";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    ansi-wl-pprint base bifunctors mtl trifecta
  ];
  license = stdenv.lib.licenses.bsd3;
}
