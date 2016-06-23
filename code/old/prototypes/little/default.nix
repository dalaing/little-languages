{ mkDerivation, base, bifunctors, errors, generics-eot, lens
, profunctors, QuickCheck, stdenv
}:
mkDerivation {
  pname = "little";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors errors generics-eot lens profunctors QuickCheck
  ];
  license = stdenv.lib.licenses.bsd3;
}
