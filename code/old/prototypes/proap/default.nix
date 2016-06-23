{ mkDerivation, base, bifunctors, generics-eot, lens, profunctors
, QuickCheck, semigroupoids, semigroups, stdenv
}:
mkDerivation {
  pname = "proap";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors generics-eot lens profunctors QuickCheck
    semigroupoids semigroups
  ];
  license = stdenv.lib.licenses.bsd3;
}
