{ mkDerivation, base, bifunctors, bound, errors, lens, profunctors
, QuickCheck, semigroupoids, semigroups, singletons, stdenv, tagged
}:
mkDerivation {
  pname = "classy";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors bound errors lens profunctors QuickCheck
    semigroupoids semigroups singletons tagged
  ];
  license = stdenv.lib.licenses.bsd3;
}
