{ mkDerivation, base, bound, lens, mtl, prelude-extras, profunctors
, QuickCheck, semigroupoids, semigroups, singletons, stdenv, tagged
, validation
}:
mkDerivation {
  pname = "arith";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bound lens mtl prelude-extras profunctors QuickCheck
    semigroupoids semigroups singletons tagged validation
  ];
  license = stdenv.lib.licenses.bsd3;
}
