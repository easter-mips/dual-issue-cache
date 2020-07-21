{ mkDerivation, base, clash-prelude, containers, ghc-typelits-extra
, ghc-typelits-knownnat, ghc-typelits-natnormalise, lens, mtl
, stdenv
}:
mkDerivation {
  pname = "icache";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base clash-prelude containers ghc-typelits-extra
    ghc-typelits-knownnat ghc-typelits-natnormalise lens mtl
  ];
  testHaskellDepends = [
    base ghc-typelits-extra ghc-typelits-knownnat
    ghc-typelits-natnormalise
  ];
  license = stdenv.lib.licenses.bsd3;
}
