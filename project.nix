{ mkDerivation, base, clash-prelude, containers, ghc-typelits-extra
, ghc-typelits-knownnat, ghc-typelits-natnormalise, hedgehog, lens
, mtl, stdenv, tasty, tasty-expected-failure, tasty-hedgehog
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
    ghc-typelits-natnormalise hedgehog tasty tasty-expected-failure
    tasty-hedgehog
  ];
  license = stdenv.lib.licenses.bsd3;
}
