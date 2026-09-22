{ mkDerivation, base, bytestring, callPackage, containers, hspec
, hspec-discover, lib, mtl, sage, text, time, transformers
}:
mkDerivation {
  pname = "tomlin";
  version = "0.1.0.0";
  src = callPackage ./src.nix {};
  libraryHaskellDepends = [
    base bytestring containers mtl sage text time transformers
  ];
  testHaskellDepends = [ base containers hspec time ];
  testToolDepends = [ hspec-discover ];
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
