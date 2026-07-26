{ mkDerivation, base, bytestring, callPackage, containers, hspec
, hspec-discover, lib, mtl, sage, text
}:
mkDerivation {
  pname = "tomlin";
  version = "0.1.0.0";
  src = callPackage ./src.nix {};
  libraryHaskellDepends = [
    base bytestring containers mtl sage text
  ];
  testHaskellDepends = [ base containers hspec ];
  testToolDepends = [ hspec-discover ];
  license = lib.licenses.mit;
}
