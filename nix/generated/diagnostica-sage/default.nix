{ mkDerivation, base, bytestring, callPackage, diagnostica, hspec
, lib, parsers, sage, sage-parsers-instances, text
}:
mkDerivation {
  pname = "diagnostica-sage";
  version = "0.1.1.1";
  src = callPackage ./src.nix {};
  libraryHaskellDepends = [ base bytestring diagnostica sage text ];
  testHaskellDepends = [
    base bytestring diagnostica hspec parsers sage
    sage-parsers-instances
  ];
  description = "diagnostica support for sage parse errors";
  license = lib.licenses.bsd3;
}
