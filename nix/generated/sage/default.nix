{ mkDerivation, base, bytestring, callPackage, containers, deepseq
, hedgehog, hspec, hspec-hedgehog, lib, text
}:
mkDerivation {
  pname = "sage";
  version = "0.5";
  src = callPackage ./src.nix {};
  postUnpack = "sourceRoot+=/sage; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring containers deepseq text
  ];
  testHaskellDepends = [
    base bytestring containers hedgehog hspec hspec-hedgehog text
  ];
  description = "Efficient parser combinators";
  license = lib.licenses.bsd3;
}
