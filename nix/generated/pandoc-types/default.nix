{ mkDerivation, aeson, base, bytestring, callPackage, containers
, criterion, deepseq, HUnit, lib, QuickCheck, syb, template-haskell
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text, transformers
}:
mkDerivation {
  pname = "pandoc-types";
  version = "1.23.1.2";
  src = callPackage ./src.nix {};
  libraryHaskellDepends = [
    aeson base bytestring containers deepseq QuickCheck syb text
    transformers
  ];
  testHaskellDepends = [
    aeson base bytestring containers HUnit QuickCheck syb
    template-haskell test-framework test-framework-hunit
    test-framework-quickcheck2 text
  ];
  benchmarkHaskellDepends = [ base criterion text ];
  homepage = "https://pandoc.org/";
  description = "Types for representing a structured document";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
