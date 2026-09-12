{ mkDerivation, base, bytestring, callPackage, deepseq, lib
, QuickCheck, tasty, tasty-hunit, tasty-quickcheck, text-short
}:
mkDerivation {
  pname = "argon2";
  version = "1.3.0.1";
  src = callPackage ./src.nix {};
  libraryHaskellDepends = [ base bytestring deepseq text-short ];
  testHaskellDepends = [
    base bytestring QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  description = "Memory-hard password hash and proof-of-work function";
  license = lib.licenses.bsd3;
}
