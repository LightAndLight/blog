{ mkDerivation, base, bytestring, callPackage, containers, cookie
, hashable, hspec, hspec-discover, http-types, lib, QuickCheck
, quickcheck-instances, tagged, text, text-iso8601, time-compat
, uuid-types
}:
mkDerivation {
  pname = "http-api-data";
  version = "0.7";
  src = callPackage ./src.nix {};
  libraryHaskellDepends = [
    base bytestring containers cookie hashable http-types tagged text
    text-iso8601 time-compat uuid-types
  ];
  testHaskellDepends = [
    base bytestring containers cookie hspec QuickCheck
    quickcheck-instances text time-compat uuid-types
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://github.com/fizruk/http-api-data";
  description = "Converting to/from HTTP API data like URL pieces, headers and query parameters";
  license = lib.licenses.bsd3;
}
