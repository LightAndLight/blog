{ mkDerivation, aeson, array, base, bytestring, callPackage
, cassava, containers, directory, erf, filepath, lib, mtl
, ordered-containers, parsec, pretty, pretty-show, regex-tdfa
, scientific, tasty, tasty-golden, text, time, toml-parser
, typst-symbols, vector, xml-conduit, yaml
}:
mkDerivation {
  pname = "typst";
  version = "0.11.0.1";
  src = callPackage ./src.nix {};
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base bytestring cassava containers directory erf
    filepath mtl ordered-containers parsec pretty regex-tdfa scientific
    text time toml-parser typst-symbols vector xml-conduit yaml
  ];
  testHaskellDepends = [
    base bytestring directory filepath pretty-show tasty tasty-golden
    text time
  ];
  description = "Parsing and evaluating typst syntax";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
