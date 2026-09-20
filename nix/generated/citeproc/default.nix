{ mkDerivation, aeson, attoparsec, base, bytestring, callPackage
, case-insensitive, containers, data-default, Diff, directory
, file-embed, filepath, lib, mtl, pandoc-types, pretty, safe
, scientific, text, timeit, transformers, unicode-collation
, uniplate, vector, xml-conduit
}:
mkDerivation {
  pname = "citeproc";
  version = "0.13.0.1";
  src = callPackage ./src.nix {};
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring case-insensitive containers
    data-default file-embed filepath pandoc-types safe scientific text
    transformers unicode-collation uniplate vector xml-conduit
  ];
  testHaskellDepends = [
    aeson base bytestring containers Diff directory filepath mtl pretty
    text timeit transformers
  ];
  description = "Generates citations and bibliography from CSL styles";
  license = lib.meta.getLicenseFromSpdxId "BSD-2-Clause";
}
