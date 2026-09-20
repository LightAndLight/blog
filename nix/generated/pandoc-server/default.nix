{ mkDerivation, aeson, base, base64-bytestring, bytestring
, callPackage, containers, data-default, doctemplates, lib, pandoc
, pandoc-types, servant-server, skylighting, text
, unicode-collation, wai, wai-cors
}:
mkDerivation {
  pname = "pandoc-server";
  version = "0.1.3.1";
  src = callPackage ./src.nix {};
  libraryHaskellDepends = [
    aeson base base64-bytestring bytestring containers data-default
    doctemplates pandoc pandoc-types servant-server skylighting text
    unicode-collation wai wai-cors
  ];
  homepage = "https://pandoc.org";
  description = "Pandoc document conversion as an HTTP servant-server";
  license = lib.meta.getLicenseFromSpdxId "GPL-2.0-or-later";
}
