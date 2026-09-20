{ mkDerivation, base, callPackage, lib, text }:
mkDerivation {
  pname = "typst-symbols";
  version = "0.3";
  src = callPackage ./src.nix {};
  libraryHaskellDepends = [ base text ];
  homepage = "https://github.com/jgm/typst-symbols";
  description = "Symbol and emoji lookup for typst language";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
