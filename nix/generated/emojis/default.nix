{ mkDerivation, base, callPackage, containers, HUnit, lib, text }:
mkDerivation {
  pname = "emojis";
  version = "0.1.5";
  src = callPackage ./src.nix {};
  libraryHaskellDepends = [ base containers text ];
  testHaskellDepends = [ base HUnit text ];
  homepage = "https://github.com/jgm/emojis#readme";
  description = "Conversion between emoji characters and their names";
  license = lib.licenses.bsd3;
}
