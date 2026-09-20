{ mkDerivation, base, bytestring, callPackage, containers
, directory, filepath, lib, mtl, pandoc-types, parsec, pretty-show
, split, syb, tagged, tasty, tasty-golden, text, typst-symbols, xml
}:
mkDerivation {
  pname = "texmath";
  version = "0.13.2.2";
  src = callPackage ./src.nix {};
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers mtl pandoc-types parsec split syb text
    typst-symbols xml
  ];
  testHaskellDepends = [
    base bytestring directory filepath pretty-show tagged tasty
    tasty-golden text xml
  ];
  homepage = "http://github.com/jgm/texmath";
  description = "Conversion between math formats";
  license = lib.licenses.gpl2Only;
}
