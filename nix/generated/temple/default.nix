{ mkDerivation, base, bytestring, callPackage, containers
, diagnostica, diagnostica-sage, filepath, hspec, hspec-discover
, lib, mtl, optparse-applicative, sage, text
}:
mkDerivation {
  pname = "temple";
  version = "0.1.0.0";
  src = callPackage ./src.nix {};
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers filepath mtl sage text
  ];
  executableHaskellDepends = [
    base bytestring containers diagnostica diagnostica-sage filepath
    mtl optparse-applicative sage text
  ];
  testHaskellDepends = [ base bytestring hspec sage ];
  testToolDepends = [ hspec-discover ];
  license = lib.licenses.gpl3Only;
  mainProgram = "temple";
}
