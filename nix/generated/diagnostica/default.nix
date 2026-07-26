{ mkDerivation, ansi-terminal, base, bytestring, callPackage
, containers, hspec, lib, text
}:
mkDerivation {
  pname = "diagnostica";
  version = "0.2";
  src = callPackage ./src.nix {};
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base bytestring containers text
  ];
  executableHaskellDepends = [ base bytestring ];
  testHaskellDepends = [ base bytestring hspec text ];
  description = "A library for building and rendering error diagnostics";
  license = lib.licenses.bsd3;
  mainProgram = "example";
}
