{ mkDerivation, base, bytestring, callPackage, containers
, directory, doclayout, filepath, lib, mtl, tasty, tasty-bench
, tasty-hunit, tasty-quickcheck, template-haskell, text
}:
mkDerivation {
  pname = "djot";
  version = "0.1.4.1";
  src = callPackage ./src.nix {};
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers doclayout mtl template-haskell text
  ];
  executableHaskellDepends = [ base bytestring doclayout text ];
  testHaskellDepends = [
    base bytestring directory doclayout filepath tasty tasty-hunit
    tasty-quickcheck text
  ];
  benchmarkHaskellDepends = [
    base bytestring directory doclayout filepath tasty-bench
  ];
  description = "Parser and renderer for djot light markup syntax";
  license = lib.meta.getLicenseFromSpdxId "MIT";
  mainProgram = "djoths";
}
