{ mkDerivation, base, callPackage, lib, parsers, sage }:
mkDerivation {
  pname = "sage-parsers-instances";
  version = "0.1.0.1";
  src = callPackage ./src.nix {};
  postUnpack = "sourceRoot+=/sage-parsers-instances; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base parsers sage ];
  description = "Orphan `parsers` instances for `sage`";
  license = lib.licenses.bsd3;
}
