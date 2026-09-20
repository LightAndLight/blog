{ mkDerivation, base, callPackage, commonmark
, commonmark-extensions, lib, pandoc-types, text
}:
mkDerivation {
  pname = "commonmark-pandoc";
  version = "0.3";
  src = callPackage ./src.nix {};
  libraryHaskellDepends = [
    base commonmark commonmark-extensions pandoc-types text
  ];
  homepage = "https://github.com/jgm/commonmark-hs";
  description = "Bridge between commonmark and pandoc AST";
  license = lib.licenses.bsd3;
}
