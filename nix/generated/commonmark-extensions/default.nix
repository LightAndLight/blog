{ mkDerivation, base, callPackage, commonmark, containers, emojis
, filepath, lib, network-uri, parsec, tasty, tasty-bench
, tasty-hunit, text, transformers
}:
mkDerivation {
  pname = "commonmark-extensions";
  version = "0.2.7.1";
  src = callPackage ./src.nix {};
  libraryHaskellDepends = [
    base commonmark containers emojis filepath network-uri parsec text
    transformers
  ];
  testHaskellDepends = [
    base commonmark parsec tasty tasty-hunit text
  ];
  benchmarkHaskellDepends = [ base commonmark tasty-bench text ];
  homepage = "https://github.com/jgm/commonmark-hs";
  description = "Pure Haskell commonmark parser";
  license = lib.licenses.bsd3;
}
