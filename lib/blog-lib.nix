{ mkDerivation, argon2, base, bytestring, containers, diagnostica
, diagnostica-sage, entropy, lib, mtl, sage, temple, text
, text-short, tomlin
}:
mkDerivation {
  pname = "blog-lib";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    argon2 base bytestring containers diagnostica diagnostica-sage
    entropy mtl sage temple text text-short tomlin
  ];
  license = lib.meta.getLicenseFromSpdxId "GPL-3.0-only";
}
