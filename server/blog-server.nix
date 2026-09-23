{ mkDerivation, aeson, argon2, async, barbies, base
, base16-bytestring, blog-lib, bytestring, containers, cookie
, cryptohash-sha256, crypton-connection, crypton-x509-store
, diagnostica, diagnostica-sage, directory, exceptions, filepath
, hedgehog, hspec, hspec-hedgehog, http-api-data, http-client
, http-client-tls, http-types, lib, mmorph, mtl
, optparse-applicative, pandoc, pandoc-types, process, sage, stm
, tar, temple, temporary, text, text-short, time, tls, tomlin
, transformers, wai, warp, warp-tls
}:
mkDerivation {
  pname = "blog-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base16-bytestring blog-lib bytestring containers
    cryptohash-sha256 diagnostica diagnostica-sage directory exceptions
    filepath mmorph mtl pandoc pandoc-types sage tar temple text time
    tomlin transformers
  ];
  executableHaskellDepends = [
    argon2 base blog-lib bytestring containers cookie directory
    exceptions filepath http-api-data http-types mtl
    optparse-applicative pandoc pandoc-types sage stm temple text
    text-short time tomlin transformers wai warp warp-tls
  ];
  testHaskellDepends = [
    async barbies base blog-lib bytestring containers
    crypton-connection crypton-x509-store directory filepath hedgehog
    hspec hspec-hedgehog http-api-data http-client http-client-tls
    http-types mmorph mtl process temporary text tls
  ];
  license = lib.meta.getLicenseFromSpdxId "GPL-3.0-only";
  mainProgram = "blog-server";
}
