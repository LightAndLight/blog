{ mkDerivation, base, blog-lib, bytestring, crypton-connection
, crypton-x509-store, diagnostica, diagnostica-sage, directory
, exceptions, filepath, http-api-data, http-client, http-client-tls
, http-types, lib, optparse-applicative, process, sage, text, time
, tls, tomlin, transformers, unix
}:
mkDerivation {
  pname = "blog-cli";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blog-lib bytestring crypton-connection crypton-x509-store
    diagnostica diagnostica-sage directory exceptions filepath
    http-api-data http-client http-client-tls http-types
    optparse-applicative process sage text time tls tomlin transformers
    unix
  ];
  license = lib.meta.getLicenseFromSpdxId "GPL-3.0-only";
  mainProgram = "blog";
}
