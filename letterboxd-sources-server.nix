{ mkDerivation, aeson, aeson-casing, base, bytestring, hpack
, http-api-data, http-client, http-client-tls, http-types, lens
, lifted-base, monad-logger, mtl, servant, servant-client
, servant-client-core, servant-server, stdenv, text, wai, wai-cors
, wai-extra, warp
}:
mkDerivation {
  pname = "letterboxd-sources-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson aeson-casing base bytestring http-api-data http-client
    http-client-tls http-types lens lifted-base monad-logger mtl
    servant servant-client servant-client-core servant-server text wai
    wai-cors wai-extra warp
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/letterboxd-sources-server#readme";
  license = stdenv.lib.licenses.bsd3;
}
