{ mkDerivation, aeson, aeson-casing, base, bytestring, containers
, hpack, http-api-data, http-client, http-client-tls, http-types
, lens, lifted-async, lifted-base, monad-logger, mtl, servant
, servant-client, servant-client-core, servant-server, stdenv, text
, transformers-base, wai, wai-cors, wai-extra, warp
}:
mkDerivation {
  pname = "letterboxd-sources-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson aeson-casing base bytestring containers http-api-data
    http-client http-client-tls http-types lens lifted-async
    lifted-base monad-logger mtl servant servant-client
    servant-client-core servant-server text transformers-base wai
    wai-cors wai-extra warp
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/letterboxd-sources-server#readme";
  license = stdenv.lib.licenses.bsd3;
}
