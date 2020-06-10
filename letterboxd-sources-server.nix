{ mkDerivation, aeson, base, bytestring, hpack, http-client
, http-client-tls, mtl, servant, servant-client, servant-server
, stdenv, text, wai-extra, warp
}:
mkDerivation {
  pname = "letterboxd-sources-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring http-client http-client-tls mtl servant
    servant-client servant-server text wai-extra warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring http-client http-client-tls mtl servant
    servant-client servant-server text wai-extra warp
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/letterboxd-sources-server#readme";
  license = stdenv.lib.licenses.bsd3;
}
