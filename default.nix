{ mkDerivation, acid-state, aeson, base, cassava, cereal
, clientsession, containers, either, gsuite-admin-sendmail
, http-client, http-client-tls, lens, memory
, natural-transformation, protolude, random, safecopy, servant
, servant-client, servant-server, stdenv, text, time, vector, wai
, warp
}:
mkDerivation {
  pname = "money-sync-service";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    acid-state aeson base cassava cereal clientsession containers
    either gsuite-admin-sendmail http-client http-client-tls lens
    memory natural-transformation protolude random safecopy servant
    servant-client servant-server text time vector wai warp
  ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
