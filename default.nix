{ mkDerivation, base, bytestring, case-insensitive, data-default
, http-client, http-client-tls, http-types, stdenv, text, unliftio
}:
mkDerivation {
  pname = "wonky";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring case-insensitive data-default http-client
    http-client-tls http-types text unliftio
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
