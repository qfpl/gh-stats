{ mkDerivation, base, bytestring, github, http-client, lens, mtl
, optparse-applicative, sqlite-simple, sqlite-simple-errors, stdenv
, sv, text, time, vector
}:
mkDerivation {
  pname = "gh-stats";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring github http-client lens mtl optparse-applicative
    sqlite-simple sqlite-simple-errors sv text time vector
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://qfpl.io";
  description = "Pull Github stats for an organisation and its repos";
  license = stdenv.lib.licenses.bsd3;
}
