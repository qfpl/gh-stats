{ mkDerivation, base, bytestring, directory, github, hedgehog
, http-client, lens, mmorph, mtl, optparse-applicative
, sqlite-simple, sqlite-simple-errors, stdenv, sv, tasty
, tasty-hedgehog, text, time, vector
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
  testHaskellDepends = [
    base directory github hedgehog mmorph mtl sqlite-simple tasty
    tasty-hedgehog time
  ];
  homepage = "https://qfpl.io";
  description = "Pull Github stats for an organisation and its repos";
  license = stdenv.lib.licenses.bsd3;
}
