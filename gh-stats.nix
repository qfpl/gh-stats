{ mkDerivation, base, bytestring, containers, directory, github
, hedgehog, http-client, lens, lucid, mmorph, mtl
, optparse-applicative, servant, servant-lucid, servant-server
, sqlite-simple, sqlite-simple-errors, stdenv, sv, tasty
, tasty-hedgehog, text, time, validation, vector
}:
mkDerivation {
  pname = "gh-stats";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers github http-client lens lucid mtl
    optparse-applicative servant servant-lucid servant-server
    sqlite-simple sqlite-simple-errors sv text time validation vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers directory github hedgehog lens mmorph mtl
    sqlite-simple sqlite-simple-errors tasty tasty-hedgehog text time
    vector
  ];
  homepage = "https://qfpl.io";
  description = "Pull Github stats for an organisation and its repos";
  license = stdenv.lib.licenses.bsd3;
}
