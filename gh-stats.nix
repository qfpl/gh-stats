{ mkDerivation, base, bytestring, github, lens, mtl
, optparse-applicative, stdenv, sv, text, time
}:
mkDerivation {
  pname = "gh-stats";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring github lens mtl optparse-applicative sv text time
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://qfpl.io";
  description = "Pull Github stats for an organisation and its repos";
  license = stdenv.lib.licenses.bsd3;
}
