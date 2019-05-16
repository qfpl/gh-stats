{ mkDerivation, base, github, stdenv }:
mkDerivation {
  pname = "gh-stats";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base github ];
  executableHaskellDepends = [ base ];
  doHaddock = false;
  homepage = "https://qfpl.io";
  description = "Pull Github stats for an organisation and its repos";
  license = stdenv.lib.licenses.bsd3;
}
