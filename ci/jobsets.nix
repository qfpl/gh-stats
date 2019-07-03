{ nixpkgs, declInput }:

let
  pkgs = import nixpkgs {};
in
{
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF
    cat > $out <<EOF
    {
      "gh-stats": {
        "enabled": 1,
        "hidden": false,
        "description": "GitHub statistics for an organisation",
        "nixexprinput": "gh-stats",
        "nixexprpath": "./ci/ci.nix",
        "checkinterval": 300,
        "schedulingshares": 1,
        "enableemail": true,
        "emailoverride": "contact@qfpl.io",
        "keepnr": 5,
        "inputs": {
            "gh-stats": {
              "type": "git",
              "value": "https://github.com/qfpl/gh-stats master",
              "emailresponsible": true
            },
            "nixpkgs": {
              "type": "git",
              "value": "https://github.com/NixOS/nixpkgs.git eccb90a2d997d65dc514253b441e515d8e0241c3",
              "emailresponsible": false
            }
        }
      }
    }
    EOF
  '';
}
