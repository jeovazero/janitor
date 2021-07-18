let
  nixpkgs = import <nixpkgs> {};
  inherit (nixpkgs.pkgs) stdenv gnutar upx;
  janitor = import ./static.nix;
in 
stdenv.mkDerivation {
    name = "artifact";
    buildInputs = [ gnutar upx ];
    src = ./.;
    installPhase = ''
      mkdir $out
      cp ${janitor}/bin/janitor $out
      cd $out
      chmod 755 janitor
      upx janitor
      chmod 555 janitor
      tar -czf janitor.tar.gz janitor
    '';
}
