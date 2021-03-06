let
  nixpkgs = import ./nix/pinned.nix { };
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskell cabal2nix cabal-install;
  haskellPackages = haskell.packages.ghc8104;
  inherit (haskellPackages) ghcid;
  project = import ./default.nix {};
in pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    cabal-install
    cabal2nix
    ghcid
  ];
}
