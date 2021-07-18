let
  repo = builtins.fetchGit {
    "url" = "git@github.com:jeovazero/nix-ghc901-native-bignum.git";
    "rev" = "cd691a8965cfe531335a53bb0b8140eae2ebe825";
  };

  inherit (import repo) statify flavors;
  inherit (flavors.nativeBignumWithPatch) haskellPackages pkgs;

  myHSPkgs = haskellPackages.override {
    overrides = self: super: {
      memory = self.callHackage "memory" "0.16.0" {};
      cryptonite = self.callHackage "cryptonite" "0.29" {};
    };
  };

  app = { name = "janitor"; src = ./.; };
in
  statify { haskellPackages = myHSPkgs; pkgs = pkgs;}  { app = app; }
