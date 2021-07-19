let
  repo = builtins.fetchTarball {
    name = "nix-ghc901-native-bignum";
    url = "https://github.com/jeovazero/nix-ghc901-native-bignum/archive/cd691a8965cfe531335a53bb0b8140eae2ebe825.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1apa4fsczz6hx91sms5zmsv89qdcdvmjsjn424ijad3gib30ynib";
  };

  inherit (import repo) statify flavors;
  inherit (flavors.nativeBignumWithPatch) haskellPackages pkgs;

  myHSPkgs = haskellPackages.override {
    overrides = self: super: {
      memory = self.callHackage "memory" "0.16.0" {};
      cryptonite = self.callHackage "cryptonite" "0.29" {};
    };
  };

  app = { name = "janitor"; src = ../.; };
in
  statify { haskellPackages = myHSPkgs; pkgs = pkgs;}  { app = app; }
