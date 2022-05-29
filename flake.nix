{
  description = "A very basic flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hsPkgs = pkgs.haskellPackages.ghcWithPackages (p:
          with p;
          let
            # https://github.com/alpmestan/taggy-lens/pull/7
            taggy-lens-fork = (callCabal2nix "taggy-lens" (builtins.fetchGit {
              name = "taggy-lens-fork";
              url = "https://github.com/markus1189/taggy-lens/";
              ref = "refs/heads/lens-5";
              rev = "0e4c0648cd8a3bf2112b020cd9c4c9158298329b";
            }) { });
            token-bucket-jailbreak = pkgs.haskell.lib.unmarkBroken
              (pkgs.haskell.lib.doJailbreak token-bucket);
          in [
            lens-aeson
            either
            logging
            wreq
            lens
            rio
            taggy-lens-fork
            haskell-language-server
            retry
            tasty
            tasty-golden
            tasty-hedgehog
            tasty-hunit
            tasty-hspec
            temporary
            token-bucket-jailbreak
            unordered-containers
          ]);
        dogBotScript = pkgs.writeScriptBin "dog-bot" ''
          ${hsPkgs}/bin/runhaskell ./dog-bot.hs
        '';
      in rec {
        defaultApp = dogBotScript;
        defaultPackage = dogBotScript;
        devShell = pkgs.mkShell { nativeBuildInputs = [ hsPkgs ]; };
      });
}
