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
            # Use official taggy-lens repo master branch which includes lens-5 support
            # PR #7 (https://github.com/alpmestan/taggy-lens/pull/7) was merged in April 2022
            # but hasn't been released to Hackage yet, so we build from git
            taggy-lens-updated = (callCabal2nix "taggy-lens" (builtins.fetchGit {
              name = "taggy-lens-lens5";
              url = "https://github.com/alpmestan/taggy-lens/";
              ref = "refs/heads/master";
              # Using a recent commit that includes lens-5 support
              rev = "87235bfb9c3ee8b3d487c1cf48a22f247e59286d";
            }) { });

            # token-bucket requires jailbreak because it has outdated upper bound time < 1.13
            # but current nixpkgs has time-1.15. The package works fine with newer time versions,
            # just the cabal constraint is outdated. Package is marked broken in nixpkgs due to this.
            # See: https://github.com/haskell-hvr/token-bucket
            token-bucket-fixed = pkgs.haskell.lib.unmarkBroken
              (pkgs.haskell.lib.doJailbreak token-bucket);
          in [
            lens-aeson
            either
            logging
            wreq
            lens
            rio
            taggy-lens-updated
            haskell-language-server
            retry
            tasty
            tasty-golden
            tasty-hedgehog
            tasty-hunit
            tasty-hspec
            temporary
            token-bucket-fixed
            unordered-containers
          ]);
        dogBotScript = pkgs.writeScriptBin "dog-bot" ''
          #!/usr/bin/env bash
          ${hsPkgs}/bin/runhaskell ${./dog-bot.hs}
        '';
      in rec {
        packages.default = dogBotScript;
        devShell = pkgs.mkShell { nativeBuildInputs = [ hsPkgs ]; };
      });
}
