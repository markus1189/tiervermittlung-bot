{
  description = "Telegram bot scraping dog adoption listings from tiervermittlung.de";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellDeps = p:
          let
            # Hackage's taggy-lens carries a stale `lens < 5` upper bound, so
            # nixpkgs would try to build it against an old lens. Jailbreaking
            # drops the bound; the code itself compiles fine against lens-5.
            taggy-lens-fixed = pkgs.haskell.lib.doJailbreak p.taggy-lens;

            # token-bucket requires jailbreak because it has outdated upper bound time < 1.13
            # but current nixpkgs has time-1.15. The package works fine with newer time versions,
            # just the cabal constraint is outdated. Package is marked broken in nixpkgs due to this.
            # See: https://github.com/haskell-hvr/token-bucket
            token-bucket-fixed = pkgs.haskell.lib.unmarkBroken
              (pkgs.haskell.lib.doJailbreak p.token-bucket);
          in [
            p.lens-aeson
            p.logging
            p.wreq
            p.lens
            taggy-lens-fixed
            p.retry
            p.tasty
            p.tasty-hunit
            p.temporary
            token-bucket-fixed
          ];

        hsPkgs = pkgs.haskellPackages.ghcWithPackages haskellDeps;

        hsPkgsDev = pkgs.haskellPackages.ghcWithPackages
          (p: haskellDeps p ++ [ p.haskell-language-server ]);

        # Compile with ghc so `nix build` typechecks the bot; a plain
        # runhaskell wrapper would let type errors slip through to runtime.
        dogBot = pkgs.runCommand "dog-bot" { nativeBuildInputs = [ hsPkgs ]; } ''
          mkdir -p $out/bin
          ghc -Wall -O -outputdir "$TMPDIR/build" -o $out/bin/dog-bot ${./dog-bot.hs}
        '';
      in rec {
        packages.default = dogBot;
        devShell = pkgs.mkShell { nativeBuildInputs = [ hsPkgsDev ]; };
      });
}
