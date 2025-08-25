{ pkgs ? (import (builtins.fetchGit {
  name = "nixos-unstable-2022-04-05";
  url = "https://github.com/nixos/nixpkgs/";
  ref = "refs/heads/nixos-unstable";
  rev = "bc4b9eef3ce3d5a90d8693e8367c9cbfc9fc1e13";
}) { }) }:

let
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
      token-bucket-jailbreak =
        pkgs.haskell.lib.unmarkBroken (pkgs.haskell.lib.doJailbreak token-bucket);
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

in pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    hsPkgs

    bash
    cacert
    coreutils
    gnugrep
    jo
    jq
    mktemp
    pandoc
    parallel
    pup
    wget
  ];
}
