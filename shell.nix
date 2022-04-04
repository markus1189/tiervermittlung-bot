{ pkgs ? import <nixpkgs> { } }:

let
  hsPkgs = pkgs.haskellPackages.ghcWithPackages (p:
    with p; [
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
      token-bucket
      unordered-containers
    ]);
  # https://github.com/alpmestan/taggy-lens/pull/7
  taggy-lens-fork = (pkgs.haskellPackages.callCabal2nix "taggy-lens" (builtins.fetchGit {name = "taggy-lens-fork"; url = "https://github.com/markus1189/taggy-lens/"; ref = "refs/heads/lens-5"; rev = "0e4c0648cd8a3bf2112b020cd9c4c9158298329b";}) { });

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
