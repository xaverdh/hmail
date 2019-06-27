{ pkgs ? import <nixpkgs> {}
, haskellPackages ? pkgs.haskellPackages
, ref ? "master"
, fromCwd ? false
}:
let
  callCabal2nix = haskellPackages.callCabal2nix;

  fromXaverDH = attrs: pkgs.fetchFromGitLab
    (attrs // { owner = "xaverdh"; });

  dtypes = callCabal2nix "dtypes" (fromXaverDH {
    repo = "dtypes";
    rev = "5f65ab2b87c4497bcf623b9911e9fd5a2455a324";
    sha256 = "1cg0qii24k9j8vci1vmdwd18da7cvil9bfabpic4bgv0bbw24lk2";
  }) {};
  dtypes-extra = callCabal2nix "dtypes-extra" (fromXaverDH {
    repo = "dtypes-extra";
    rev = "93b80b01d6a6a02c1fd3dc585cb554d8f29d944f";
    sha256 = "0hxwb8pd3ymcmz9zdpd394myp9712sy7x9678mh2bdbs9bhnfnq4";
  }) { inherit dtypes; };
  HaskellNet = callCabal2nix "HaskellNet" (fromXaverDH {
    repo = "HaskellNet";
    rev = "patchwork";
    sha256 = "0d03kbagmva5pwi7b9lzv5vbv72nvj9vvbad4yiszr6q153n45y0";
  }) {};
  hmail = callCabal2nix "hmail"
    (if fromCwd then ./. else builtins.fetchGit {
      url = ./.;
      inherit ref;
      }) {
    inherit dtypes dtypes-extra;
  };
in pkgs.haskell.lib.appendConfigureFlags
  ( hmail.override { inherit HaskellNet; } )
  [ "--ghc-option=-threaded"
    "--ghc-option=-O2"
    "--ghc-option=-Wincomplete-patterns" ]
