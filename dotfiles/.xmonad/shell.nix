{ pkgs ? import <nixpkgs> {} }:

let
  hs = pkgs.haskellPackages.ghcWithHoogle (self: with self; [
    xmonad
    xmonad-contrib
  ]);
in
  pkgs.stdenv.mkDerivation {
    name = "haskell-shell";
    buildInputs = with pkgs.haskellPackages; [
      hs
      # apply-refact
      hasktags
      # hlint
      hspec
      intero
      stack
    ];
  }
