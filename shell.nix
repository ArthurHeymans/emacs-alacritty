# shell.nix - Standalone shell for building without flakes
# This works without requiring the code to be in a git repository
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    rustc
    cargo
    pkg-config
  ];

  buildInputs = with pkgs; [
    emacs
  ];

  shellHook = ''
    echo "Alacritty for Emacs build environment"
    echo "Rust version: $(rustc --version)"
  '';
}
