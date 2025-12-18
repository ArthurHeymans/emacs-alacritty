{
  description = "Alacritty Terminal for Emacs - Terminal emulator for Emacs using alacritty_terminal";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
        
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" "rust-analyzer" ];
        };
        
        nativeBuildInputs = with pkgs; [
          rustToolchain
          pkg-config
        ];
        
        buildInputs = with pkgs; [
          # For emacs module
          emacs
        ];
        
      in
      {
        devShells.default = pkgs.mkShell {
          inherit nativeBuildInputs buildInputs;
          
          shellHook = ''
            echo "Alacritty for Emacs development environment"
            echo "Rust version: $(rustc --version)"
            echo ""
            echo "Build commands:"
            echo "  cargo build --release    # Build the module"
            echo "  cargo test               # Run tests"
            echo ""
          '';
          
          RUST_SRC_PATH = "${rustToolchain}/lib/rustlib/src/rust/library";
        };
        
        packages.default = pkgs.rustPlatform.buildRustPackage {
          pname = "alacritty-emacs";
          version = "0.1.0";
          
          src = ./.;
          
          cargoLock = {
            lockFile = ./Cargo.lock;
          };
          
          nativeBuildInputs = nativeBuildInputs;
          buildInputs = buildInputs;
          
          # The output is a dynamic library
          postInstall = ''
            mkdir -p $out/share/emacs/site-lisp
            cp $src/alacritty.el $out/share/emacs/site-lisp/
            cp $out/lib/libalacritty_emacs.so $out/share/emacs/site-lisp/ || true
          '';
        };
      }
    );
}
