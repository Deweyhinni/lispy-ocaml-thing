{
  description = "Nix develop flake for this project";

  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url  = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };

	rustToolchainNightly = pkgs.rust-bin.selectLatestNightlyWith (toolchain: toolchain.default);
	rustToolchainStable = pkgs.rust-bin.stable.latest.default;
      in
      {
        devShells.default = pkgs.mkShell rec {
          buildInputs = [
	    rustToolchainStable
            pkgs.pkg-config
	    pkgs.openssl
          ];

          LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath buildInputs}";

          shellHook = ''
          '';
        };
      }
    );
}


