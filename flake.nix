{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nix-emacs-ci.url = "github:purcell/nix-emacs-ci";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      nix-emacs-ci,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        mkEmacsEnv =
          { emacs }:
          pkgs.mkShell {
            name = "emacs-${emacs.version}";
            packages = [ emacs ] ++ [ pkgs.eask-cli ];

            shellHook = ''
              export PATH=${emacs}/bin:$PATH
              export EMACSLOADPATH="$(pwd):$EMACSLOADPATH"

              echo "--- Welcome to the Emacs ${emacs.version} development shell! ---"
              # echo "Emacs path: $(which emacs)"
              # echo "Emacs path: $(emacs --version)"
              echo -e "\n> emacs --version"
              emacs --version
              echo -e "\n> eask --version"
              eask --version
            '';
          };

      in
      {
        devShells = {
          emacs29 = mkEmacsEnv {
            emacs = nix-emacs-ci.packages.${system}.emacs-29-4;
          };
          emacs30 = mkEmacsEnv {
            emacs = nix-emacs-ci.packages.${system}.emacs-30-1;
          };
          emacsSnapshot = mkEmacsEnv {
            emacs = nix-emacs-ci.packages.${system}.emacs-snapshot;
          };
        };
      }
    );
}
