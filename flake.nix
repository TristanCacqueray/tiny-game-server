{
  nixConfig.bash-prompt = "[nix(butler)] ";
  inputs = {
    # hspkgs provides a base package set.
    hspkgs.url = "github:podenv/hspkgs/0062866e8a9c427964d69b3d38721f6b42d10534"
      # "path:///srv/github.com/podenv/hspkgs";
    ;
    # fetch the tiny-game-hs repository
    tiny-games-hs.url =
      "github:haskell-game/tiny-games-hs/5ef87a55770a6398cfa6657455bc2a3ed32c552f";
    tiny-games-hs.flake = false;
  };
  outputs = { self, hspkgs, tiny-games-hs }:
    let
      pkgs = hspkgs.pkgs;

      # Add the local package to the set.
      haskellExtend = hpFinal: hpPrev: {
        tiny-game-server = hpPrev.callCabal2nix "tiny-game-server" self { };
        # And also patch the posix-pty to close all the FDs.
        posix-pty = let
          src = pkgs.fetchFromGitHub {
            owner = "TristanCacqueray";
            repo = "posix-pty";
            rev = "a5dff4cf9ab47ebd1d7c754fc6ed4d5367653779";
            sha256 = "sha256-jbuJaeD7JpYK6nevyxGUq4FYqTmA6qgfloIvfNW3YY4=";
          };
        in pkgs.haskell.lib.dontCheck
        (hpPrev.callCabal2nix "posix-pty" src { });
      };
      hsPkgs = pkgs.hspkgs.extend haskellExtend;

      # The tiny-game-server executable.
      pkg-exe = pkgs.haskell.lib.justStaticExecutables hsPkgs.tiny-game-server;
      run-script = pkgs.writers.writeBash "tiny-game-server.sh" ''
        export PATH=${pkgs.coreutils}/bin/:${ghc}/bin
        export TINY_GAME_HS=${tiny-games-hs}
        export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
        export LC_ALL=en_US.UTF-8
        exec ${pkg-exe}/bin/tiny-game-server
      '';

      # The necessary tools to build/test the project.
      baseTools = with pkgs; [ cabal-install hlint fourmolu hsPkgs.doctest ];

      gameDeps = p: [ p.rio p.random p.ansi-terminal-game p.random-shuffle ];
      ghc = pkgs.haskellPackages.ghcWithPackages gameDeps;

    in {
      packages."x86_64-linux".default = pkg-exe;

      apps."x86_64-linux".default = {
        type = "app";
        program = builtins.toString run-script;
      };

      packages."x86_64-linux".container = pkgs.dockerTools.buildLayeredImage {
        name = "ghcr.io/TristanCacqueray/tiny-game-server";
        contents = [ ghc ];
        extraCommands = "mkdir -p -m 1777 tmp";
        tag = "latest";
        created = "now";
        config.Entrypoint = [ (builtins.toString run-script) ];
        config.Labels = {
          "org.opencontainers.image.source" =
            "https://github.com/TristanCacqueray/tiny-game-server";
        };
      };

      devShells."x86_64-linux".ci = hsPkgs.shellFor {
        packages = p: [ p.tiny-game-server ];
        buildInputs = baseTools;
      };

      devShell."x86_64-linux" = hsPkgs.shellFor {
        packages = p:
          [
            (pkgs.haskell.lib.addBuildDepends p.tiny-game-server (gameDeps p))
          ];
        buildInputs = with pkgs; [ ghcid haskell-language-server ] ++ baseTools;
      };
    };
}
