# tiny-game-server

Run this application to play the [tiny-game-hs][tiny-game-hs] online.


## Usage

Start the standalone container:

```ShellSession
$ podman run -p 8000:8000 ghcr.io/TristanCacqueray/tiny-game-server
```

Or run the nix flake:

```ShellSession
$ nix run github:TristanCacqueray/tiny-game-server
```

Or build from source:

- Get the Haskell toolchain: [get-started](https://www.haskell.org/get-started/).
- Clone the [tiny-game-hs][tiny-game-hs] to `/srv/github.com/haskell-game/tiny-games-hs` (or set the `TINY_GAME_HS` environment variable to the clone location).
- Start the service with `cabal run`

Then visit http://localhost:8000


## Contribute

Contributions and bug reports are welcome!

Run ghcid with `nix develop --command	ghcid -WT main`

Build the container with:

```ShellSession
$ nix build .#container
$ podman load < result
```

[tiny-game-hs]: https://github.com/haskell-game/tiny-games-hs
