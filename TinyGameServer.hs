-- Quality of life syntax extension
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
-- QuasiQuotes is needed for string-qq to embed the javascript
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

-- rio to enhance the base Prelude
import RIO
import RIO.ByteString qualified as BS
import RIO.FilePath ((</>))
import RIO.Text (pack, unpack)
import System.Environment (lookupEnv)

-- string-qq for multiline
import Data.String.QQ (s)

-- ki for structured concurrency
import Ki

-- lucid2 to write html
import Lucid

-- wai/warp/websocket for network
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, pathInfo, responseFile, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsApp)
import Network.WebSockets as WS

-- posix-pty to spawn runhaskell process
import System.Posix.Pty qualified as Pty

-- process to cleanup leftover
import System.Process (cleanupProcess)

-- xstatic for javascript/css package
import Lucid.XStatic (xstaticScripts)
import XStatic (XStaticFile, xstaticMiddleware)
import XStatic.Sakura (sakuraCss)
import XStatic.Xterm (xterm)

data Game = Game
    { author :: Text
    , icon :: Text
    , dim :: (Int, Int)
    , args :: [String]
    }

allGames :: [(Text, [(Text, Game)])]
allGames =
    [
        ( "prelude"
        ,
            [ ("guess1", Game "sm" ".png" m [])
            , ("pure-doors", Game "tristanC" ".png" m [])
            , ("fifteen", Game "bradrn" ".png" m ["-XPatternSynonyms", "-XLambdaCase"])
            , ("chess", Game "fizruk" ".gif" m [])
            , ("sudoku", Game "elderephemera" ".png" m [])
            , ("matchmaking", Game "migmit" ".gif" m ["-cpp", "-DD=a=replicate;b=putStrLn;c=length;p=map;u=max(2)"])
            , ("tiny-brot", Game "tristanC" ".gif" l [])
            , ("mini-othello", Game "hellwolf" "-1.gif" m [])
            , ("one-dot", Game "OsePedro" ".png" m [])
            , ("expressit", Game "Greg8128" ".png" m [])
            , ("life", Game "Rens van Hienen" ".png" l [])
            ]
        )
    ,
        ( "base"
        ,
            [ ("timing", Game "TravisCardwell" ".png" m [])
            , ("shoot", Game "migmit" ".png" m ["-XLambdaCase"])
            , ("log2048", Game "Lysxia" ".gif" m [])
            , ("rhythm", Game "elderephemera" ".gif" m [])
            , ("peyton-says", Game "gergoerdi" ".png" m [])
            , ("acey-deucey", Game "trevarj" ".png" m [])
            , ("flower-seeds", Game "tristanC" ".png" l [])
            ]
        )
    ,
        ( "default"
        ,
            [ ("type-and-furious", Game "lsmor" ".png" m [])
            , ("shmupemup", Game "elderephemera" ".png" m [])
            , ("tsp", Game "tristanC" ".gif" m [])
            ]
        )
    ,
        ( "hackage"
        ,
            [ ("guess2", Game "sm" ".png" m [])
            , ("wordle", Game "halogenandtoast" ".png" m [])
            , ("ski", Game "sm" ".png" m [])
            , ("guesscolor", Game "TravisCardwell" ".png" z [])
            , ("bulls-n-cows", Game "akadude" ".png" m [])
            , ("hallway-to-hell", Game "juliendehos" ".gif" m [])
            , ("1234-hero", Game "gelisam" ".png" z [])
            , ("crappy-flappy", Game "gergoerdi" ".png" m [])
            , ("pong", Game "gergoerdi" ".png" m [])
            , ("minesweeper", Game "Greg8128" ".png" m [])
            , ("pong2", Game "sm" ".png" m [])
            , ("brickbreaker", Game "fgaz" ".png" z [])
            , ("lazy-march", Game "tristanC" ".gif" m [])
            , ("balances", Game "sm" ".png" z [])
            , ("vaders", Game "gergoerdi" ".png" z [])
            , ("tetris", Game "gergoerdi" ".png" m [])
            ]
        )
    ]
  where
    z = (0, 0)
    m = (80, 23)
    l = (120, 50)

serve :: WS.Connection -> Pty.Pty -> IO ()
serve conn pty = WS.withPingThread conn 30 (pure ()) $ scoped $ \scope -> do
    -- Thread to read terminal input from client
    scope `Ki.fork_` forever do
        buf <- WS.receiveData @ByteString conn
        Pty.writePty pty buf

    -- Thread to forward terminal output to client
    forever do
        Pty.threadWaitReadPty pty
        outputData <- Pty.readPty pty
        WS.sendBinaryData conn outputData

app :: FilePath -> Application
app gamesDir = xstaticMiddleware xfiles webApp
  where
    wsApp :: Text -> Text -> Game -> ServerApp
    wsApp cat name game pending_conn = do
        conn <- acceptRequest pending_conn
        let fp = gamePath cat name ".hs" gamesDir
        let args = game.args <> ["-i" <> gamesDir </> unpack cat </> unpack name, fp]
        (pty, phandle) <- Pty.spawnWithPty Nothing True "runghc" args game.dim
        serve conn pty `finally` do
            Pty.closePty pty
            cleanupProcess (Nothing, Nothing, Nothing, phandle)

    webApp :: Application
    webApp req respond =
        let respondHtml = respond . responseLBS status200 [] . renderBS . withBody
            respond404 = respond $ responseLBS status404 [] mempty
            lookupGame cat name = lookup cat allGames >>= lookup name
         in case Network.Wai.pathInfo req of
                [] -> respondHtml welcome
                ["favicon.ico"] -> respond $ responseLBS status200 [("content-type", "image/svg+xml")] logo
                [cat, name] -> case lookupGame cat name of
                    Just game -> respondHtml . gamePage cat name game =<< gameInfo cat name =<< gameSrcs
                    Nothing -> respond404
                [cat, name, p] -> case lookupGame cat name of
                    Just game
                        | p == "ws" -> case websocketsApp defaultConnectionOptions (wsApp cat name game) req of
                            Just resp -> respond resp
                            Nothing -> error "Bad WS?!"
                        | p == name <> game.icon -> do
                            fp <- gamePath cat name game.icon <$> gameSrcs
                            respond $ responseFile status200 [] fp Nothing
                    _ -> respond404
                _ -> respond404

xfiles :: [XStaticFile]
xfiles = sakuraCss : xterm

withBody :: Html () -> Html ()
withBody body = do
    doctypehtml_ do
        head_ do
            title_ "TinyGameServer"
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            xstaticScripts xfiles
        body_ [style_ "max-width: 80%"] do
            body

welcome :: Html ()
welcome = do
    pre_ do
        "                 ___         __\n"
        "|__| _  _|  _||   | . _     / _  _  _  _    | _  _    /|\n"
        "|  |(_|_)|((-||   | || )\\/  \\__)(_||||(-  __)(_||||    |\n"
    p_ do
        "This page lets you play the terminal based "
        a_ [href_ "https://github.com/haskell-game/tiny-games-hs", target_ "_blank"] "github.com/haskell-game/tiny-game-hs"
        "."
    forM_ allGames \(cat, games) -> do
        h2_ (toHtml cat)
        div_ [style_ gridStyle] do
            forM_ games \(name, game) -> do
                let root = "/" <> cat <> "/" <> name
                div_ do
                    div_ do
                        a_ [href_ root] (toHtml name)
                        br_ []
                        "(" <> toHtml (game.author) <> ")"
                    a_ [href_ root] do
                        img_ [src_ (root <> "/" <> name <> game.icon), style_ "height: 140px; width: 140px"]
        hr_ []

    p_ do
        "The source of this tiny-game-server are available at: "
        a_ [href_ "https://github.com/TristanCacqueray/tiny-game-server", target_ "_blank"] do
            "https://github.com/TristanCacqueray/tiny-game-server"
  where
    gridStyle = "display: grid; place-items: center; grid-template-columns: repeat(auto-fit, minmax(140px, 1fr)); grid-gap: 42px"

gamePage :: Text -> Text -> Game -> Html () -> Html ()
gamePage cat name game info = do
    let root = "/" <> cat <> "/" <> name
        img = root <> "/" <> name <> game.icon
        url = "https://github.com/haskell-game/tiny-games-hs/tree/main/" <> cat <> "/" <> name <> "#readme"
        start
            | game.dim == (0, 0) = "window.location.href = '" <> url <> "'"
            | otherwise = "startTerminal" <> pack (show game.dim)
    div_ [style_ "display: grid; place-items: center"] do
        h1_ (toHtml (name <> "-10-80"))
        img_ [id_ "icon", src_ img, style_ "cursor: pointer", onclick_ start]
        div_ [id_ "terminal"] mempty
        info
    script_ client

client :: Text
client =
    [s|
function startTerminal(cols, rows) {
    // Start terminal
    document.getElementById("icon").remove()
    var term = new Terminal();
    term.open(document.getElementById("terminal"));

    // Connect to server
    const socket = new WebSocket("ws://" + window.location.host + window.location.pathname + "/ws");
    socket.binaryType = 'arraybuffer';

    // Handle i/o
    term.onData(data => {
      if (socket.readyState != 1) {
        term.write(".")
        setTimeout(() => location.reload(), 5000)
      }
      socket.send(data)
    });
    socket.addEventListener('message', (event) => {
        const buf = new Uint8Array(event.data)
        // console.log("Output from server", buf);
        term.write(buf)
    });
    socket.addEventListener('close', () => term.write("\r\nEOL"))

    // Fix size
    term.resize(cols, rows)
    // somehow the viewport width doesn't match the screen, the next expression fix that.
    term._core._viewportElement.style.width = term._core.screenElement.style.width
    term.focus()
}
|]

logo :: LByteString
logo =
    [s|
<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="17cm" height="12cm" viewBox="0 0 170 120" version="1.1">
  <g style="fill: rgb(102,102,102);">
    <path d="M 0 120 L 40 60 L 0 0 L 30 0 L 70 60 L 30 120"/>
    <path d="M 136.666667 85 L 123.333333 65 L 170 65 L 170 85"/>
    <path d="M 116.666667 55 L 103.333333 35 L 170 35 L 170 55"/>
  </g>
  <g style="fill: rgb(153,153,153);">
    <path d="M 40 120 L 80 60 L 40 0 L 70 0 L 150 120 L 120 120 L 95 82.5 L 70 120"/>
  </g>
</svg>
|]

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    gamesDir <- gameSrcs
    putStrLn ("Serving :8000 " <> gamesDir)
    run 8000 (app gamesDir)

gameInfo :: Text -> Text -> FilePath -> IO (Html ())
gameInfo cat name root = do
    let readmeFile
            | name == "one-dot" = "README.adoc"
            | otherwise = "README.md"
        base = root </> unpack cat </> unpack name
    readme <-
        fromRight "" <$> try @_ @SomeException (BS.readFile (base </> readmeFile))
    src <- BS.readFile (base </> unpack name <> ".hs")
    pure do
        when (readme /= mempty) do
            pre_ [style_ "white-space: pre-line"] (toHtml readme)
            hr_ [style_ "width: 80%"]
        pre_ do
            toHtml src

gamePath :: Text -> Text -> Text -> FilePath -> FilePath
gamePath cat name ext root =
    root </> unpack cat </> unpack name </> unpack name <> unpack ext

gameSrcs :: IO FilePath
gameSrcs = fromMaybe "/srv/github.com/haskell-game/tiny-games-hs" <$> lookupEnv "TINY_GAME_HS"
