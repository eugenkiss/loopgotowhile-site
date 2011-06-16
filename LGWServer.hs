{-# LANGUAGE DoAndIfThenElse #-}
import System.Timeout (timeout)
import Control.Applicative (optional)
import Control.Concurrent.MVar
import Control.Monad.Trans (liftIO)
import Control.Monad (msum)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List.Split (splitOn)

import qualified Happstack.Server as H
import Happstack.Server 
    ( Response, ServerPart, Method(POST), BodyPolicy(..)
    , decodeBody, defaultBodyPolicy 
    , dir, dirs, nullDir, anyPath
    , serveFile, asContentType 
    , nullConf, ok, simpleHTTP, toResponse, methodM, badRequest, look
    ) 
import Happstack.Server.RqData (checkRq)

import qualified Language.LoopGotoWhile.Loop.Strict     as LoopS
import qualified Language.LoopGotoWhile.Loop.Extended   as LoopE
import qualified Language.LoopGotoWhile.Loop.Transform  as LoopT
import qualified Language.LoopGotoWhile.Goto.Strict     as GotoS
import qualified Language.LoopGotoWhile.Goto.Extended   as GotoE
import qualified Language.LoopGotoWhile.Goto.Transform  as GotoT
import qualified Language.LoopGotoWhile.While.Strict    as WhileS
import qualified Language.LoopGotoWhile.While.Extended  as WhileE
import qualified Language.LoopGotoWhile.While.Transform as WhileT


main :: IO ()
main = do
    mvar <- newEmptyMVar
    simpleHTTP myConf $ handlers mvar

-- The time limit for computations in ms.
timeLimit :: Int
timeLimit = 5000

myConf :: H.Conf
myConf = H.Conf 
    { H.port      = 8000
    , H.validator = Nothing
    , H.logAccess = H.logAccess nullConf
    , H.timeout   = 10
    }

handlers :: MVar () -> ServerPart Response
handlers mvar = msum
    [ dirs "loop/strict"    $ runCode mvar LoopS.run
    , dirs "loop/extended"  $ runCode mvar LoopE.run
    , dirs "goto/strict"    $ runCode mvar GotoS.run
    , dirs "goto/extended"  $ runCode mvar GotoE.run
    , dirs "while/strict"   $ runCode mvar WhileS.run
    , dirs "while/extended" $ runCode mvar WhileE.run

    , dirs "loop/to/strict"  $ transformCode mvar $ \s ->
          LoopE.parse s >>= return . show . LoopT.toStrict 
    , dirs "loop/to/loop"    $ transformCode mvar $ \s ->
          LoopE.parse s >>= return . show 
    , dirs "loop/to/goto"    $ transformCode mvar $ \s ->
          LoopE.parse s >>= return . show . LoopT.toGoto
    , dirs "loop/to/while"   $ transformCode mvar $ \s ->
          LoopE.parse s >>= return . show . LoopT.toWhile
    , dirs "goto/to/strict"  $ transformCode mvar $ \s ->
          GotoE.parse s >>= return . show . GotoT.toStrict
    , dirs "goto/to/goto"    $ transformCode mvar $ \s ->
          GotoE.parse s >>= return . show 
    , dirs "goto/to/while"   $ transformCode mvar $ \s ->
          GotoE.parse s >>= return . show . GotoT.toWhile
    , dirs "while/to/strict" $ transformCode mvar $ \s ->
          WhileE.parse s >>= return . show . WhileT.toStrict
    , dirs "while/to/while"  $ transformCode mvar $ \s ->
          WhileE.parse s >>= return . show
    , dirs "while/to/goto"   $ transformCode mvar $ \s ->
          WhileE.parse s >>= return . show . WhileT.toGoto
    , anyPath $ dir "to" $ anyPath $ 
          badRequest $ toResponse "Impossible transformation!"

    -- Comment this section when not testing
    {-, nullDir >> serveFile (asContentType "text/html") "index.html"-}
    {-, dir "style.css" $ serveFile (asContentType "text/css") "style.css"-}
    {-, dirs "codemirror/codemirror.css" $ serveFile (asContentType "text/css") "codemirror/codemirror.css"-}
    {-, dirs "codemirror/theme.css" $ serveFile (asContentType "text/css") "codemirror/theme.css"-}
    {-, dirs "codemirror/codemirror.js" $ serveFile (asContentType "text/javascript") "codemirror/codemirror.js"-}
    {-, dirs "codemirror/loop.js" $ serveFile (asContentType "text/javascript") "codemirror/loop.js"-}
    {-, dirs "codemirror/loop-strict.js" $ serveFile (asContentType "text/javascript") "codemirror/loop-strict.js"-}
    {-, dirs "codemirror/goto.js" $ serveFile (asContentType "text/javascript") "codemirror/goto.js"-}
    {-, dirs "codemirror/goto-strict.js" $ serveFile (asContentType "text/javascript") "codemirror/goto-strict.js"-}
    {-, dirs "codemirror/while.js" $ serveFile (asContentType "text/javascript") "codemirror/while.js"-}
    {-, dirs "codemirror/while-strict.js" $ serveFile (asContentType "text/javascript") "codemirror/while-strict.js"-}

    , badRequest $ toResponse "Nothing here!"
    ]

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 100000 1000)

runCode :: MVar () -> (String -> [Integer] -> Either String Integer) -> ServerPart Response
runCode mvar runner = do 
    methodM POST
    wasFree <- liftIO $ tryPutMVar mvar ()
    if wasFree then do
       -- TODO: Instead of failing with an error response like "Request body too
       -- large" like suggested in the documentation, the source code from the
       -- form submission will just be chopped off but still read. This leads to
       -- a parser error message for the user which is highly misleading, since
       -- the user does not realise that only a part of his entered source code is
       -- decoded by the server.
       decodeBody myPolicy
       source <- look "source"
       -- TODO: This is stupid. If there is an error when parsing the
       -- arguments, instead of returning an error response like suggested in
       -- the documentation args will simply become NOTHING. If I, however,
       -- remove `optional` then this whole function behaves in very weird
       -- ways, as if a silent exception happened...
       args   <- optional $ look "args" `checkRq` parseArgs
       result <- liftIO $ timeout (1000 * timeLimit) $ do
           let s  = runner source $ fromMaybe [] args
           -- TODO: Instead of printing force strictness in another way
           print s 
           return s
       _ <- liftIO $ tryTakeMVar mvar
       case result of
         Nothing        -> badRequest $ toResponse "Computation took too long!"
         Just (Left x)  -> badRequest $ toResponse x
         Just (Right x) -> ok         $ toResponse x
    else badRequest $ toResponse "Server is busy!"

transformCode :: MVar () -> (String -> Either String String) -> ServerPart Response
transformCode mvar runner = do 
    methodM POST
    wasFree <- liftIO $ tryPutMVar mvar ()
    if wasFree then do
       decodeBody myPolicy
       source <- look "source"
       result <- liftIO $ timeout (1000 * timeLimit) $ do
           let s  = runner source
           -- TODO: Instead of printing force strictness in another way
           print s 
           return s
       _ <- liftIO $ tryTakeMVar mvar
       case result of
         Nothing        -> badRequest $ toResponse "Computation took too long!"
         Just (Left x)  -> badRequest $ toResponse x
         Just (Right x) -> ok         $ toResponse x
    else badRequest $ toResponse "Server is busy!"

parseArgs :: String -> Either String [Integer]
parseArgs "" = Right []
parseArgs s  = if any (== Nothing) result
                  then Left "Parse Error on Arguments!"
                  else Right $ map (fromMaybe 0) result
  where result = map maybeRead . splitOn "," . trimWhiteSpace $ s
        trimWhiteSpace = filter (/= ' ')

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads
