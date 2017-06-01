{-# LANGUAGE OverloadedStrings #-}

module Informer.Util
( printerr
, readStdoutOrDie
, formatStrict
, showText
)
where

import Data.Text.Format
import Data.Text.Lazy (toStrict)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit
import System.IO
import System.Process.Text

printerr :: Text -> IO ()
printerr = TIO.hPutStrLn stderr

readStdoutOrDie :: FilePath -> [String] -> Text -> IO Text
readStdoutOrDie fn args input = do
    (code, out, err) <- readProcessWithExitCode fn args input

    case code of
        ExitSuccess -> return out
        ExitFailure n -> do
            TIO.hPutStr stderr out
            TIO.hPutStr stderr err
            TIO.hPutStrLn stderr ""
            error $ fn ++ " " ++ show args ++ " failed with code " ++ show n

formatStrict f as = toStrict $ format f as

showText :: Show a => a -> Text
showText = T.pack . show
