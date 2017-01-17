module Informer.Util
( printerr
)
where

import System.IO

printerr = hPutStrLn stderr
