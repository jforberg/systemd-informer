module Informer.Notifications
( sendNotification
)
where

import Control.Monad
import System.IO
import System.Process

sendNotification :: String -> IO ()
sendNotification text = do
    let prog = "/usr/bin/sendmail"
        args = [ prog, "johan@forberg.se" ]

    output <- readProcess prog args text

    when (output /= "") $ hPutStrLn stderr output

