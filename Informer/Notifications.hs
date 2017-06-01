module Informer.Notifications
( sendNotification
)
where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Exit
import System.IO
import System.Process.Text

sendNotification :: Text -> IO ()
sendNotification text = do
    let prog = "/usr/bin/sendmail"
        args = [ prog, "johan@forberg.se" ]

    (code, out, err) <- readProcessWithExitCode prog args text

    case code of
        ExitSuccess -> return ()
        ExitFailure n -> do
            TIO.hPutStr stderr out
            TIO.hPutStr stderr err
            hPutStrLn stderr $ prog ++ " failed with code " ++ show n
                                    ++ ", continuing anyway..."

