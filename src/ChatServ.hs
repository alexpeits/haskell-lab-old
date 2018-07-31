module ChatServ where

import Network
import System.IO
-- import Network.Socket

main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn (PortNumber 25000)
  (handle, host, port) <- accept sock
  talk handle
  hClose handle

talk :: Handle -> IO ()
talk h = do
  hSetBuffering h LineBuffering
  line <- hGetLine h
  putStrLn $ "Received: " ++ line
  hPutStrLn h $ "Hello, " ++ line
