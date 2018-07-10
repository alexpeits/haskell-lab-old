module Conduits where

import Control.Monad.IO.Class

import Data.Word (Word8)
import qualified Data.Map as M

import Conduit

loudYield :: Int -> ConduitM i Int IO ()
loudYield x = do
  liftIO $ putStrLn $ "Yielding: " ++ show x
  yield x

loudSinkNull :: ConduitM Int o IO ()
loudSinkNull =
  mapM_C $ \x -> putStrLn $ "Awaited: " ++ show x

main1 =
  runConduit $ mapM_ loudYield [1..5]
            .| loudSinkNull

loudSinkNull' :: ConduitM Int o IO ()
loudSinkNull' = loop
  where loop = do
          liftIO $ putStrLn "calling await"
          mx <- await
          case mx of
            Nothing -> liftIO $ putStrLn "all done!"
            Just x  -> do
              liftIO $ putStrLn $ "received: " ++ show x
              loop

main2 =
  runConduit $ mapM_ loudYield [1..5]
            .| loudSinkNull'


source3 :: ConduitM () Int IO ()
source3 = do
  liftIO $ putStrLn "acquire some resources"
  mapM_ yield [1..10]

main3 = runConduit $ source3 .| takeC 2 .| (printC >> undefined)

source4 :: (MonadResource m) => ConduitM () Int m ()
source4 = bracketP
  (putStrLn "acquire some resources")
  (\() -> putStrLn "clean up")
  (\() -> mapM_ yield [1..10])

main4 :: IO ()
main4 = runConduitRes $
  source4 .| takeC 2 .| (printC >> undefined)

main5 :: IO ()
main5 = print
  $ runConduitPure
  $ yieldMany [1..10 :: Double]
  .| getZipSink ((,)
                 <$> ZipSink sumC
                 <*> ZipSink (fromIntegral <$> lengthC))

main6 = print
  $ runConduitPure
  $ yieldMany [1..10]
  .| foldlC (flip (:)) []

main7 = runConduit
  $ yieldMany [1..10]
  .| (foldMC f 0 >>= liftIO . print)
  where
    f total x = do
      putStr $ "Received: " ++ show x ++ ", "
      putStrLn $ "Running total: " ++ show total
      return $ total + x

sinkHistogram :: Monad m => ConduitM Int o m (M.Map Int Int)
sinkHistogram =
  foldlC go M.empty
  where go m w = M.insertWith (+) w 1 m

main8 = runConduitPure
  $ yieldMany [1, 2, 1, 1, 1, 2, 3, 2, 1, 2, 3]
  .| sinkHistogram

main9 :: IO ()
main9 = runConduit
  $ yieldMany [1..10 :: Int]
  .| do
       mapC id .| (await >>= maybe (return ()) leftover)
       printC
  .| do
       leftover "Hello"
       printC

-- README examples

main10 :: IO ()
main10 = do
  putStrLn "List version:"
  print $ takeWhile (< 18) $ map (* 2) $ take 10 [1..]
  putStrLn ""
  putStrLn "Conduit version:"
  print $ runConduitPure
    $ yieldMany [1..]
    .| takeC 10
    .| mapC (* 2)
    .| takeWhileC (< 18)
    .| sinkList

main11 :: IO ()
main11 = do
  putStrLn "List version:"
  mapM_ print $ takeWhile (< 18) $ map (* 2) $ take 10 [1..]
  putStrLn ""
  putStrLn "Conduit version:"
  runConduit
    $ yieldMany [1..]
    .| takeC 10
    .| mapC (* 2)
    .| takeWhileC (< 18)
    .| mapM_C print

magic :: Int -> IO Int
magic x = do
    putStrLn $ "I'm doing magic with " ++ show x
    return $ x * 2

main12 :: IO ()
main12 = do
    putStrLn "List version:"
    mapM magic (take 10 [1..]) >>= mapM_ print . takeWhile (< 18)
    putStrLn ""
    putStrLn "Conduit version:"
    runConduit
          $ yieldMany [1..]
         .| takeC 10
         .| mapMC magic
         .| takeWhileC (< 18)
         .| mapM_C print
