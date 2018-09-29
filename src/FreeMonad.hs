{-# LANGUAGE DeriveFunctor #-}
module FreeMonad where

import Control.Monad.Free
import qualified Data.Map as M

data DSL next
  = Get String (String -> next)
  | Set String String next
  | End
  deriving Functor

get :: String -> Free DSL String
get key = liftF (Get key id)

set :: String -> String -> Free DSL ()
set key value = liftF (Set key value ())

end :: Free DSL a
end = liftF End

follow :: String -> Free DSL String
follow key = do
  key' <- get key
  get key'

m :: M.Map String String
m = M.fromList
  [ ("foo", "bar")
  , ("bar", "baz")
  , ("yeah", "sure")
  ]

run :: Free DSL a -> IO ()
run (Free (Get key nxt)) = do
  let res = m M.! key
  putStrLn res
  run $ nxt res
run (Free (Set key value nxt)) = do
  putStrLn $ key ++ " " ++ value
  run nxt
run (Free End) = putStrLn "Fin"
run (Pure _) = return ()
