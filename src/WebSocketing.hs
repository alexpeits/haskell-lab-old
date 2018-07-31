{-# LANGUAGE OverloadedStrings #-}
module WebSocketing where

import           Control.Concurrent     (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import           Control.Exception      (finally)
import           Control.Monad          (forM_, forever)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char              (isPunctuation, isSpace)
import           Data.Monoid            ((<>), mappend)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T

import qualified Network.WebSockets     as WS


type Client = (Text, WS.Connection)
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient = (:)

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

broadcastFrom :: Client -> Text -> ServerState -> IO ()
broadcastFrom client message clients = do
  T.putStrLn message
  forM_ (removeClient client clients) $ \(_, conn) -> WS.sendTextData conn message

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" 25000 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg <- WS.receiveData conn
  clients <- liftIO $ readMVar state
  let
    prefix     = "Hi! I am "
    client     = (T.drop (T.length prefix) msg, conn)
    disconnect = do
      s <- modifyMVar state $ \s ->
        let s' = removeClient client s in return (s', s')
      broadcast (fst client <> " disconnected") s
  case msg of
    _ | not (prefix `T.isPrefixOf` msg) ->
        WS.sendTextData conn ("Wrong announcement" :: Text)
      | clientExists client clients ->
        WS.sendTextData conn ("User already exists" :: Text)
      | otherwise -> flip finally disconnect $ do
          liftIO $ modifyMVar_ state $ \s -> do
            let s' = addClient client s
            WS.sendTextData conn $
              "Welcome! Users: " <>
              T.intercalate ", " (map fst s)
            broadcast (fst client <> " joined") s'
            return s'
          talk conn state client

talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state (user, _) = forever $ do
  msg <- WS.receiveData conn
  liftIO $ readMVar state >>= broadcast (user <> ": " <> msg)
