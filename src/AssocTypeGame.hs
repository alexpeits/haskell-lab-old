{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module AssocTypeGame where


type Position = (Int, Int)

class Player p where
  playerPosition :: p -> Position
  playerMoveTo :: Position -> p -> p

class Player (PlayerType g) => GameState g where
  type PlayerType g :: *
  getPlayer :: g -> PlayerType g
  getMonsterPositions :: g -> [Position]

data PlayerData = PlayerData { _pos :: Position }

instance Player PlayerData where
  playerPosition = _pos
  playerMoveTo pos player = player { _pos = pos }

data GameStateData = GameStateData PlayerData [Position]
instance GameState GameStateData where
  type PlayerType GameStateData = PlayerData
  getPlayer           (GameStateData p _) = p
  getMonsterPositions (GameStateData _ mPoses) = mPoses

checkForCollisions :: GameState s => s -> [Position] -> Bool
checkForCollisions s ps =
  let
    p    = getPlayer s
    pPos = playerPosition p
  in
  pPos `elem` ps
