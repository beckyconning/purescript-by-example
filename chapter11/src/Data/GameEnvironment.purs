module Data.GameEnvironment where

type PlayerName = String

newtype GameEnvironment = GameEnvironment
  { playerName    :: PlayerName
  , debugMode     :: Boolean
  , cheatMode     :: Boolean
  }

gameEnvironment :: PlayerName -> Boolean -> Boolean -> GameEnvironment
gameEnvironment playerName debugMode cheatMode = GameEnvironment
  { playerName    : playerName
  , debugMode     : debugMode
  , cheatMode     : cheatMode
  }
