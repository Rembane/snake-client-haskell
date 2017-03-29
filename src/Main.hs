{-# LANGUAGE DeriveGeneric, GADTs, OverloadedStrings #-}
module Main where

import Data.Aeson ((.=), ToJSON(..), Value, encode, object)
import GHC.Generics (Generic)
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import System.Info (os, compilerVersion)

data Direction = Up
               | Right
               | Down
               | Left
               deriving (Generic, Show)

instance ToJSON Direction

type GameId = String
type Tick = Int
type Name = String

data GameSettings = GameSettings
    { maxNoofPlayers                     :: Int
    , startSnakeLength                   :: Int
    , timeInMsPerTick                    :: Int
    , obstaclesEnabled                   :: Bool
    , foodEnabled                        :: Bool
    , headToTailConsumes                 :: Bool
    , tailConsumeGrows                   :: Bool
    , addFoodLikelihood                  :: Int
    , removeFoodLikelihood               :: Int
    , spontaneousGrowthEveryNWorldTick   :: Int
    , trainingGame                       :: Bool
    , pointsPerLength                    :: Int
    , pointsPerFood                      :: Int
    , pointsPerCausedDeath               :: Int
    , pointsPerNibble                    :: Int
    , noofRoundsTailProtectedAfterNibble :: Int
    } deriving (Generic, Show)

instance ToJSON GameSettings

defaultGameSettings :: GameSettings
defaultGameSettings = GameSettings
    { maxNoofPlayers                     = 5
    , startSnakeLength                   = 1
    , timeInMsPerTick                    = 250
    , obstaclesEnabled                   = True
    , foodEnabled                        = True
    , headToTailConsumes                 = True
    , tailConsumeGrows                   = False
    , addFoodLikelihood                  = 15
    , removeFoodLikelihood               = 5
    , spontaneousGrowthEveryNWorldTick   = 3
    , trainingGame                       = False
    , pointsPerLength                    = 1
    , pointsPerFood                      = 2
    , pointsPerCausedDeath               = 5
    , pointsPerNibble                    = 10
    , noofRoundsTailProtectedAfterNibble = 3
    }

data Message where
    StartGame    :: Message
    Move         :: Direction -> GameId -> Tick -> Message
    Registration :: Name -> GameSettings -> Message
    Ping         :: Message
    ClientInfo   :: Message

instance ToJSON Message where
    toJSON m =
        case m of
          StartGame -> object $
              ["type" .= ("se.cygni.snake.api.request.StartGame" :: String)]
          Move dir gid tick -> object $
              [ "type"      .= ("se.cygni.snake.api.request.RegisterMove" :: String)
              , "direction" .= dir
              , "gameId"    .= gid
              , "gameTick"  .= tick
              ]
          Registration name settings -> object $
              [ "type"         .= ("se.cygni.snake.api.request.RegisterPlayer" :: String)
              , "playerName"   .= name
              , "gameSettings" .= settings
              ]
          Ping -> object $
              ["type" .= ("se.cygni.snake.api.request.HeartBeatRequest" :: String)]
          ClientInfo -> object $
              [ "type"                   .= ("se.cygni.snake.api.request.ClientInfo" :: String)
              , "language"               .= ("Haskell" :: String)
              , "languageVersion"        .= compilerVersion
              , "operatingSystem"        .= os
              , "operatingSystemVersion" .= ("???" :: String)
              , "clientVersion"          .= ("0.0.1" :: String)
              ]

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"
    mapM_ (WS.sendTextData conn . encode) [Registration "TestBob" defaultGameSettings, ClientInfo]
    -- TODO: Start here!
    -- https://github.com/cygni/snakebot-client-clojurescript/blob/master/src/cljs_snake_bot/core.cljs

main :: IO ()
main = withSocketsDo $ WS.runClient "snake.cygni.se" 80 "/" app
