{-# LANGUAGE DeriveGeneric #-}
module Game.Protocol
  ( EngineRequest(..)
  , EngineResponse(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson   (FromJSON, ToJSON)

-- | DragonRuby -> Haskell 的请求
data EngineRequest
  = ReqInit
  | ReqStep
  | ReqLoadLevel Int
  deriving (Show, Eq, Generic)

instance FromJSON EngineRequest

-- | Haskell -> DragonRuby 的响应
data EngineResponse = EngineResponse
  { erOk    :: Bool
  , erState :: String  -- 序列化后的游戏状态 JSON
  } deriving (Show, Eq, Generic)

instance ToJSON EngineResponse
