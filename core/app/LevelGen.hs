{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson (encode, ToJSON)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BL

-- 简单的关卡配置结构：之后可以和 Game.Level 联动
data LevelConfig = LevelConfig
  { lcId    :: Int
  , lcName  :: String
  } deriving (Show, Eq, Generic)

instance ToJSON LevelConfig

main :: IO ()
main = do
  let levels =
        [ LevelConfig 1 "Intro Signal"
        , LevelConfig 2 "Tuning Alone"
        ]
  BL.putStrLn (encode levels)
