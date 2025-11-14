{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (eitherDecode, encode)

import Game.Protocol

-- | 从 stdin 读 JSON，请求 -> 响应写回 stdout
main :: IO ()
main = do
  input <- BL.getContents
  case eitherDecode input of
    Left err -> BL.putStrLn (encode (EngineResponse False ("decode error: " <> err)))
    Right (_req :: EngineRequest) ->
      -- TODO: 在这里接 GameState / Level / Signal / Narrative
      BL.putStrLn (encode (EngineResponse True "{}"))
