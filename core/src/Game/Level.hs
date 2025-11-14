module Game.Level
  ( Level(..)
  , loadLevels
  ) where

-- | 关卡定义，可以根据需要扩展
data Level = Level
  { levelId      :: Int
  , levelName    :: String
  , levelConfig  :: String   -- 先用 String 占位：稍后改成配置结构
  } deriving (Show, Eq)

-- | 从某处加载关卡（之后可以从 JSON 或文件系统加载）
loadLevels :: IO [Level]
loadLevels = pure
  [ Level 1 "Intro Signal"  "{}"
  , Level 2 "Tuning Alone" "{}"
  ]
