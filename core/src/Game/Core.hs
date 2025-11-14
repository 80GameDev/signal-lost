module Game.Core
  ( GameState(..)
  , StepResult(..)
  ) where

-- | 整体游戏状态（世界状态 + 当前关卡等）
data GameState = GameState
  { gsCurrentLevel :: Int
  , gsTime         :: Double
  } deriving (Show, Eq)

-- | 一次“步进”之后返回给前端的结果
data StepResult = StepResult
  { srGameStateJson :: String  -- ^ 序列化后的 JSON（给 DragonRuby）
  } deriving (Show, Eq)
