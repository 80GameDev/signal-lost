module Game.Narrative
  ( NarrativeEvent(..)
  , checkNarrativeTriggers
  ) where

-- | 剧情事件：解锁独白 / 对话 / 画面
data NarrativeEvent
  = RadioMonologue String
  | MemoryFlashback String
  deriving (Show, Eq)

-- | 根据游戏状态判断是否触发剧情（之后和 GameState 联动）
checkNarrativeTriggers :: IO [NarrativeEvent]
checkNarrativeTriggers = pure []
