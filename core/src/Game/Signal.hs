module Game.Signal
  ( Signal(..)
  , sampleSignal
  ) where

-- | 简单波形模型，占位：之后可以改成更复杂的物理模型
data Signal = Signal
  { sigFrequency :: Double
  , sigAmplitude :: Double
  , sigPhase     :: Double
  } deriving (Show, Eq)

-- | 根据时间采样信号
sampleSignal :: Signal -> Double -> Double
sampleSignal s t =
  sigAmplitude s * sin (2 * pi * sigFrequency s * t + sigPhase s)
