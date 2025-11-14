# frozen_string_literal: true

# 波形视图占位：用于显示来自 Haskell 信号模型的采样点
class WaveformView
  def initialize(x:, y:, w:, h:)
    @x, @y, @w, @h = x, y, w, h
    @points = []
  end

  def update_samples(samples)
    @points = samples
  end

  def tick args
    # TODO: 把 @points 画成折线
  end
end
