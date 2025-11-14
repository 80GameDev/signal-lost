# frozen_string_literal: true

# 简单旋钮控件占位
class Knob
  attr_reader :value

  def initialize(x:, y:, radius:, value: 0.0)
    @x, @y, @radius = x, y, radius
    @value = value
  end

  def tick args
    # TODO: 根据输入更新 @value，并画出旋钮
  end
end
