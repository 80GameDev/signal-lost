# frozen_string_literal: true

class PlayScene
  def initialize(haskell_bridge)
    @bridge = haskell_bridge
    @state  = {}
  end

  def tick args
    # TODO: 将当前输入编码成 JSON 发给 Haskell
    args.outputs.labels << [10, 700, "Play Scene (stub)", 3]
  end
end
