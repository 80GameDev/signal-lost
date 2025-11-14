# frozen_string_literal: true

# 入口：DragonRuby 主循环会从这里调用
# 这里只是占位，之后根据 DragonRuby 的约定填充 tick 方法等逻辑。

def tick args
  args.outputs.labels << [10, 30, "Signal Lost - stub main.rb"]
end
