# app/scenes/boot_scene.rb
#
# 启动场景：Day 1 简单画面
# 只负责显示游戏标题和简单背景，用来测试项目结构是否正常。

class BootScene
  def tick(args)
    render_background(args)
    render_title(args)
    render_hint(args)
  end

  private

  def render_background(args)
    # 深蓝偏黑背景，呼应“被海水淹没的末日世界”
    args.outputs.background_color = [5, 10, 25]
  end

  def render_title(args)
    labels = args.outputs.labels

    # 主标题：英文 + 中文副标题
    labels << [
      60,
      700,
      "Signal Lost",
      6,
      0, # size_enum, alignment_enum
      230,
      240,
      255
    ]

    labels << [64, 660, "孤波 — Day 1 Boot Scene", 3, 0, 180, 190, 210]
  end

  def render_hint(args)
    labels = args.outputs.labels

    labels << [64, 620, "This is the Day 1 skeleton.", 2, 0, 160, 180, 200]

    labels << [64, 590, "接下来将逐步加入：波形可视化 + 调频滑块 + 音频噪声。", 2, 0, 160, 180, 200]

    labels << [
      64,
      540,
      "(按 ESC 退出，确认 DragonRuby 能正常运行这个项目。)",
      1,
      0,
      130,
      140,
      160
    ]
  end
end
