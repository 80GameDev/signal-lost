# app/scenes/boot_scene.rb
#
# 启动场景：Day 2 基础波形可视化
# 在 Day 1 的基础上，增加：
# - 正弦波形绘制（模拟无线电波）
# - 可调节的频率参数（用键盘 ← → 控制）
#
# 说明：
# - 频率越高，屏幕上的波形周期越密集；
# - 后续 Day 3 会把键盘控制升级为真正的“调频滑块 UI”。

class BootScene
  def tick(args)
    handle_input(args)
    render_background(args)
    render_wave(args)
    render_title(args)
    render_hint(args)
  end

  private

  # 处理与波形相关的输入（Day 2: 先用键盘控制）
  def handle_input(args)
    state = args.state

    # 波形基础参数
    state.wave_freq ||= 2.0 # 频率：决定波的“疏密”（越大越密）
    state.wave_speed ||= 0.08 # 相位前进速度：决定波滚动的速度

    kb = args.inputs.keyboard

    # 用 ← → 改变频率，模拟“调频”的感觉
    if kb.key_held.left
      state.wave_freq -= 0.02
    elsif kb.key_held.right
      state.wave_freq += 0.02
    end

    # 简单限制一下频率范围，避免过于极端
    if state.wave_freq < 0.3
      state.wave_freq = 0.3
    elsif state.wave_freq > 12.0
      state.wave_freq = 12.0
    end
  end

  def render_background(args)
    # 深蓝偏黑背景，呼应“被海水淹没的末日世界”
    args.outputs.background_color = [5, 10, 25]
  end

  # 核心：绘制正弦波
  def render_wave(args)
    state = args.state
    lines = args.outputs.lines

    # 初始化 & 推进相位，让波形在时间轴上“流动”
    state.wave_phase ||= 0.0
    state.wave_phase += state.wave_speed || 0.08

    screen_w = 1280
    center_y = 360 # 波形中线
    amplitude = 80 # 振幅（上下波动的高度）
    step_x = 4 # 每段线段的横向步长（越小越平滑但越耗性能）

    # 基础颜色：偏冷的蓝色，带一点亮度
    color = [120, 200, 255, 255] # r, g, b, a

    x = 0
    while x < screen_w
      # 根据 x、频率和相位计算正弦值
      rad1 = state.wave_phase + x * state.wave_freq * 0.01
      rad2 = state.wave_phase + (x + step_x) * state.wave_freq * 0.01

      y1 = center_y + Math.sin(rad1) * amplitude
      y2 = center_y + Math.sin(rad2) * amplitude

      # line: [x, y, x2, y2, r, g, b, a]
      lines << [x, y1, x + step_x, y2, *color]

      x += step_x
    end

    # 画一条中线，帮助玩家感受波形上下起伏
    args.outputs.lines << [0, center_y, screen_w, center_y, 40, 80, 120, 180]
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

    labels << [64, 660, "孤波 — Day 2 Wave Prototype", 3, 0, 180, 190, 210]
  end

  def render_hint(args)
    labels = args.outputs.labels

    labels << [
      64,
      620,
      "This is the Day 2 wave prototype.",
      2,
      0,
      160,
      180,
      200
    ]

    labels << [
      64,
      590,
      "← → 調整頻率 (frequency)，觀察波形變得更密 / 更疏。",
      2,
      0,
      160,
      180,
      200
    ]

    labels << [64, 560, "接下來會在這個波形基礎上加入：調頻滑塊、噪聲與關卡邏輯。", 2, 0, 160, 180, 200]

    labels << [
      64,
      520,
      "(按 ESC 退出，確認 DragonRuby 能正常運行這個 Day 2 Demo。)",
      1,
      0,
      130,
      140,
      160
    ]

    # 显示当前频率数值，方便调试
    labels << [
      64,
      480,
      "Current freq: #{args.state.wave_freq.round(2)}",
      1,
      0,
      190,
      210,
      230
    ]
  end
end
