# app/scenes/boot_scene.rb
#
# 启动场景：Day 2–3 基础波形 + 调频滑块 UI
#
# Day 2:
#   - 使用 args.outputs.lines 绘制一条正弦波，模拟无线电波形；
#   - 通过键盘 ← → 调整频率。
# Day 3:
#   - 增加一个「调频滑块」(frequency slider)，点击滑块即可改变目标频率；
#   - 滑块和内部的 wave_freq / frequency_target 状态保持同步。

class BootScene
  def tick(args)
    defaults(args)
    handle_input(args)
    update_derived_state(args)
    render(args)
  end

  # ---- 状态初始化 --------------------------------------------------------

  def defaults(args)
    s = args.state

    # 频率范围：可以根据手感再调
    s.min_freq ||= 1.0
    s.max_freq ||= 8.0

    # 当前波形频率（核心物理参数）
    s.wave_freq ||= 3.0

    # Day 3: 目标频率变量（供后面其它场景使用）
    s.frequency_target ||= s.wave_freq

    # 滑块 UI 的矩形区域
    s.slider_rect ||= { x: 200, y: 80, w: 880, h: 24 }

    # 滑块的归一化值（0.0 ~ 1.0），表示在 min_freq~max_freq 之间的位置
    if s.slider_value.nil?
      range = (s.max_freq - s.min_freq)
      range = 1.0 if range.zero?
      s.slider_value = (s.wave_freq - s.min_freq) / range
    end
  end

  # ---- 输入处理 ----------------------------------------------------------

  def handle_input(args)
    handle_keyboard_input(args)
    handle_slider_input(args)
  end

  # 键盘 ← → 调频（兼容 Day 2）
  def handle_keyboard_input(args)
    s = args.state
    step = 0.05

    if args.inputs.left
      s.wave_freq -= step
    elsif args.inputs.right
      s.wave_freq += step
    end

    # 限制频率在合法范围内
    s.wave_freq = s.min_freq if s.wave_freq < s.min_freq
    s.wave_freq = s.max_freq if s.wave_freq > s.max_freq

    # 把 wave_freq 映射回 slider_value
    range = (s.max_freq - s.min_freq)
    range = 1.0 if range.zero?
    s.slider_value = (s.wave_freq - s.min_freq) / range
  end

  # 鼠标点击滑块：根据点击位置设置 slider_value → wave_freq
  def handle_slider_input(args)
    s = args.state
    rect = s.slider_rect
    click = args.inputs.mouse.click
    return unless click

    # DragonRuby: click.point.inside_rect?(hash_rect) 用于点击检测
    if click.point.inside_rect?(rect)
      rel_x = click.point.x - rect[:x]
      rel_x = 0 if rel_x < 0
      rel_x = rect[:w] if rel_x > rect[:w]

      s.slider_value = rel_x / rect[:w].to_f

      range = (s.max_freq - s.min_freq)
      range = 1.0 if range.zero?
      s.wave_freq = s.min_freq + s.slider_value * range
    end
  end

  # 把所有依赖值同步一下，方便别的系统读取
  def update_derived_state(args)
    s = args.state
    s.frequency_target = s.wave_freq
  end

  # ---- 渲染 --------------------------------------------------------------

  def render(args)
    render_background(args)
    render_wave(args)
    render_ui(args)
  end

  # 简单的深色背景 + 顶部标题
  def render_background(args)
    args.outputs.solids << [0, 0, 1280, 720, 6, 12, 24]

    args.outputs.labels << [
      640,
      700,
      "Signal Lost / 孤波  -  Boot Prototype",
      1, # alignment_enum: center
      0, # size_enum
      200,
      220,
      255
    ]
  end

  # 核心：绘制正弦波
  def render_wave(args)
    s = args.state
    lines = args.outputs.lines

    # 初始化 & 推进相位，让波形在时间轴上“流动”
    s.wave_phase ||= 0.0
    s.wave_phase += s.wave_speed || 0.08

    screen_w = 1280
    center_y = 360 # 波形中线
    amplitude = 80 # 振幅（上下波动的高度）
    step_x = 4 # 每段线段的横向步长（越小越平滑但越耗性能）

    # 基础颜色：偏冷的蓝色，带一点亮度
    color = [120, 200, 255, 255] # r, g, b, a

    x = 0
    while x < screen_w
      # 根据 x、频率和相位计算正弦值
      rad1 = s.wave_phase + x * s.wave_freq * 0.01
      rad2 = s.wave_phase + (x + step_x) * s.wave_freq * 0.01

      y1 = center_y + Math.sin(rad1) * amplitude
      y2 = center_y + Math.sin(rad2) * amplitude

      # line: [x, y, x2, y2, r, g, b, a]
      lines << [x, y1, x + step_x, y2, *color]

      x += step_x
    end

    # 画一条中线，帮助玩家感受波形上下起伏
    args.outputs.lines << [0, center_y, screen_w, center_y, 40, 80, 120, 180]
  end

  # Day 3: 调频滑块 + 文本提示
  def render_ui(args)
    s = args.state
    rect = s.slider_rect

    # 滑轨
    args.outputs.solids << [
      rect[:x],
      rect[:y],
      rect[:w],
      rect[:h],
      20,
      30,
      50,
      220
    ]

    args.outputs.borders << [
      rect[:x],
      rect[:y],
      rect[:w],
      rect[:h],
      180,
      200,
      230,
      255
    ]

    # 滑块小方块
    handle_size = rect[:h] * 1.8
    handle_center_x = rect[:x] + s.slider_value * rect[:w]
    handle_center_y = rect[:y] + rect[:h] / 2

    args.outputs.solids << [
      handle_center_x - handle_size / 2,
      handle_center_y - handle_size / 2,
      handle_size,
      handle_size,
      220,
      240,
      255,
      255
    ]

    # 提示文字
    args.outputs.labels << [
      rect[:x],
      rect[:y] + rect[:h] + 40,
      "← → 调节频率 / Click 滑块调频",
      0,
      0,
      200,
      220,
      255
    ]

    # 当前频率数值
    args.outputs.labels << [
      rect[:x],
      rect[:y] + rect[:h] + 20,
      "Current freq: #{s.wave_freq.round(2)}  (target: #{s.frequency_target.round(2)})",
      0,
      0,
      190,
      210,
      230
    ]
  end
end
