# ================================================================
# 文件路径：app/scenes/boot_scene.rb
# 场景名称：BootScene（启动场景 / 波形原型）
#
# 项目阶段说明：
#   Day 2:
#     - 实现基础波形（正弦波）渲染，并支持 ← → 键调节频率
#   Day 3:
#     - 增加交互式“调频滑块”（frequency slider），允许玩家通过鼠标点击改变波形频率
#   Day 4:
#     - 增加音频反馈：持续播放带噪声的无线电声，根据当前频率与目标频率的偏差，
#       动态调整噪声与“信号音”的音量，模拟“接近真频”的感觉
#   Day 5:
#     - 接近提示（Proximity Hint）：当频率接近目标值时，视觉上逐步“净化”——波形更亮、抖动更小；
#       滑块上出现目标窗口高亮带（Lock Band），并在锁定阈值内显示“Signal locked”。
#
# 主要目标：
#   - 展示动态“无线电波”视觉效果
#   - 实现简单的信号调频机制（键盘 + 滑块）
#   - 为后续关卡与剧情提供基础的音画反馈系统
# ================================================================

class BootScene
  # 一些简单的常量，方便统一调节
  SLIDER_RECT = { x: 160, y: 120, w: 960, h: 24 }.freeze

  # 频率范围（只是视觉/听觉上的抽象，不是物理真实值）
  MIN_FREQ = 0.5
  MAX_FREQ = 8.0

  # 目标频率（Day 4 先固定为一个值，后面可以做成每关不同）
  TARGET_FREQ = 4.2

  # 允许认为“非常接近”的频差（用于 UI 提示）
  LOCK_TOLERANCE = 0.08

  def initialize
    @initialized = false
  end

  # DragonRuby 每帧会调用当前场景的 tick(args)
  def tick(args)
    # -------------------------------------------------------------
    # 主循环：按帧驱动（DragonRuby 每帧调用）
    # 步骤：初始化 → 处理输入 → 更新波形 → 更新音频 → 渲染
    # -------------------------------------------------------------
    init_state(args) unless @initialized
    handle_input(args) # Day 2–3：键盘 + 滑块输入
    update_wave(args) # Day 2–3：根据频率更新波形动画参数
    update_audio(args) # Day 4：根据频率偏差调节噪声/信号音量
    render(args) # 渲染波形、UI 与调试文本
  end

  # -------------------------------------------------------------
  # 初始化：只在第一次进入场景时执行一次
  # -------------------------------------------------------------
  def init_state(args)
    # -------------------------------------------------------------
    # 初始化：只在第一次进入场景时执行一次
    # 职责：建立默认参数、UI 几何信息、音频开关等状态
    # -------------------------------------------------------------
    s = state(args)

    # 逻辑频率参数
    s.wave_freq ||= 2.0 # 当前波形频率
    s.frequency_target ||= TARGET_FREQ.to_f # 目标频率
    s.slider_value ||= 0.35 # 0.0~1.0 之间的归一化值
    s.phase ||= 0.0 # 用于波形动画的相位

    # 调频滑块的矩形
    s.slider_rect ||= SLIDER_RECT.dup

    # 输入状态（避免一次按键重复触发）
    s.last_mouse_down ||= false

    # Day 4：音频初始化
    ensure_audio_started(args)

    @initialized = true
  end

  # 一个小工具方法，统一访问本场景在 args.state 中的存储
  def state(args)
    # 小工具：统一获取/创建本场景的 state 容器（args.state.boot_scene）
    args.state.boot_scene ||= args.state.new_entity(:boot_scene)
  end

  # -------------------------------------------------------------
  # 输入处理：键盘 ← → 微调 + 鼠标点击滑块
  # -------------------------------------------------------------
  def handle_input(args)
    # -------------------------------------------------------------
    # 输入处理：键盘 ← → 微调 + 鼠标点击滑块
    # 说明：
    #   - 键盘：按住左右键以恒定速度微调 slider_value
    #   - 鼠标：点击滑块区域直接跳转到对应位置（并 clamp 到 [0, 1]）
    # -------------------------------------------------------------
    s = state(args)
    kb = args.inputs.keyboard
    ms = args.inputs.mouse

    # 1）键盘左右键：微调 slider_value
    #    这里假设 60fps，一秒钟大概可以移动 0.5 的 slider 区间
    if kb.left
      s.slider_value -= 0.5 / 60.0
    elsif kb.right
      s.slider_value += 0.5 / 60.0
    end

    # clamp 到 [0, 1]
    if s.slider_value < 0.0
      s.slider_value = 0.0
    elsif s.slider_value > 1.0
      s.slider_value = 1.0
    end

    # 2）鼠标点击滑块：直接跳到鼠标所在位置
    rect = s.slider_rect
    rect_array = [rect[:x], rect[:y], rect[:w], rect[:h]]

    mouse_down = ms.button_left
    just_clicked = mouse_down && !s.last_mouse_down

    if just_clicked && [ms.x, ms.y].inside_rect?(rect_array)
      relative_x = (ms.x - rect[:x]) / rect[:w].to_f

      # 同样 clamp 到 [0, 1]
      if relative_x < 0.0
        relative_x = 0.0
      elsif relative_x > 1.0
        relative_x = 1.0
      end

      s.slider_value = relative_x
    end

    s.last_mouse_down = mouse_down

    # 根据 slider_value 更新当前频率
    # 将 0~1 的 slider_value 映射为实际频率范围
    s.wave_freq = MIN_FREQ + s.slider_value * (MAX_FREQ - MIN_FREQ)
  end

  # -------------------------------------------------------------
  # 更新波形动画（主要是相位）
  # -------------------------------------------------------------
  def update_wave(args)
    # -------------------------------------------------------------
    # 更新波形动画相位：根据时间推进，让波形“流动”
    # 备注：Kernel.tick_count 是自启动以来的帧数，60fps 下 1 秒约 60 帧
    # -------------------------------------------------------------
    s = state(args)
    # 简单做一个基于时间的相位偏移，让波形“向前流动”
    # Kernel.tick_count 是游戏运行到现在的总帧数
    s.phase = Kernel.tick_count / 60.0 * 0.6
  end

  # -------------------------------------------------------------
  # Day 4：音频逻辑
  #
  # 我们使用两个持续播放的通道：
  #   - :radio_noise  —— 无线电噪声（radio_static.wav）
  #   - :radio_tone   —— 信号音/蜂鸣（shortwave_beep.wav）
  #
  # 根据当前频率与目标频率的差值：
  #   - 越接近目标频率：噪声逐渐变小，信号音变大
  #   - 远离目标频率：噪声占主导，信号音几乎听不到
  # -------------------------------------------------------------
  def ensure_audio_started(args)
    # -------------------------------------------------------------
    # 音频：懒加载与循环播放
    # 包含两条轨：radio_noise（噪声）与 radio_tone（信号音）
    # 只需在首次进入场景时启动，之后每帧仅更新 gain
    # -------------------------------------------------------------
    # 只需要在第一次进入场景时设置一次即可
    # radio_static.wav 建议是连续的“沙沙”噪声，放在 app/audio/ 下
    args.audio[:radio_noise] ||= {
      input: "sounds/radio_static_16bit.wav",
      looping: true,
      gain: 0.9
    }

    # shortwave_beep.wav 建议是有一点音高的短波哔声，可以循环播放
    args.audio[:radio_tone] ||= {
      input: "sounds/shortwave_beep_16bit.wav",
      looping: true,
      gain: 0.0 # 初始时几乎听不到信号
    }
  end

  def update_audio(args)
    # -------------------------------------------------------------
    # 根据频率偏差调节音量（Day 4）
    # 接近目标频率：噪声减小、信号音增大；远离则相反
    # 映射：freq_diff → proximity ∈ [0,1]（指数衰减）
    # -------------------------------------------------------------
    s = state(args)

    proximity = compute_proximity(s)

    # 噪声音量：远时接近 1，近时降到一个中等水平（避免完全静音显得太突兀）
    noise_gain = 0.3 + (1.0 - proximity) * 0.7
    # 信号音量：接近目标频率时接近 1
    tone_gain = proximity

    args.audio[:radio_noise].gain = noise_gain if args.audio[:radio_noise]

    if args.audio[:radio_tone]
      args.audio[:radio_tone].gain = tone_gain
      # 可选：也可以轻微根据当前频率调整 pitch，制造“滑音”感觉：
      # 比如：
      #   args.audio[:radio_tone].pitch = 0.8 + proximity * 0.4
      # 目前先保持 1.0，后续关卡需要时再开启
    end

    # 噪声音量：远时接近 1，近时降到一个中等水平（避免完全静音显得太突兀）
    noise_gain = 0.3 + (1.0 - proximity) * 0.7
    # 信号音量：接近目标频率时接近 1
    tone_gain = proximity

    args.audio[:radio_noise].gain = noise_gain if args.audio[:radio_noise]

    if args.audio[:radio_tone]
      args.audio[:radio_tone].gain = tone_gain
      # 可选：也可以轻微根据当前频率调整 pitch，制造“滑音”感觉：
      # 比如：
      #   args.audio[:radio_tone].pitch = 0.8 + proximity * 0.4
      # 目前先保持 1.0，后续关卡需要时再开启
    end
  end

  # -------------------------------------------------------------
  # 渲染：波形 + 滑块 + 文本 UI
  # -------------------------------------------------------------
  def render(args)
    # -------------------------------------------------------------
    # 渲染入口：背景 → 波形 → 滑块/锁定带 → 文本
    # -------------------------------------------------------------
    s = state(args)

    render_background(args)
    render_wave(args, s)
    render_slider(args, s)
    render_text(args, s)
  end

  def render_background(args)
    # -------------------------------------------------------------
    # 背景层：提供柔和底色与边距，便于区分 UI 元素
    # -------------------------------------------------------------
    # 简单深蓝色背景，后续可以替换为图像或更复杂的场景
    args.outputs.solids << { x: 0, y: 0, w: 1280, h: 720, r: 5, g: 10, b: 25 }
  end

  def render_wave(args, s)
    # -------------------------------------------------------------
    # 绘制波形：用多段线近似正弦曲线
    # Day 5：离目标越近，抖动越小、亮度越高（“净化”效果）
    # 实现：基础振幅 + 轻微随机扰动（随 proximity 衰减）
    # -------------------------------------------------------------
    # 接近度用于视觉“净化”
    proximity = compute_proximity(s)

    lines = []
    x_start = 80
    x_end = 1200
    width = x_end - x_start
    center_y = 420

    # 振幅与随机抖动：越接近越稳定
    base_amplitude = 56
    # Day 5：接近时抖动收敛；远离时抖动明显
    jitter_scale = (1.0 - proximity) * 12.0

    samples = 220
    amplitude = base_amplitude

    (0..samples - 1).each do |i|
      t1 = i.to_f / samples
      t2 = (i + 1).to_f / samples

      x1 = x_start + t1 * width
      x2 = x_start + t2 * width

      # 引入轻微随机抖动（只影响远离时的“噪波”感）
      n1 = (Kernel.rand * jitter_scale) - jitter_scale * 0.5
      n2 = (Kernel.rand * jitter_scale) - jitter_scale * 0.5

      y1 =
        center_y +
          Math.sin(2 * Math::PI * (t1 * s.wave_freq + s.phase)) * amplitude + n1
      y2 =
        center_y +
          Math.sin(2 * Math::PI * (t2 * s.wave_freq + s.phase)) * amplitude + n2

      # 基础波线：颜色随接近度增亮
      c = 160 + (95 * proximity).to_i # 160~255
      lines << { x: x1, y: y1, x2: x2, y2: y2, r: c, g: c - 20, b: 255 }
    end

    args.outputs.lines << lines
  end

  def render_slider(args, s)
    # -------------------------------------------------------------
    # 绘制调频滑块（frequency slider）
    # 元素：底条、填充条、滑块圆头、数值与强度条、锁定提示
    # 包含：调用 render_lock_band 渲染目标频段高亮带
    # -------------------------------------------------------------
    rect = s.slider_rect

    # 滑槽背景
    args.outputs.solids << {
      x: rect[:x],
      y: rect[:y],
      w: rect[:w],
      h: rect[:h],
      r: 30,
      g: 40,
      b: 70
    }

    # 当前滑块位置
    knob_x = rect[:x] + rect[:w] * s.slider_value

    args.outputs.solids << {
      x: knob_x - 6,
      y: rect[:y] - 4,
      w: 12,
      h: rect[:h] + 8,
      r: 200,
      g: 220,
      b: 255
    }
  end

  def render_text(args, s)
    # -------------------------------------------------------------
    # 文本标签：辅助调试与玩家反馈（当前频率/目标/Δ/锁定状态）
    # -------------------------------------------------------------
    rect = s.slider_rect

    freq = s.wave_freq
    target = s.frequency_target
    # 频率偏差（绝对值）：越小越接近目标
    freq_diff = (freq - target).abs
    # 将偏差映射到 [0,1]：指数衰减，远离→接近 0，接近→接近 1
    proximity = Math.exp(-freq_diff * 1.0)
    if proximity < 0.0
      proximity = 0.0
    elsif proximity > 1.0
      proximity = 1.0
    end

    locked = freq_diff <= LOCK_TOLERANCE

    # 当前频率显示（实时数值）
    args.outputs.labels << [
      rect[:x],
      rect[:y] + rect[:h] + 34,
      "Current freq: #{freq.round(2)}   Target: #{target.round(2)}   Δ = #{freq_diff.round(3)}",
      0,
      0,
      190,
      210,
      230
    ]

    # 信号强度条
    strength_bar_w = 260
    strength_x = rect[:x]
    strength_y = rect[:y] + rect[:h] + 70

    # 背景条
    args.outputs.solids << {
      x: strength_x,
      y: strength_y,
      w: strength_bar_w,
      h: 14,
      r: 25,
      g: 30,
      b: 55
    }

    # 绿色强度部分
    args.outputs.solids << {
      x: strength_x,
      y: strength_y,
      w: strength_bar_w * proximity,
      h: 14,
      r: 80,
      g: 200,
      b: 140
    }

    # 文字提示
    label_text =
      if locked
        "Signal locked. Noise becomes gentle."
      else
        "Adjust frequency until signal stabilises..."
      end

    args.outputs.labels << [
      strength_x,
      strength_y + 26,
      label_text,
      0,
      0,
      180,
      200,
      220
    ]
  end
end

# --- Merged additions from boot_scene1.rb ---
def compute_proximity(s)
  # -------------------------------------------------------------
  # 工具函数：将当前频差映射为 [0, 1] 的接近度
  # 映射方式：proximity = exp(-|freq - target| * k)，k=1.0
  # -------------------------------------------------------------
  freq_diff = (s.wave_freq - s.frequency_target).abs
  proximity = Math.exp(-freq_diff * 1.0)
  proximity = 0.0 if proximity < 0.0
  proximity = 1.0 if proximity > 1.0
  proximity
end
def render_lock_band(args, s)
  # -------------------------------------------------------------
  # 目标频段高亮带（Lock Band）
  # 说明：以 TARGET_FREQ 为中心，LOCK_TOLERANCE 为半宽，在滑块上画半透明带
  # 同时在中心画准星线；透明度/亮度可随 proximity 轻微变化
  # -------------------------------------------------------------
  rect = s.slider_rect

  # 目标频率在 slider 上的归一化位置
  target_norm = (s.frequency_target - MIN_FREQ) / (MAX_FREQ - MIN_FREQ)
  target_x = rect[:x] + rect[:w] * target_norm

  # 把 LOCK_TOLERANCE 转成 slider 上的像素宽度
  tol_norm = LOCK_TOLERANCE / (MAX_FREQ - MIN_FREQ)
  band_w = rect[:w] * tol_norm * 2.0 # 左右各一半
  band_x = target_x - band_w * 0.5

  # 透明度随接近度稍增
  proximity = compute_proximity(s)
  alpha = (60 + proximity * 80).to_i

  # 高亮带（轻微呼吸动画）
  pulse = (Math.sin(Kernel.tick_count / 30.0) * 0.5 + 0.5) * 20
  args.outputs.solids << {
    x: band_x,
    y: rect[:y] - 6,
    w: band_w,
    h: rect[:h] + 12,
    r: 100,
    g: 200,
    b: 220,
    a: alpha + pulse.to_i
  }

  # 在目标中心画一条细的准星线
  args.outputs.lines << {
    x: target_x,
    y: rect[:y] - 12,
    x2: target_x,
    y2: rect[:y] + rect[:h] + 12,
    r: 180,
    g: 220,
    b: 255,
    a: 160
  }
end
