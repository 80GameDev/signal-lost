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
#   Day 6–9:
#     - 引入 LEVELS 配置，区分干涉 / 共振 / 衰减三类关卡，并为每关配置频率、容差与提示文本。
#     - 根据关卡类型切换波形渲染与音频逻辑，形成 3 个可玩谜题原型。
#   Day 10:
#     - 增加关卡标题与整体进度显示（render_level_header），方便进行“谜题整合测试”，
#       在画面顶部一眼看出当前关卡与完成状态。
#
# 主要目标：
#   - 展示动态“无线电波”视觉效果
#   - 实现简单的信号调频机制（键盘 + 滑块）
#   - 为后续关卡与剧情提供基础的音画反馈系统
# ================================================================

class BootScene
  def level_index(args)
    args.state.level_index ||= 0
    args.state.level_index
  end

  def level_data(args)
    LEVELS[level_index(args)] || LEVELS.last
  end

  def target_freq(args)
    if (ld = level_data(args)) && ld[:freq]
      ld[:freq].to_f
    elsif defined?(TARGET_FREQ)
      TARGET_FREQ.to_f
    else
      4.2
    end
  end

  def lock_tolerance(args)
    if (ld = level_data(args)) && ld[:lock_tolerance]
      ld[:lock_tolerance].to_f
    else
      0.08
    end
  end

  def level_hint(args)
    (
      begin
        level_data(args)[:hint]
      rescue StandardError
        nil
      end
    )
  end

  # 若外部未定义 LEVELS，这里提供一个最小默认，保证兼容
  LEVELS = [
    {
      freq: 4.2,
      lock_tolerance: 0.08,
      hint: "Tune to ~4.2 Hz to lock the first signal."
    }
  ] unless self.class.const_defined?(:LEVELS)

  # 一些简单的常量，方便统一调节
  SLIDER_RECT = { x: 160, y: 120, w: 960, h: 24 }.freeze

  # 频率范围（只是视觉/听觉上的抽象，不是物理真实值）
  MIN_FREQ = 0.5
  MAX_FREQ = 8.0

  # 目标频率（Day 4 先固定为一个值，后面可以做成每关不同）
  TARGET_FREQ = 4.2

  # 允许认为“非常接近”的频差（用于 UI 提示）
  LOCK_TOLERANCE = 0.08

  # --- DAY 8 CHANGE START: resonance level ---
  # 声音资源常量（集中声明，便于你后续对照修改）
  SND_NOISE_DEFAULT = "sounds/radio_static_16bit.wav"
  SND_TONE_DEFAULT = "sounds/shortwave_beep_16bit.wav"
  SND_NOISE_RESO = "sounds/radio_static_16bit.wav" # resonance_base.wav 如文件名不同，改这里即可
  SND_TONE_RESO = "sounds/shortwave_beep_16bit.wav" # resonance_clear.wav 如文件名不同，改这里即可

  def level_type(args)
    (
      begin
        level_data(args)[:type]
      rescue StandardError
        nil
      end
    ) || :interference
  end

  def resonance_params(args)
    ld = level_data(args) || {}
    {
      glow_threshold: (ld[:glow_threshold] || 0.92).to_f,
      amp_gain_max: (ld[:amp_gain_max] || 2.2).to_f
    }
  end
  # --- DAY 8 CHANGE END ---

  # --- DAY 9 CHANGE START: attenuation level params ---
  def attenuation_params(args)
    ld = level_data(args) || {}
    {
      # 何时开始衰减（帧数）
      decay_start_frames: (ld[:decay_start_frames] || 90).to_i,
      # 从开始衰减到“几乎完全衰减”的持续时间
      decay_duration_frames: (ld[:decay_duration_frames] || 600).to_i,
      # 最低残留强度：避免完全为 0，保留一点“幽灵信号”
      min_decay_factor: (ld[:min_decay_factor] || 0.15).to_f
    }
  end

  def initialize
    @initialized = false
  end

  # DragonRuby 每帧会调用当前场景的 tick(args)
  def tick(args)
    # -------------------------------------------------------------
    # 主循环：按帧驱动（DragonRuby 每帧调用）
    # 步骤：初始化 → 处理输入 → 更新波形 → 更新音频 → 更新关卡 → 渲染
    # -------------------------------------------------------------
    init_state(args) unless @initialized
    handle_input(args) # Day 2–3：键盘 + 滑块输入
    update_wave(args) # Day 2–3：根据频率更新波形动画参数
    update_audio(args) # Day 4：根据频率偏差调节噪声/信号音量
    update_level_progress(args) # Day 7：根据频率锁定情况更新关卡状态
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
    s.frequency_target = target_freq(args).to_f #s.frequency_target ||= TARGET_FREQ.to_f # 目标频率
    s.slider_value ||= 0.1 # 0.0~1.0 之间的归一化值
    s.phase ||= 0.0 # 用于波形动画的相位
    s.wave_points = [] # 波形点缓存
    s.wave_color = { r: 190, g: 210, b: 230 }
    s.noise_gain = 0.5
    s.signal_gain = 0.0

    # Day 7：关卡进度状态
    s.locked_frames ||= 0
    s.level_cleared = false if s.level_cleared.nil?
    # --- DAY 9 CHANGE START: level elapsed frames ---
    # 记录当前关卡已运行帧数（供“衰减/失真”关卡使用）
    s.level_elapsed_frames ||= 0
    # --- DAY 9 CHANGE END ---

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

    # Day 7：调试键，按 L 回到关卡 1（干涉波原型）
    if kb.key_down.l
      args.state.level_index = 0
      s.frequency_target = target_freq(args).to_f
      s.slider_value = 0.1
      s.phase = 0.0
      s.locked_frames = 0
      s.level_cleared = false
    end
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

    # --- DAY 9 CHANGE START: attenuation timer ---
    # 对“衰减/失真”关卡记录已运行的帧数，其它关卡只是确保字段存在
    if level_type(args) == :attenuation && !s.level_cleared
      s.level_elapsed_frames ||= 0
      s.level_elapsed_frames += 1
    else
      s.level_elapsed_frames ||= 0
    end
    # --- DAY 9 CHANGE END ---

    # 简单做一个基于时间的相位偏移，让波形“向前流动”
    # Kernel.tick_count 是游戏运行到现在的总帧数
    # s.phase = Kernel.tick_count / 60.0 * 0.6
    s.wave_freq = MIN_FREQ + (MAX_FREQ - MIN_FREQ) * s.slider_value

    # 相位推进，使波形动起来
    s.phase += s.wave_freq / 60 * 0.6
    # s.phase -= 2 * Math::PI while s.phase > 2 * Math::PI
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
    # 根据关卡类型与关卡自定义字段选择音频文件
    # -------------------------------------------------------------
    # 音频：懒加载与循环播放
    # 包含两条轨：radio_noise（噪声）与 radio_tone（信号音）
    # 只需在首次进入场景时启动，之后每帧仅更新 gain
    # -------------------------------------------------------------
    # 只需要在第一次进入场景时设置一次即可
    ld = level_data(args) || {}
    t = level_type(args)

    desired_noise =
      ld[:noise_file] || (t == :resonance ? SND_NOISE_RESO : SND_NOISE_DEFAULT)
    desired_tone =
      ld[:tone_file] || (t == :resonance ? SND_TONE_RESO : SND_TONE_DEFAULT)

    # 如果还没建，或文件与期望不一致，则（重新）设置
    if !args.audio[:radio_noise] ||
         args.audio[:radio_noise].input != desired_noise
      args.audio[:radio_noise] = {
        input: desired_noise,
        looping: true,
        gain: 0.9
      }
    end

    if !args.audio[:radio_tone] || args.audio[:radio_tone].input != desired_tone
      args.audio[:radio_tone] = {
        input: desired_tone,
        looping: true,
        gain: 0.0,
        pitch: 1.0
      }
    end
  end
  # --- DAY 8 CHANGE END ---

  def update_audio(args)
    # -------------------------------------------------------------
    # 根据频率偏差调节音量（Day 4）
    # 接近目标频率：噪声减小、信号音增大；远离则相反
    # 映射：freq_diff → proximity ∈ [0,1]（指数衰减）
    # -------------------------------------------------------------
    s = state(args)
    proximity = compute_proximity(s)

    case level_type(args)
    when :resonance
      # 共振：信号更清晰、噪声更快收敛
      tone_gain = 0.15 + 0.85 * (proximity**2)
      noise_gain = 0.15 + 0.85 * (1.0 - proximity**2)
      # 可选：让音调也随接近度略微上扬
      if args.audio[:radio_tone]
        args.audio[:radio_tone].pitch = 0.9 + 0.2 * proximity
      end

      # --- DAY 9 CHANGE START: attenuation audio ---
    when :attenuation
      params = attenuation_params(args)
      elapsed = (s.level_elapsed_frames || 0).to_i
      start = params[:decay_start_frames]
      dur = params[:decay_duration_frames]
      min_f = params[:min_decay_factor]

      # 计算随时间变化的衰减因子 decay_factor ∈ [min_f, 1]
      if elapsed <= start
        decay_factor = 1.0
      else
        t = ((elapsed - start).to_f / dur.to_f)
        t = 1.0 if t > 1.0
        t = 0.0 if t < 0.0
        decay_factor = 1.0 - t
      end
      decay_factor = min_f if decay_factor < min_f

      base_tone = proximity
      base_noise = 0.3 + (1.0 - proximity) * 0.7

      # 信号：随时间整体变弱，但接近目标时仍略有优势
      tone_gain = base_tone * (0.25 + 0.75 * decay_factor)
      # 噪声：随衰减逐渐加强，模拟“信号被噪声吞没”
      noise_mix = 0.4 + 0.6 * (1.0 - decay_factor)
      noise_gain = base_noise * noise_mix

      # 轻微 pitch 抖动，制造“失真感”
      if args.audio[:radio_tone]
        jitter = (1.0 - decay_factor) * 0.04
        base_pitch = 1.0 + (proximity * 0.1)
        args.audio[:radio_tone].pitch =
          base_pitch + (Kernel.rand * jitter - jitter * 0.5)
      end
      # --- DAY 9 CHANGE END: attenuation audio ---
    else
      # 其它关卡沿用 Day 4 的默认曲线
      tone_gain = proximity
      noise_gain = 0.3 + (1.0 - proximity) * 0.7
    end

    args.audio[:radio_tone].gain = tone_gain if args.audio[:radio_tone]
    args.audio[:radio_noise].gain = noise_gain if args.audio[:radio_noise]
  end
  # --- DAY 8 CHANGE END ---

  # -------------------------------------------------------------
  # 渲染：波形 + 滑块 + 文本 UI
  # -------------------------------------------------------------
  def render(args)
    # -------------------------------------------------------------
    # 渲染入口：背景 → 关卡标题 → 波形 → 滑块/锁定带 → 文本
    # Day 10：在顶部显示当前关卡信息，便于调试与整体验证
    # -------------------------------------------------------------
    s = state(args)

    render_background(args)
    render_level_header(args, s)
    render_wave(args, s)
    render_slider(args, s)
    render_proximity_ui(args, s)
    render_lock_band(args, s)
  end

  def render_background(args)
    # -------------------------------------------------------------
    # 背景层：提供柔和底色与边距，便于区分 UI 元素
    # -------------------------------------------------------------
    # 简单深蓝色背景，后续可以替换为图像或更复杂的场景
    args.outputs.solids << { x: 0, y: 0, w: 1280, h: 720, r: 5, g: 10, b: 25 }
  end

  def render_level_header(args, s)
    # -------------------------------------------------------------
    # Day 10：关卡标题与整体进度显示
    # 在画面上方展示：
    #   - 当前关卡序号 / 总关卡数
    #   - 关卡名称（如 Interference / Resonance / Attenuation）
    #   - 当前状态（TUNING / CLEARED / FINAL）
    # 方便在 Day 10 进行“谜题整合测试”和后续剧情整合。
    # -------------------------------------------------------------
    ld = level_data(args) || {}
    index = level_index(args)
    total = LEVELS.length
    name = ld[:name] || "Signal"
    type = (ld[:type] || :interference).to_s.capitalize

    status =
      if s.level_cleared && index == total - 1
        "FINAL"
      elsif s.level_cleared
        "CLEARED"
      else
        "TUNING"
      end

    label_text = "Level #{index + 1}/#{total} — #{name} (#{type}) [#{status}]"

    args.outputs.labels << [
      40, # x
      700, # y
      label_text, # 文本
      0, # 对齐（左对齐）
      0, # 对齐（基线）
      210,
      235,
      255
    ]
  end

  def render_wave(args, s)
    # -------------------------------------------------------------
    # 绘制波形：用多段线近似正弦曲线
    # Day 5：离目标越近，抖动越小、亮度越高（“净化”效果）
    # 实现：基础振幅 + 轻微随机扰动（随 proximity 衰减）
    # -------------------------------------------------------------
    # 接近度用于视觉“净化”
    ld = level_data(args)

    case (ld && ld[:type])
    when :interference
      render_interference_wave(args, s)
    when :resonance
      render_resonance_wave(args, s)
    when :attenuation
      # --- DAY 9 CHANGE: 使用专门的衰减波渲染 ---
      render_attenuation_wave(args, s)
    else
      render_single_wave(args, s)
    end
  end
  # --- DAY 8 CHANGE END ---

  # 原来的单波渲染逻辑，稍微包一层，供其它关卡复用
  def render_single_wave(args, s)
    proximity = compute_proximity(s)

    lines = []
    x_start = 80
    x_end = 1200
    width = x_end - x_start
    center_y = 420

    base_amplitude = 56
    jitter_scale = (1.0 - proximity) * 12.0
    samples = 220
    amplitude = base_amplitude

    (0..samples - 1).each do |i|
      t1 = i.to_f / samples
      t2 = (i + 1).to_f / samples

      x1 = x_start + t1 * width
      x2 = x_start + t2 * width

      n1 = (Kernel.rand * jitter_scale) - jitter_scale * 0.5
      n2 = (Kernel.rand * jitter_scale) - jitter_scale * 0.5

      y1 =
        center_y +
          Math.sin(2 * Math::PI * (t1 * s.wave_freq + s.phase)) * amplitude + n1
      y2 =
        center_y +
          Math.sin(2 * Math::PI * (t2 * s.wave_freq + s.phase)) * amplitude + n2

      c = 160 + (95 * proximity).to_i # 160~255
      lines << { x: x1, y: y1, x2: x2, y2: y2, r: c, g: c - 20, b: 255 }
    end

    args.outputs.lines << lines
  end

  # Day 7：干涉波可视化（两条波 + 叠加波）
  def render_interference_wave(args, s)
    proximity = compute_proximity(s)

    lines_carrier = []
    lines_player = []
    lines_combined = []

    x_start = 80
    x_end = 1200
    width = x_end - x_start
    center_y = 420

    # 频率与相位
    carrier_freq = s.frequency_target # 基准波：目标频率
    player_freq = s.wave_freq # 玩家当前调到的频率

    carrier_phase = s.phase
    player_phase = s.phase + 0.25 # 稍微错一点相位，视觉上更容易看出两条不同的波

    # 振幅和扰动：接近时叠加波更大、更平滑；远离时抖动更乱、振幅较低
    base_amp_carrier = 40
    base_amp_player = 40

    # 叠加波的振幅随 proximity 放大，远离时接近抵消
    combined_amp = 32 * (0.4 + proximity * 1.2) # 约 12.8 ~ 51.2

    jitter_scale = (1.0 - proximity) * 10.0
    samples = 220

    (0..samples - 1).each do |i|
      t1 = i.to_f / samples
      t2 = (i + 1).to_f / samples

      x1 = x_start + t1 * width
      x2 = x_start + t2 * width

      n1 = (Kernel.rand * jitter_scale) - jitter_scale * 0.5
      n2 = (Kernel.rand * jitter_scale) - jitter_scale * 0.5

      # 单独两条波
      y1_carrier =
        center_y +
          Math.sin(2 * Math::PI * (t1 * carrier_freq + carrier_phase)) *
            base_amp_carrier + n1
      y2_carrier =
        center_y +
          Math.sin(2 * Math::PI * (t2 * carrier_freq + carrier_phase)) *
            base_amp_carrier + n2

      y1_player =
        center_y +
          Math.sin(2 * Math::PI * (t1 * player_freq + player_phase)) *
            base_amp_player + n1
      y2_player =
        center_y +
          Math.sin(2 * Math::PI * (t2 * player_freq + player_phase)) *
            base_amp_player + n2

      # 叠加波：简单相加再缩放
      sum1 =
        Math.sin(2 * Math::PI * (t1 * carrier_freq + carrier_phase)) +
          Math.sin(2 * Math::PI * (t1 * player_freq + player_phase))
      sum2 =
        Math.sin(2 * Math::PI * (t2 * carrier_freq + carrier_phase)) +
          Math.sin(2 * Math::PI * (t2 * player_freq + player_phase))

      y1_combined = center_y + sum1 * combined_amp + n1 * 0.4
      y2_combined = center_y + sum2 * combined_amp + n2 * 0.4

      # 颜色：基准波偏蓝，玩家波偏洋红，叠加波更亮
      lines_carrier << {
        x: x1,
        y: y1_carrier,
        x2: x2,
        y2: y2_carrier,
        r: 80,
        g: 180,
        b: 255,
        a: 180
      }
      lines_player << {
        x: x1,
        y: y1_player,
        x2: x2,
        y2: y2_player,
        r: 230,
        g: 110,
        b: 220,
        a: 180
      }

      c = 200 + (55 * proximity).to_i # 接近时更亮
      lines_combined << {
        x: x1,
        y: y1_combined,
        x2: x2,
        y2: y2_combined,
        r: c,
        g: c,
        b: 255,
        a: 255
      }
    end

    args.outputs.lines << lines_carrier
    args.outputs.lines << lines_player
    args.outputs.lines << lines_combined

    # 简单文字标注
    args.outputs.labels << [
      x_start,
      center_y + 110,
      "Carrier (target)",
      0,
      0,
      160,
      200,
      255
    ]

    args.outputs.labels << [
      x_start,
      center_y - 110,
      "Your tuning",
      0,
      0,
      230,
      150,
      230
    ]

    args.outputs.labels << [
      x_start,
      center_y + 140,
      "Interference sum — becomes smooth & strong when frequencies match.",
      0,
      0,
      210,
      230,
      255
    ]
  end
  # --- DAY 8 CHANGE START: resonance level ---
  # 共振关渲染：接近目标频率时振幅放大 + 发光/halo
  def render_resonance_wave(args, s)
    p = compute_proximity(s)
    params = resonance_params(args)
    glow_threshold = params[:glow_threshold]
    amp_gain_max = params[:amp_gain_max]

    lines_main = []
    lines_halo = []

    x_start = 80
    x_end = 1200
    width = x_end - x_start
    center_y = 420

    base_amplitude = 56
    # 二次缓入，避免一上来太猛
    amp_scale = 1.0 + (amp_gain_max - 1.0) * (p * p)
    amplitude = base_amplitude * amp_scale

    p # 接近时抖动更小（让波形更“稳”），远离时抖动更大
    jitter_scale = (1.0 - p) * 8.0
    samples = 220

    (0..samples - 1).each do |i|
      t1 = i.to_f / samples
      t2 = (i + 1).to_f / samples

      x1 = x_start + t1 * width
      x2 = x_start + t2 * width

      n1 = (Kernel.rand * jitter_scale) - jitter_scale * 0.5
      n2 = (Kernel.rand * jitter_scale) - jitter_scale * 0.5

      y1 =
        center_y +
          Math.sin(2 * Math::PI * (t1 * s.wave_freq + s.phase)) * amplitude + n1
      y2 =
        center_y +
          Math.sin(2 * Math::PI * (t2 * s.wave_freq + s.phase)) * amplitude + n2

      # 主线颜色：随接近度升亮
      c = 160 + (95 * p).to_i # 160~255
      lines_main << { x: x1, y: y1, x2: x2, y2: y2, r: c, g: c, b: 255, a: 255 }

      # 若达到发光阈值，再叠加一层 halo
      if p >= glow_threshold
        # 更亮、更透明，画几条微小偏移，模拟粗线/光晕
        halo_alpha = 160
        halo_color = { r: 220, g: 235, b: 255, a: halo_alpha }
        offsets = [[0, 0], [0, 1], [0, -1], [1, 0], [-1, 0]]
        offsets.each do |dx, dy|
          lines_halo << {
            x: x1 + dx,
            y: y1 + dy,
            x2: x2 + dx,
            y2: y2 + dy
          }.merge(halo_color)
        end
      end
    end

    args.outputs.lines << lines_halo unless lines_halo.empty?
    args.outputs.lines << lines_main
  end
  # --- DAY 8 CHANGE END ---

  # --- DAY 9 CHANGE START: attenuation wave rendering ---
  # 衰减关卡波形：
  #   - 接近目标频率：波形仍相对平稳
  #   - 随时间推移：整体亮度和振幅下降，抖动和“噪点”增加
  def render_attenuation_wave(args, s)
    p = compute_proximity(s)
    params = attenuation_params(args)
    elapsed = (s.level_elapsed_frames || 0).to_i
    start = params[:decay_start_frames]
    dur = params[:decay_duration_frames]
    min_f = params[:min_decay_factor]

    if elapsed <= start
      decay_factor = 1.0
    else
      t = ((elapsed - start).to_f / dur.to_f)
      t = 1.0 if t > 1.0
      t = 0.0 if t < 0.0
      decay_factor = 1.0 - t
    end
    decay_factor = min_f if decay_factor < min_f

    x_start = 80
    x_end = 1200
    width = x_end - x_start
    center_y = 420

    base_amplitude = 56
    # 接近目标时放大一点，但整体受 decay_factor 压制
    amp_scale = (0.4 + 0.6 * p) * (0.4 + 0.6 * decay_factor)
    amplitude = base_amplitude * amp_scale

    # 抖动：远离 + 衰减越严重，抖动越大
    jitter_prox = (1.0 - p) * 8.0
    jitter_decay = (1.0 - decay_factor) * 12.0
    jitter_scale = jitter_prox + jitter_decay

    samples = 220
    lines = []

    (0..samples - 1).each do |i|
      t1 = i.to_f / samples
      t2 = (i + 1).to_f / samples

      x1 = x_start + t1 * width
      x2 = x_start + t2 * width

      n1 = (Kernel.rand * jitter_scale) - jitter_scale * 0.5
      n2 = (Kernel.rand * jitter_scale) - jitter_scale * 0.5

      y1 =
        center_y +
          Math.sin(2 * Math::PI * (t1 * s.wave_freq + s.phase)) * amplitude + n1
      y2 =
        center_y +
          Math.sin(2 * Math::PI * (t2 * s.wave_freq + s.phase)) * amplitude + n2

      # 颜色：随接近度和衰减因子一起变暗
      c = (120 + 95 * p * decay_factor).to_i # 120 ~ 215
      a = (80 + 175 * decay_factor).to_i # 80 ~ 255

      lines << { x: x1, y: y1, x2: x2, y2: y2, r: c, g: c - 20, b: 255, a: a }
    end

    args.outputs.lines << lines

    # 额外：在衰减接近尾声时，加一点横向“噪点线”模拟信号崩坏
    if decay_factor <= min_f + 0.05
      glitch_lines = []
      10.times do
        y = center_y + (Kernel.rand * 140 - 70)
        glitch_lines << {
          x: x_start,
          y: y,
          x2: x_end,
          y2: y,
          r: 180,
          g: 180,
          b: 255,
          a: 80
        }
      end
      args.outputs.lines << glitch_lines
    end
  end
  # --- DAY 9 CHANGE END: attenuation wave rendering ---

  def render_slider(args, s)
    # -------------------------------------------------------------
    # 绘制调频滑块（frequency slider）
    # 元素：底条、填充条、滑块圆头、数值与强度条、锁定提示
    # 包含：调用 render_lock_band 渲染目标频段高亮带
    # -------------------------------------------------------------
    rect = s.slider_rect
    # 背景槽
    args.outputs.solids << {
      x: rect[:x],
      y: rect[:y],
      w: rect[:w],
      h: rect[:h],
      r: 30,
      g: 40,
      b: 60,
      a: 200
    }

    # 目标频率位置
    target = s.frequency_target
    target_norm = (target - MIN_FREQ) / (MAX_FREQ - MIN_FREQ).to_f
    target_x = rect[:x] + rect[:w] * target_norm

    # 当前滑块位置
    knob_x = rect[:x] + rect[:w] * s.slider_value
    knob = {
      x: knob_x - 4,
      y: rect[:y] - 6,
      w: 8,
      h: rect[:h] + 12,
      r: 200,
      g: 220,
      b: 255
    }
    args.outputs.solids << knob

    # 目标细线
    args.outputs.lines << {
      x: target_x,
      y: rect[:y] - 8,
      x2: target_x,
      y2: rect[:y] + rect[:h] + 8,
      r: 160,
      g: 220,
      b: 255,
      a: 160
    }
  end

  # UI：接近度条、数值提示、锁定提示
  def render_proximity_ui(args, s)
    rect = s.slider_rect

    freq = s.wave_freq
    target = s.frequency_target
    freq_diff = (freq - target).abs
    proximity = compute_proximity(s)
    proximity = 0.0 if proximity < 0.0
    proximity = 1.0 if proximity > 1.0

    locked = freq_diff <= lock_tolerance(args)
    frames_needed = success_hold_frames(args)

    if locked
      hint = level_hint(args)

      if s.level_cleared
        # 已经满足持续帧数要求：提示可以继续推进
        text = hint || "Signal acquired. Press Enter to continue."
        # text = "Signal acquired. Press Enter to continue."
      else
        # 尚未满足：显示“稳定中 + 百分比”
        progress = (s.locked_frames.to_f / frames_needed.to_f)
        progress = 0.0 if progress < 0.0
        progress = 1.0 if progress > 1.0
        percent = (progress * 100).to_i
        text = hint || "Stabilizing signal... hold frequency (#{percent}%)"
        # text = "Stabilizing signal... hold frequency (#{percent}%)"
      end

      args.outputs.labels << [
        rect[:x],
        rect[:y] + rect[:h] + 90,
        text,
        0,
        0,
        200,
        240,
        255
      ]

      # 只有在真正"level_cleared" 后，Enter/Space 才推进关卡
      if s.level_cleared &&
           (
             args.inputs.keyboard.key_down.enter ||
               args.inputs.keyboard.key_down.space
           )
        advance_level!(args)
      end
    end

    # 当前频率显示
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
    args.outputs.labels << [
      strength_x + strength_bar_w + 10,
      strength_y + 5,
      (locked ? "Signal locked ✔" : "Signal weak"),
      0,
      0,
      (locked ? 120 : 200),
      (locked ? 230 : 180),
      (locked ? 120 : 150)
    ]
  end

  def render_lock_band(args, s)
    rect = s.slider_rect

    # 以 target_freq 为中心，按“容差”绘制半透明高亮带
    target = s.frequency_target
    target_norm = (target - MIN_FREQ) / (MAX_FREQ - MIN_FREQ).to_f
    target_x = rect[:x] + rect[:w] * target_norm

    # 说明：以 TARGET_FREQ 为中心，lock_tolerance(args) 为半宽，在滑块上画半透明带
    tol_norm = lock_tolerance(args) / (MAX_FREQ - MIN_FREQ)
    band_w = rect[:w] * (2 * tol_norm)
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

# Day 6：当锁定后推进到下一关，并刷新与关卡相关的状态
# 当锁定后推进到下一关，并刷新与关卡相关的状态
def advance_level!(args)
  args.state.level_index ||= 0
  args.state.level_index += 1
  args.state.level_index = 0 if args.state.level_index >= LEVELS.length

  s = state(args)
  s.frequency_target = target_freq(args).to_f
  s.slider_value = 0.1
  s.phase = 0.0

  # Day 7：重置关卡进度
  s.locked_frames = 0
  s.level_cleared = false

  # --- DAY 9 CHANGE START: reset elapsed frames ---
  # 新关卡开始时，清零衰减计时
  s.level_elapsed_frames = 0
  # --- DAY 9 CHANGE END ---
end

# --- 小工具 ---
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

def success_hold_frames(args)
  if (ld = level_data(args)) && ld[:success_hold_frames]
    ld[:success_hold_frames].to_i
  else
    60
  end
end

def update_level_progress(args)
  s = state(args)

  freq_diff = (s.wave_freq - s.frequency_target).abs
  tol = lock_tolerance(args)
  frames_needed = success_hold_frames(args)

  # 在锁定区内就累积帧数，离开就清零
  if freq_diff <= tol
    s.locked_frames ||= 0
    s.locked_frames += 1
  else
    s.locked_frames = 0
  end

  # 达到持续帧数要求后判定“本关已解”
  unless s.level_cleared
    if s.locked_frames >= frames_needed
      s.level_cleared = true

      # Day 7：对关卡 1 触发全局标记
      if (ld = level_data(args)) && ld[:id] == 1
        args.state.level1_cleared = true
      end
    end
  end

  # --- DAY 8 CHANGE START: resonance level ---
  # 关卡完成后的后续动作（示例：自动切到下一关）
  if s.level_cleared
    s.post_clear_frames ||= 0
    s.post_clear_frames += 1

    if s.post_clear_frames == 1
      s.clear_banner_timer = 90 # 1.5s 显示“Level Cleared”
    end

    # 显示简单的过关提示
    if s.clear_banner_timer && s.clear_banner_timer > 0
      s.clear_banner_timer -= 1
      args.outputs.labels << [
        640,
        560,
        "Level Cleared! 即将进入下一关…",
        0,
        1,
        180,
        240,
        180
      ]
    end

    # 倒计时结束后切到下一关（若有）
    if s.clear_banner_timer == 0
      next_index = level_index(args) + 1
      if LEVELS[next_index]
        args.state.level_index = next_index
        # 重新初始化：清理当前场景状态并重建音频
        @initialized = false
        init_state(args)
        ensure_audio_started(args)
        s.level_cleared = false
      else
        # 没有下一关了：留在当前关，显示恭喜
        args.outputs.labels << [
          640,
          520,
          "All levels complete. 恭喜通关！",
          0,
          1,
          220,
          240,
          220
        ]
      end
    end
  end
  # --- DAY 8 CHANGE END ---
end
