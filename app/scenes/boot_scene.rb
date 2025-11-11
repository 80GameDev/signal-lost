# ================================================================
# 文件路径：app/scenes/boot_scene.rb
# 场景名称：BootScene（启动场景 / 波形原型）
#
# 项目阶段说明：
#   Day 2: 实现基础波形（正弦波）渲染，并支持 ← → 键调节频率。
#   Day 3: 增加交互式“调频滑块”（frequency slider），
#          允许玩家通过鼠标点击改变波形频率。
#
# 主要目标：
#   - 展示动态“无线电波”视觉效果；
#   - 实现简单的信号调频机制；
#   - 为后续剧情或解谜玩法提供基础的波形调控系统。
# ================================================================

class BootScene
  # tick(args) 是 DragonRuby 中的主循环入口，
  # 每帧都会被自动调用（约 60 FPS）
  def tick(args)
    defaults(args) # 初始化状态（首次进入或重置时）
    handle_input(args) # 处理玩家输入（键盘/鼠标）
    update_derived_state(args) # 更新衍生状态（例如目标频率同步）
    render(args) # 绘制所有图像和 UI
  end

  # ---------------------------------------------------------------
  # 一、状态初始化（defaults）
  # ---------------------------------------------------------------
  def defaults(args)
    s = args.state # 访问全局状态对象（DragonRuby 内置）

    # 设置频率范围（允许的最小和最大值）
    s.min_freq ||= 1.0
    s.max_freq ||= 8.0

    # 当前正弦波的频率（主要物理参数）
    s.wave_freq ||= 3.0

    # Day 3: frequency_target 是逻辑层的“目标频率”，
    # 可被其他系统（比如调谐、解谜模块）读取。
    s.frequency_target ||= s.wave_freq

    # 滑块的屏幕区域定义（矩形坐标）
    # x, y 为左下角坐标；w, h 为宽高。
    s.slider_rect ||= { x: 200, y: 80, w: 880, h: 24 }

    # slider_value 表示滑块的“归一化位置”（0.0 ~ 1.0）
    # 对应 min_freq → max_freq 的线性映射。
    if s.slider_value.nil?
      range = (s.max_freq - s.min_freq)
      range = 1.0 if range.zero? # 防止除以 0
      s.slider_value = (s.wave_freq - s.min_freq) / range
    end
  end

  # ---------------------------------------------------------------
  # 二、输入处理（handle_input）
  # ---------------------------------------------------------------
  def handle_input(args)
    handle_keyboard_input(args) # 键盘 ← → 调节
    handle_slider_input(args) # 鼠标点击滑块调节
  end

  # 键盘输入逻辑（Day 2 版本兼容）
  def handle_keyboard_input(args)
    s = args.state
    step = 0.05 # 每次调整的步长

    # 通过方向键改变频率
    if args.inputs.left
      s.wave_freq -= step
    elsif args.inputs.right
      s.wave_freq += step
    end

    # 限制在 [min_freq, max_freq] 范围内
    s.wave_freq = s.min_freq if s.wave_freq < s.min_freq
    s.wave_freq = s.max_freq if s.wave_freq > s.max_freq

    # 同步 slider_value，使滑块位置反映最新频率
    range = (s.max_freq - s.min_freq)
    range = 1.0 if range.zero?
    s.slider_value = (s.wave_freq - s.min_freq) / range
  end

  # 鼠标点击滑块：根据点击位置更新频率
  def handle_slider_input(args)
    s = args.state
    rect = s.slider_rect
    click = args.inputs.mouse.click
    return unless click # 没有点击时直接返回

    # DragonRuby 内置方法：判断鼠标点击是否落在矩形区域内
    if click.point.inside_rect?(rect)
      # 计算点击点相对滑轨左端的距离
      rel_x = click.point.x - rect[:x]
      rel_x = 0 if rel_x < 0
      rel_x = rect[:w] if rel_x > rect[:w]

      # 将点击位置映射为归一化比例（0.0 ~ 1.0）
      s.slider_value = rel_x / rect[:w].to_f

      # 将比例反映为真实频率值
      range = (s.max_freq - s.min_freq)
      range = 1.0 if range.zero?
      s.wave_freq = s.min_freq + s.slider_value * range
    end
  end

  # ---------------------------------------------------------------
  # 三、衍生状态同步
  # ---------------------------------------------------------------
  # 用于保证 UI 和物理逻辑数据一致，
  # 例如：将 wave_freq 推送到 frequency_target。
  def update_derived_state(args)
    s = args.state
    s.frequency_target = s.wave_freq
  end

  # ---------------------------------------------------------------
  # 四、渲染系统（render）
  # ---------------------------------------------------------------
  def render(args)
    render_background(args) # 背景与标题
    render_wave(args) # 主波形
    render_ui(args) # 滑块与文字提示
  end

  # 背景渲染（深色基调 + 居中标题）
  def render_background(args)
    # 深蓝背景（RGB 6,12,24）
    args.outputs.solids << [0, 0, 1280, 720, 6, 12, 24]

    # 居中标题标签
    args.outputs.labels << [
      640,
      700, # 坐标（中心上方）
      "Signal Lost / 孤波  -  Boot Prototype", # 文本内容
      1,
      0, # 对齐方式与字号枚举
      200,
      220,
      255 # 字体颜色（淡蓝）
    ]
  end

  # 波形渲染逻辑（核心视觉效果）
  def render_wave(args)
    s = args.state
    lines = args.outputs.lines # DragonRuby 的线段输出集合

    # 初始化波相位，用于让波形“流动”起来
    s.wave_phase ||= 0.0
    s.wave_phase += s.wave_speed || 0.08

    # 屏幕参数
    screen_w = 1280
    center_y = 360 # 波形中心线
    amplitude = 80 # 波幅
    step_x = 4 # 每个线段的水平长度（越小越平滑）

    # 波形颜色：亮蓝色
    color = [120, 200, 255, 255]

    x = 0
    while x < screen_w
      # 计算相邻两点的相位值
      rad1 = s.wave_phase + x * s.wave_freq * 0.01
      rad2 = s.wave_phase + (x + step_x) * s.wave_freq * 0.01

      # 根据正弦函数计算纵坐标
      y1 = center_y + Math.sin(rad1) * amplitude
      y2 = center_y + Math.sin(rad2) * amplitude

      # 绘制线段（从 [x, y1] 到 [x+step_x, y2]）
      lines << [x, y1, x + step_x, y2, *color]
      x += step_x
    end

    # 辅助中线（便于观察波动）
    args.outputs.lines << [0, center_y, screen_w, center_y, 40, 80, 120, 180]
  end

  # Day 3: UI 渲染，包括滑轨、滑块和文本提示
  def render_ui(args)
    s = args.state
    rect = s.slider_rect

    # --- 绘制滑轨 ---
    args.outputs.solids << [
      rect[:x],
      rect[:y],
      rect[:w],
      rect[:h],
      20,
      30,
      50,
      220 # 深蓝半透明
    ]

    args.outputs.borders << [
      rect[:x],
      rect[:y],
      rect[:w],
      rect[:h],
      180,
      200,
      230,
      255 # 浅蓝描边
    ]

    # --- 绘制滑块 ---
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
      255 # 亮色滑块
    ]

    # --- 文本提示 ---
    # 操作提示（键盘 + 鼠标）
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

    # 当前频率显示（实时数值）
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
