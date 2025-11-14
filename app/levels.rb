# ================================================================
# 文件：app/levels.rb
# 作用：关卡配置（Day 6 + Day 8）
# ================================================================

LEVELS = [
  # 关卡 1：干涉（Day 7 已实现）
  {
    id: 1,
    type: :interference,
    name: "Interference",
    freq: 4.2, # Hz
    lock_tolerance: 0.08, # 锁定带宽（频率误差容忍）
    success_hold_frames: 60, # 在锁定区连续维持 1 秒（60 fps）
    hint: "Anybody here? 有人在吗？"
  },
  # --- DAY 8 CHANGE START: resonance level ---
  # 关卡 2：共振（本次新增）
  {
    id: 2,
    type: :resonance,
    name: "Resonance",
    freq: 5.6, # 你可以根据音乐/关卡节奏调整
    lock_tolerance: 0.06, # 共振关稍微收紧一点判定
    success_hold_frames: 60, # 连续 1 秒
    glow_threshold: 0.92, # 触发“发光”的接近阈值
    amp_gain_max: 2.2, # 振幅最大放大倍数（相对基础振幅）
    # 可选：若要单独换声音，可在此解开注释并放入你的文件名
    # noise_file: "sounds/resonance_base.wav",
    # tone_file:  "sounds/resonance_clear.wav",
    hint: "When waves align, the world gets louder. 当频率契合，世界变得清晰。"
  },
  # --- DAY 8 CHANGE END ---
  # 关卡 3：衰减/失真（占位；Day 9 再做）
  # 关卡 3：衰减/失真（占位；Day 9 再做）
  # {
  #   id: 3,
  #   type: :attenuation,
  #   name: "Attenuation",
  #   freq: 3.1,
  #   lock_tolerance: 0.08,
  #   success_hold_frames: 60,
  #   hint: "Signal from the past... 信号来自过去…"
  # }
  # --- DAY 9 CHANGE START: Level 3 - Attenuation / Distortion ---
  # 关卡 3：衰减 / 失真
  # 设计要点：
  #   - 开局一段时间信号比较清晰
  #   - 过了若干帧后开始“衰减”：信号变弱、噪声变强、波形发暗并抖动加剧
  #   - 玩家如果“早点调准”，能在更清晰的阶段解出信号
  {
    id: 3,
    type: :attenuation,
    name: "Attenuation",
    # 目标频率：略低于前两关，制造“沉到底部”的感觉
    freq: 3.1,
    # 频率容差与前两关保持一致，难度主要来自时间衰减
    lock_tolerance: 0.08,
    # 需要在锁定带中持续 1 秒
    success_hold_frames: 60,
    # 氛围文案
    hint: "Signal from the past... 信号来自过去…",
    # —— Day 9：衰减相关参数 ——
    # 从进入关卡起，前 90 帧（约 1.5 秒）信号相对稳定
    decay_start_frames: 90,
    # 从开始衰减到“几乎听不清”为止的时长：600 帧 ≈ 10 秒
    decay_duration_frames: 600,
    # 即使几乎完全衰减，仍保留一点“幽灵残响”，避免完全死寂
    min_decay_factor: 0.15
    # 如你有专门的语音/失真音源，可以加上：
    # tone_file:  "sounds/your_past_signal_voice.wav"
    # noise_file: "sounds/your_extra_static.wav"
  }
  # --- DAY 9 CHANGE END ---
]
