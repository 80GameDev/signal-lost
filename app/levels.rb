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
  {
    id: 3,
    type: :attenuation,
    name: "Attenuation",
    freq: 3.1,
    lock_tolerance: 0.08,
    success_hold_frames: 60,
    hint: "Signal from the past... 信号来自过去…"
  }
]
