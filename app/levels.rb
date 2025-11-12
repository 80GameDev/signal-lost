# ================================================================
# 文件：app/levels.rb
# 作用：关卡配置（Day 6）
# ================================================================

LEVELS = [
  # 关卡 1：干涉（“Level 1 是干涉波关卡”）
  {
    id: 1,
    name: "Interference",
    freq: 4.2, # Hz（与 Day 5 的默认 TARGET_FREQ 对齐）
    lock_tolerance: 0.08, # 与现有 UI 提示阈值一致
    success_hold_frames: 60, # 在锁定区连续维持 1 秒（60fps）即可过关
    hint: "Anybody here? 有人在吗？"
  },
  # 关卡 2：共振（占位；Day 8 扩展）
  {
    id: 2,
    name: "Resonance",
    freq: 6.5,
    lock_tolerance: 0.08,
    success_hold_frames: 60,
    hint: "Sounds like ... myself. 听起来像…我自己。"
  },
  # 关卡 3：衰减/失真（占位；Day 9 扩展）
  {
    id: 3,
    name: "Attenuation",
    freq: 3.1,
    lock_tolerance: 0.08,
    success_hold_frames: 60,
    hint: "Signal from the past... 信号来自过去…"
  }
]
