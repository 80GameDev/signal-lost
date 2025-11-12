# ================================================================
# 文件路径：app/main.rb
# 项目名称：《Signal Lost / 孤波》
#
# 阶段说明：
#   Day 1–3:
#     - 建立最基本的“场景管理系统（SceneManager）”
#     - 接入启动场景（BootScene），实现游戏循环的最小可运行单元
#     - 在 BootScene 中渲染基础波形与调频滑块
#   Day 4:
#     - 在 BootScene 中加入简易音频系统
#     - 根据当前频率与目标频率的偏差，动态调整噪声和信号音量
#
# DragonRuby 框架机制简介：
#   - DragonRuby 游戏的主循环函数必须命名为 tick(args)
#   - 每一帧都会自动调用 tick，并传入 args（全局上下文）
#   - 我们通常把游戏逻辑组织为多个“场景”（Scene），
#     每个场景负责自己的初始化、输入处理、状态更新、渲染。
#
# 本文件职责：
#   - 引入 BootScene（启动场景）
#   - 实现 SceneManager（场景管理器）
#   - 定义 DragonRuby 全局 tick(args) 并把控制权交给当前场景
# ================================================================

# ---------------------------------------------------------------
# 一、引入各个场景
# ---------------------------------------------------------------
require "app/levels"
require "app/scenes/boot_scene"

# ---------------------------------------------------------------
# 二、场景管理器：负责当前场景的切换与调度
# ---------------------------------------------------------------
class SceneManager
  attr_reader :current_scene_name, :current_scene

  def initialize
    # 目前只需要一个场景：BootScene
    @current_scene_name = :boot
    @current_scene = BootScene.new
  end

  # 可选：对外暴露一个切换场景的接口
  def switch_to(scene_name)
    case scene_name
    when :boot
      @current_scene = BootScene.new
      @current_scene_name = :boot
    else
      # 将来可以在这里添加更多场景
      puts "[SceneManager] unknown scene: #{scene_name}"
    end
  end

  # 每一帧由 main.rb 调用
  def tick(args)
    # 把 args 传入当前场景，由场景自行处理输入、更新和渲染
    @current_scene.tick(args)
  end
end

# ---------------------------------------------------------------
# 三、全局 tick(args)
# ---------------------------------------------------------------
# DragonRuby 主循环入口函数（类似 Unity 的 Update 或 Godot 的 _process）
# 每一帧都会被引擎自动调用。
def tick(args)
  # 初始化全局的场景管理器
  # args.state 是全局状态存储区（整个游戏生命周期内保持）
  args.state.scene_manager ||= SceneManager.new

  # 把本帧逻辑交给场景管理器，
  # 再由场景管理器调用当前场景的 tick(args)。
  args.state.scene_manager.tick(args)
end
