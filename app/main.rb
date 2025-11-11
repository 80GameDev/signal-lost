# app/main.rb
#
# 《Signal Lost / 孤波》
# Day 1: 初始化项目结构 + 跑起来一个简单画面

# 载入启动场景（目前只有一个 boot 场景，后面会增加 radio 场景、menu 场景等）
$gtk.require "app/scenes/boot_scene.rb"

class SceneManager
  def initialize
    @current_scene_symbol = :boot
    @scenes = {}
  end

  def current_scene(_args)
    # 延迟初始化场景，避免一次性 new 太多对象
    @scenes[@current_scene_symbol] ||= case @current_scene_symbol
    when :boot
      BootScene.new
    else
      BootScene.new # fallback
    end
  end

  def tick(args)
    current_scene(args).tick(args)
  end

  # 预留接口：以后可以在关卡之间切换场景
  def switch_to(scene_symbol)
    @current_scene_symbol = scene_symbol
  end
end

def tick(args)
  # 初始化全局状态中的场景管理器
  args.state.scene_manager ||= SceneManager.new

  # 调用当前场景的 tick
  args.state.scene_manager.tick(args)
end
