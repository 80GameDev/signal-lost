# app/main.rb
#
# 《Signal Lost / 孤波》
# Day 1–3: 场景管理 + 启动场景

$gtk.require "app/scenes/boot_scene.rb"

class SceneManager
  def initialize
    # 目前只存在一个启动场景，后面可以在这里切换到 menu / radio 等其它场景
    @current_scene = BootScene.new
  end

  def tick(args)
    @current_scene.tick(args)
  end
end

def tick(args)
  # 初始化全局场景管理器
  args.state.scene_manager ||= SceneManager.new

  # 把每一帧的更新交给当前场景
  args.state.scene_manager.tick(args)
end
