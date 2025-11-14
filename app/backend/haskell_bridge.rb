# frozen_string_literal: true

require "open3"
require "json"

# 调用 Haskell 可执行文件、管理 JSON 的桥接层
class HaskellBridge
  def initialize(executable_path: "core/.stack-work/dist/.../build/core-app/core-app")
    @executable_path = executable_path
  end

  def call_engine(payload)
    json_in = JSON.generate(payload)
    stdout, stderr, status =
      Open3.capture3(@executable_path, stdin_data: json_in)

    unless status.success?
      warn "Haskell engine error: #{stderr}"
      return { "ok" => false, "state" => "{}" }
    end

    JSON.parse(stdout)
  rescue StandardError => e
    warn "HaskellBridge exception: #{e}"
    { "ok" => false, "state" => "{}" }
  end
end
