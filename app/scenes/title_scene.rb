# frozen_string_literal: true

class TitleScene
  def initialize; end

  def tick args
    args.outputs.labels << [10, 700, "Signal Lost", 5]
    args.outputs.labels << [10, 660, "Press any key to start", 2]
  end
end
