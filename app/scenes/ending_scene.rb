# frozen_string_literal: true

class EndingScene
  def initialize; end

  def tick args
    args.outputs.labels << [10, 700, "Ending", 5]
    args.outputs.labels << [10, 660, "Thank you for playing.", 2]
  end
end
