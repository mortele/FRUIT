require 'test/unit'
require 'fruit_processor'

class FruitProcessorTest < Test::Unit::TestCase
  def setup
    @fixture = FruitProcessor.new
  end

  def test_init
    @fixture.create_module
    @fixture.create_driver
  end

end
