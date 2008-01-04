require 'test/unit'
require '../lib/fruit_processor'

class FruitProcessorTest < Test::Unit::TestCase
  def setup
    @fixture = FruitProcessor.new
  end

  def test_init
    @fixture.create_module
    @fixture.create_driver
  end

  def test_generate_spec
    assert_equal (0, @fixture.get_specs.size)
  end

end
