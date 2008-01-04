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

  def test_generate_spec
    p @fixture.get_specs.size
    assert_equal (4, @fixture.get_specs.size)
  end

end
