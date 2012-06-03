require 'test/unit'
require '../lib/fruit_processor'

class FruitProcessorTest < Test::Unit::TestCase
  @@driver = "fruit_driver_gen.f90"
  @@basket = "fruit_basket_gen.f90"
  @@generated = [@@basket, @@driver]

  def setup
    @fixture = FruitProcessor.new

    @@generated.each{|f|
      if File.exists?(f)
        File.delete(f)
      end
    }
  end

#  def test_init
#    @fixture.load_files "."
#    @fixture.fruit_picker
#    @fixture.create_driver
#  end
  
  def test_create_driver
    @@generated.each{|f|
      if File.exists?(f)
        File.delete(f)
      end
    }
    @fixture.create_driver
    assert_equal(true, File.exists?(@@driver))
    File::open(@@driver){|f|
      assert_equal("program fruit_driver_gen", f.gets.chomp!)
      assert_equal(0, /^\s*use\s+fruit\s*$/ =~ f.gets.chomp!)
      assert_equal(0, /^\s*use\s+fruit_basket_gen\s*$/ =~ f.gets.chomp!)
      assert_equal(0, /^\s*call\s+init_fruit\s*$/ =~ f.gets.chomp!)
    }
  end

  def test_file_generated
    @fixture.load_files "."
    @fixture.fruit_picker
    @fixture.create_driver

    @@generated.each{|f|
      assert_equal(true, File.exists?(f))
    }
  end

#  def test_generate_spec
#    assert_equal(0, @fixture.get_specs.size)
#  end

end
