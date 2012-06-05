require 'test/unit'
require '../lib/fruit_processor'

class FruitProcessorTest < Test::Unit::TestCase
  @@driver = "fruit_driver_gen.f90"
  @@basket = "fruit_basket_gen.f90"
  @@generated = [@@basket, @@driver]

  def setup
    @fixture = FruitProcessor.new
  end

  def test_init
    @fixture.load_files "."
    @fixture.fruit_picker
    @fixture.create_driver
  end

#  def test_get_spec_hash
#    got_hash = @fixture.get_spec_hash("calculator_test.f90")
#  end

  def test_load_files
    @fixture.load_files "."

    files = @fixture.get_files
    assert_equal(1, files.grep(/calculator_test\.f90$/).length, "detect .f90 files") 
    assert_equal(1, files.grep(/myvector_test\.f03$/).length, "detect .f03 files") 
  end

  def test_test_module_name_from_file_path
    result = @fixture.test_module_name_from_file_path("./abc/def/ghi_jk.f90")
    assert_equal("ghi_jk", result)

    result = @fixture.test_module_name_from_file_path("mmm/nnn/ppp_qqq.f03")
    assert_equal("ppp_qqq", result)
  end
  
  def test_create_driver
    if File.exists?(@@driver)
      File.delete(@@driver)
    end
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
    @@generated.each{|f|
      if File.exists?(f)
        File.delete(f)
      end
    }

    test_init

    @@generated.each{|f|
      assert_equal(true, File.exists?(f))
    }
  end

  def test_gather_specs
    test_init

    sub_names = @fixture.get_methods_of_filename("./calculator_test.f90")
    assert_equal(
      "test_calculator_should_produce_4_when_2_and_2_are_inputs", sub_names[0]
    )
    assert_equal("test_more_with_spec_in_spec_variable", sub_names[1])
    assert_equal("test_calculator_should_remember_previous_calculation_results", sub_names[2])
    assert_equal("test_calculator_should_reset_when_reset_is_called", sub_names[3])

    sub_names = @fixture.get_methods_of_filename("./myvector_test.f03")
    assert_equal("test_init", sub_names[0])
    assert_equal("test_always_ok", sub_names[1])
    assert_equal("test_always_two_fails_one_success", sub_names[2])
    assert_equal("test_all_elems", sub_names[3])
  end

#  def test_generate_spec
#    assert_equal(0, @fixture.get_specs.size)
#  end

end
