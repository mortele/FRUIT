#!/usr/bin/env ruby

require 'test/unit'
require '../gui_fruit'

class DummyWindow
  
  def add_text string
  # p string
  end
  def btn_open_dir_setup
  # puts __method__
  end
  def btn_open_dir_activate
  # puts __method__
  end
  def btn_fruitf90_setup
  # puts __method__
  end

  def choose_dir
  # puts __method__
    return "./test dir"
  end

  def show_dirname_of_tester(dir)
  # puts __method__
  # puts dir
  end

  def add_one_testf90 filename
  # puts __method__
  # puts "  adding tester " + filename
    return "dummy_check"
  end
 
  def enable_fruitf90_select
  # puts __method__
  end
  def disable_fruit_run
  # puts __method__
  end
  def set_btn_clear_and_run a, b
  # puts __method__
  end
end

class GuiCoreTest < Test::Unit::TestCase
  def test_hello_world
    core = GuiCore.new

    window = DummyWindow.new
    core.initial(window)

    assert_equal(
      File.expand_path(File.dirname("../gui_fruit.rb")), 
      core.show_dir_rake_base
    )
    
  end

  def test_open_dir_and_update
    core = GuiCore.new
    window = DummyWindow.new
    core.initial(window)

    ret = core.open_dir_and_update
    assert_equal("./test dir", ret)
  end

  def test_reload_all_f
    core = GuiCore.new
    window = DummyWindow.new
    core.initial(window)

    all_f = core.reload_all_f("./test dir")
    assert_equal(["./abc_test.f90"], all_f)
  end

  def test_if_ready_fruitf90
    core = GuiCore.new
    window = DummyWindow.new
    core.initial(window)

    core.reload_all_f("./test dir with fruitf90")
    assert(core.if_ready_fruitf90, "fruit.f90 and fruit_util.f90 exists")
  end

  def test_if_ready_fruitf90_nof90dir
    core = GuiCore.new
    window = DummyWindow.new
    core.initial(window)
    core.choose_fruitf90dir("./test dir")
    assert_equal(false, core.if_ready_fruitf90)
    core.choose_fruitf90dir("./test dir with fruitf90")
    assert_equal("./test dir with fruitf90", core.if_ready_fruitf90)
  end
end
