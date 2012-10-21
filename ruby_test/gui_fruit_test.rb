#!/usr/bin/env ruby

require 'test/unit'
require '../gui_fruit'

class DummyWindow
  
  def add_text string
  # p string
  end
  def add_text_warn string
  #p string
  end

  def btn_source_dir_setup
  end
  def disable_btn_source_dir
  end
  def enable_btn_source_dir
  end

  def btn_open_dir_setup
  end
  def btn_open_dir_activate
  end
  def btn_fruitf90_setup
  end

  def choose_dir
    return "./test dir"
  end

  def show_dirname_of_tester(dir)
  # puts dir
  end

  def add_one_testf90 filename
  # puts "  adding tester " + filename
    return "dummy_check"
  end
 
  def enable_fruitf90_select
  end
  def disable_fruit_run
  end
  def set_btn_clear_and_run a, b
  end
end

class GuiCoreTest < Test::Unit::TestCase
  def test_fetch_missing_mod
    core = GuiCore.new
    window = DummyWindow.new
    core.initial(window)

    tester_dir = "./dir_only_tester"
    source_dir = "./dir only tested"

    #p Dir::glob(tester_dir + "/*")
    #p Dir::glob(source_dir + "/*")

    File.open(source_dir + "/aaa.mod", "w"){|f|
      f.write "some dummy text\n"
    }

    if File.symlink?(tester_dir + "/aaa.mod")
       File.unlink(tester_dir + "/aaa.mod")
    end

    assert(!File.symlink?(tester_dir + "/aaa.mod"), "aaa.mod remove")
    core.fetch_missing_mod(tester_dir, source_dir)
    assert( File.symlink?(tester_dir + "/aaa.mod"), "aaa.mod not exist")

    if File.symlink?(tester_dir + "/aaa.mod")
       File.unlink(tester_dir + "/aaa.mod")
    end

    tester_dir = "./dir_only_tester"
    source_dir = "./dir_without_mod"

    assert(!File.symlink?(tester_dir + "/aaa.mod"), "aaa.mod missing")
    ret = core.fetch_missing_mod(tester_dir, source_dir)
    assert(!ret, "fetch missing mod should fail")
    assert(!File.symlink?(tester_dir + "/aaa.mod"), "aaa.mod still missing")
  end

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

  def test_reload_all_f_source_dir
    core = GuiCore.new
    window = DummyWindow.new
    core.initial(window)
    
    dir = "./dir only tested"
    ["fruit_baket_gen.f90", "fruit_driver_gen.f90"].each{|f|
      dirf =  dir + "/" + f
      File.unlink dirf if File.exist? dirf 
    }
    all_f = core.reload_all_f("./dir only tested")
    assert_equal([], all_f)

    ["fruit_baket_gen.f90", "fruit_driver_gen.f90"].each{|f|
      dirf =  dir + "/" + f
      assert(!File.exists?(dirf), f + " deleted")
    }


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

  def test_if_source_exist
    core = GuiCore.new
    window = DummyWindow.new
    core.initial(window)
    assert( core.if_source_exist("./dir_tester_and_tested"))
    assert(!core.if_source_exist("./dir_only_tester"      ))
    assert( core.if_source_exist("./dir_tester_and_tested2"), "dir_tester_and_tested2")
    assert( core.if_source_exist("./dir_tester_and_tested3"), "true when only fruit module is missing")
  end

  def test_writeout_rakefile_source
    core = GuiCore.new
    window = DummyWindow.new
    core.initial(window)

    build_dir = "./dir_only_tester"

    dir = "dir only tested"
    name = "rakefile_gui_tmp" 
    target = dir + "/" + name
    File.unlink target if File.exist? target

    assert(!File.exists?(target), "rakefile deleted")
    core.writeout_rakefile_source(dir, name, build_dir)
    assert( File.exists?(target), "rakefile generated")

    line = File.readlines(target).grep(/#{build_dir}/)
    assert(line.size > 0, "build dir seen in rakefile")
  end
end
