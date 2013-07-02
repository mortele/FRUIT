#!/usr/bin/env ruby

require 'test/unit'
require 'rubygems'
require 'rake'
include Rake::DSL if defined?(Rake::DSL)

require '../rake_base'
require '../rake_base_deps'

class FruitRakeBaseDepsTest < Test::Unit::TestCase
  def test_variable

    assert_equal("", $goal, "default $goal is empty string")
    assert($base_dir,     "$base_dir defined")
    assert($build_dir,    "$build_dir defined")

    assert_equal("", $source_dir)

    if ($ext_obj == "obj")
      assert(RakeBaseDeps::OBJ.index("sample08.obj"), "OBJ has sample08.obj")
    else
      assert(RakeBaseDeps::OBJ.index("sample08.o"), "OBJ has sample08.o")
    end
  end

  def test_opt_of_build_dir
    opt = RakeBaseDeps.opt_of_build_dir("some_build_dir")
    assert_equal(opt, '"-Isome_build_dir"')

    opt = RakeBaseDeps.opt_of_build_dir("./")
    assert_equal(opt, "")

    opt = RakeBaseDeps.opt_of_build_dir("")
    assert_equal(opt, "")
  end

  def test_conv_dosish
    (tgt, src) = RakeBaseDeps.conv_dosish("abc/def", "ghi/jkl", true)
    assert_equal(tgt, "abc\\def", "slash -> backslash")

    (tgt, src) = RakeBaseDeps.conv_dosish("abc/def", "ghi/jkl", false)
    assert_equal(tgt, "abc/def", "not slash -> backslash")
    assert_equal(src, "ghi/jkl", "not slash -> backslash")
  end
end


