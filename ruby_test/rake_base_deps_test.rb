#!/usr/bin/env ruby

require 'test/unit'
require 'rubygems'
require 'rake'
include Rake::DSL if defined?(Rake::DSL)


class FruitRakeBaseDepsTest < Test::Unit::TestCase
  def test_variable

    require '../rake_base'
    require '../rake_base_deps'

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
end


