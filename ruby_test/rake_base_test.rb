#!/usr/bin/env ruby

require 'test/unit'
require 'rubygems'
require 'rake'
include Rake::DSL if defined?(Rake::DSL)


class FruitRakeBaseTest < Test::Unit::TestCase
  def test_variable

    require '../rake_base'

    assert_equal("", $goal, "default $goal is empty string")
    assert($base_dir,     "$base_dir defined")
    assert($build_dir,    "$build_dir defined")

    assert_equal("", $source_dir)
    assert_equal("", $obj_dir)

    assert(RakeBase::OBJ.index("sample08.o"), "OBJ has sample08.o")
  end
end


