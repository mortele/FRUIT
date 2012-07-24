#!/usr/bin/env ruby

require 'test/unit'
require '../rake_estimate'
require 'rubygems'
require 'fruit_processor'

class FruitRakeEstimateTest < Test::Unit::TestCase
  def test_set_all_f
    esti = FruitRakeEstimate.new
    esti.set_all_f
    assert(esti.all_f.include?("sample08.f08"))
    assert(esti.all_f.include?("test_mod.f03"))
    assert(esti.all_f.include?("main.f90"))
    assert(!esti.all_f.include?("ignored.f78"), "ignored.f78 must be ignored")
  end

  def test_set_forward
    esti = FruitRakeEstimate.new
    forward = esti.set_forward
    assert_equal(["sample08.f08"], forward["test_mod.f03"])
    assert_equal([], forward["sample08.f08"])
    assert_equal(["test_mod.f03"], forward["main.f90"])
  end

  def test_f_to_o
    result = FruitRakeEstimate.new.f_to_o("abcdef.f03")
    assert_equal("abcdef.o", result)
  end

  def test_get_needed
    esti = FruitRakeEstimate.new
    assert_equal(
      ["test_mod.f03", "sample08.f08"].sort, 
      esti.get_needed(["test_mod.f03"]).sort
    )
    assert_equal(
      ["test_mod.f03", "sample08.f08", "main.f90"].sort, 
      esti.get_needed(["main.f90"]).sort
    )
  end

  def test_get_ordered
    esti = FruitRakeEstimate.new
    needed = esti.get_needed(["main.f90"])
    ordered = esti.get_ordered(needed)
    assert_equal(
      ["sample08.f08", "test_mod.f03", "main.f90"], 
      ordered)
  end

  def test_src_and_obj_for_main
    esti = FruitRakeEstimate.new
    src, obj = esti.src_and_obj_for_main("main.f90")

    assert_equal(
      ["sample08.f08", "test_mod.f03", "main.f90"], 
      src)
    assert_equal(
      ["sample08.o", "test_mod.o", "main.o"], 
      obj)
  end
end

