#!/usr/bin/env ruby

require 'test/unit'
require '../rake_estimate'
require 'rubygems'
require 'fruit_processor'

class FruitRakeEstimateTest < Test::Unit::TestCase


  def test_estim_external_func
    dir = "external_func/"
    esti = FruitRakeEstimate.new
    esti.source_dirs = [dir, ]
    esti.set_all_f

    assert(esti.all_f.include?(dir + "ext_func_test.f90"), 
      "should include #{dir}ext_func_test.f90")
    assert(esti.all_f.include?(dir + "src_with_ext_func.f90"), 
      "should include src_with_ext_func.f90")
    assert(esti.all_f.include?(dir + "src_add_two.f90"), 
      "should include src_add_two.f90"      )

    forward = esti.set_forward
    assert_equal(
             ["src_with_ext_func.f90", "src_add_two.f90"].sort, 
      forward["ext_func_test.f90"].sort, 
      "ext_func_test.f90 should need src_with_ext_func.f90 and src_add_two.f90."
    )
  end


  def test_estim_external_func2
    dir = "external_func/"
    esti = FruitRakeEstimate.new
    esti.source_dirs = [dir, ]
    esti.set_all_f

    assert_equal(
      [
        "src_with_ext_func.f90", 
        "src_add_two.f90", 
        "ext_func_test.f90", 
      ].sort,
      esti.get_needed(["ext_func_test.f90"]).sort
    )
  end


  def test_estim_external
    dir = "rake_estim_external/"
    esti = FruitRakeEstimate.new
    esti.source_dirs = [dir, ]
    esti.set_all_f

    assert(esti.all_f.include?(dir + "file_with_ext_sub_test.f90"), 
      "should include #{dir}file_with_ext_sub.f90")
    assert(esti.all_f.include?(dir + "file_with_ext_sub.f90"), 
      "should include file_with_ext_sub.f90")
    assert(esti.all_f.include?(dir + "some_module.f90"), 
      "should include file_with_ext_sub.f90")

    forward = esti.set_forward
    assert_equal(
             ["some_module.f90"], 
      forward["file_with_ext_sub.f90"], 
      "file_with_ext_sub.f90 needs some_module.f90"
    )

    assert_equal(
             ["file_with_ext_sub.f90", "abc_module.f90"].sort, 
      forward["file_with_ext_sub_test.f90"].sort, 
      "file_with_ext_sub_test.f90 needs file_with_ext_sub.f90"
    )

    assert_equal(
      [
        "abc_module.f90", 
        "file_with_ext_sub.f90", 
        "file_with_ext_sub_test.f90", 
        "some_module.f90"
      ].sort,
      esti.get_needed(["file_with_ext_sub_test.f90"]).sort
    )

  end


  def test_get_ordered__circular
    esti = FruitRakeEstimate.new

    needed = [
      "ext_subs_test.f90",
      "ext_sub_1.f90",
      "ext_sub_2.f90",
    ]

    esti.forward = {
      'ext_subs_test.f90' => ['ext_sub_1.f90'], 
      'ext_sub_2.f90' => ['ext_sub_1.f90'], 
      'ext_sub_1.f90' => ['ext_sub_2.f90'], 
    }

    ordered = esti.get_ordered(needed)

    assert_equal(3, ordered.size)
    assert(ordered.include?("ext_subs_test.f90"))
    assert(ordered.include?("ext_sub_1.f90"))
    assert(ordered.include?("ext_sub_2.f90"))
  end

end

