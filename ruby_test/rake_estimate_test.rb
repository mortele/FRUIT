#!/usr/bin/env ruby

require 'test/unit'
require '../rake_estimate'
require 'rubygems'
require 'fruit_processor'

class FruitRakeEstimateTest < Test::Unit::TestCase

  def test_parse_sharp_line_stack
    esti = FruitRakeEstimate.new

    macro_stack = []

    macro_stack = esti.parse_sharp_line_stack("#ifdef ABC", macro_stack)
    assert_equal(["ABC"], macro_stack)

    macro_stack = esti.parse_sharp_line_stack("#ifdef DEF", macro_stack)
    assert_equal(["ABC", "DEF"], macro_stack)

    macro_stack = esti.parse_sharp_line_stack("#else", macro_stack)
    assert_equal(["ABC", "#not#DEF"], macro_stack)

    macro_stack = esti.parse_sharp_line_stack("#endif", macro_stack)
    assert_equal(["ABC"], macro_stack)

    macro_stack = esti.parse_sharp_line_stack("#endif", macro_stack)
    assert_equal([], macro_stack)
  end

  def test_if_macro_stack
    esti = FruitRakeEstimate.new

    macro_stack = []
    esti.identifiers = []
    assert_equal(true, esti.if_macro_stack(macro_stack))

    macro_stack = ["ABC"]
    esti.identifiers = ["ABC"]
    assert_equal(true, esti.if_macro_stack(macro_stack))

    macro_stack = ["ABC"]
    esti.identifiers = ["DEF"]
    assert_equal(false, esti.if_macro_stack(macro_stack))

    macro_stack = ["#not#ABC"]
    esti.identifiers = ["DEF"]
    assert_equal(true, esti.if_macro_stack(macro_stack))

    macro_stack = ["#not#ABC"]
    esti.identifiers = []
    assert_equal(true, esti.if_macro_stack(macro_stack))

    macro_stack = ["#not#ABC"]
    esti.identifiers = ["ABC"]
    assert_equal(false, esti.if_macro_stack(macro_stack))

    macro_stack = ["ABC", "DEF"]
    esti.identifiers = ["DEF"]
    assert_equal(false, esti.if_macro_stack(macro_stack))

    esti.identifiers = ["DEF", "ABC"]
    assert_equal(true, esti.if_macro_stack(macro_stack))
  end


 def test_set_forward_nested_ifdef
   esti = FruitRakeEstimate.new
   esti.identifiers = ["OUTER", "INNER"]
   forward = esti.set_forward
   assert_equal(["mod_1.f90", "mod_2.f90"].sort, forward["nested_ifdef.f90"].sort)

   esti = FruitRakeEstimate.new
   esti.identifiers = ["OUTER"]
   forward = esti.set_forward
   assert_equal(["mod_1.f90"], forward["nested_ifdef.f90"])

   esti = FruitRakeEstimate.new
   esti.identifiers = ["INNER"]
   forward = esti.set_forward
   assert_equal([], forward["nested_ifdef.f90"])

   esti = FruitRakeEstimate.new
   esti.identifiers = []
   forward = esti.set_forward
   assert_equal([], forward["nested_ifdef.f90"])
 end

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

  def test_set_forward2
    esti = FruitRakeEstimate.new
    forward = esti.set_forward
    assert_equal([], forward["need_itself.f90"])
    assert_equal(["need_itself.f90"], forward["main_for_need_itself.f90"])
  end

  def test_set_forward_macro
    esti = FruitRakeEstimate.new
    forward = esti.set_forward
    assert_equal([], forward["using_macro.f90"])
    assert_equal(["sample08.f08"], forward["using_macro_else.f90"])
  end

  def test_set_forward_macro_set
    esti = FruitRakeEstimate.new
    esti.identifiers = ["USE_TEST_MOD"]
    forward = esti.set_forward
    assert_equal(["test_mod.f03"], forward["using_macro.f90"])
    assert_equal(["test_mod.f03"], forward["using_macro_else.f90"])
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

  def test_get_needed2
    esti = FruitRakeEstimate.new
    assert_equal(
      ["need_itself.f90"],
      esti.get_needed(["need_itself.f90"])
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

