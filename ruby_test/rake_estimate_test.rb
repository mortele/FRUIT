#!/usr/bin/env ruby

require 'test/unit'
require '../rake_estimate'
require 'rubygems'
require 'fruit_processor'

class FruitRakeEstimateTest < Test::Unit::TestCase

  def test_missing_modules
    FileUtils.cd("dir_only_tester"){
      esti = FruitRakeEstimate.new
      missings = esti.missing_modules
      assert_equal(["aaa"], missings, "missing module is aaa")
    }
    FileUtils.cd("dir_tester_and_tested2"){
      esti = FruitRakeEstimate.new
      missings = esti.missing_modules
      assert_equal([], missings, "All used module exist")
    }
    FileUtils.cd("dir_tester_and_tested3"){
      esti = FruitRakeEstimate.new
      missings = esti.missing_modules
      assert_equal(["fruit"], missings, "All used module exist")
    }
  end

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

  def test_set_forward3_lacking
    FileUtils.cd("dir_only_tester"){
      esti = FruitRakeEstimate.new
      forward = esti.set_forward
      assert_equal([],  forward["test_aaa.f90"],
        "test_aaa.f90 needs module aaa. but no source for module aaa there."
      )
    }
    FileUtils.cd("dir_tester_and_tested2"){
      esti = FruitRakeEstimate.new
      forward = esti.set_forward
      assert_equal(["some_name.f90"],  forward["some_test.f90"],
        "some_test.f90 needs module 'some_name_2' provided by 'some_name.f90"
      )
    }
  end

  def test_set_forward_crlf_and_cr
    esti = FruitRakeEstimate.new
    forward = esti.set_forward
    assert(forward["needs_mod_3.f90"].include?("mod_3_crlf.f90"), "needs mod_3_crlf")
    assert(forward["needs_mod_3.f90"].include?("mod_4_cr.f90"), "needs mod_4_cr")
    assert_equal(2, forward["needs_mod_3.f90"].size, "needs mod_3_crlf and mod_4_cr")
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

  def test_set_forward_subdir
    esti = FruitRakeEstimate.new
    esti.source_dirs = ["./", "subdir/"]
    forward = esti.set_forward
    assert_equal(["mod_in_subdir.f90"], forward["main_use_subdir.f90"])
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

  def test_src_and_obj_for_main__absent
    esti = FruitRakeEstimate.new

    assert_raise RuntimeError do
      src, obj = esti.src_and_obj_for_main("not_existing.f90")
    end
  end

  def test_src_and_obj_for_main__2
    esti = FruitRakeEstimate.new
    src, obj = esti.src_and_obj_for_main("main.f90")

    assert_equal(["sample08.f08", "test_mod.f03", "main.f90"], src)
    assert_equal(["sample08.o", "test_mod.o", "main.o"], obj)
  end

  def test_src_and_obj_for_main__subdir
    esti = FruitRakeEstimate.new
    esti.source_dirs = ["./", "subdir/"]
    src, obj = esti.src_and_obj_for_main("main_use_subdir.f90")

    assert_equal(["mod_in_subdir.f90", "main_use_subdir.f90"], src)
    assert_equal(["mod_in_subdir.o", "main_use_subdir.o"], obj)
  end
end

