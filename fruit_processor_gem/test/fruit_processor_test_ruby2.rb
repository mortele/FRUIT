require 'minitest/autorun'  #<- added oct_2016
require 'minitest/unit'  #<- added oct_2016
require '../lib/fruit_processor'
require 'pathname'

class FruitProcessorTest < Minitest::Test
  @@driver     = "fruit_driver_gen.f90"
  @@driver_mpi = "fruit_driver_mpi_gen.f90"
  @@basket     = "fruit_basket_gen.f90"
  @@generated = [@@basket, @@driver]

  def setup
    @fixture = FruitProcessor.new
  end

  def test_init
    @fixture.pre_process
    @fixture.pre_process_mpi
  end

#  def test_get_spec_hash
#    got_hash = @fixture.get_spec_hash("calculator_test.f90")
#  end

  def test_fruit_picker__when_removed
    dir = "for_test_picker_when_removed"
    basket        = dir + "/fruit_basket_gen.f90"
    tester        = dir + "/when_removed_test.f90"

    if (File.exist?(basket))
        File.unlink(basket)
    end

    (1..2).each{|iloop|
      File.open( tester, "w"){|f|
        f.write <<-END
          module when_removed_test
            implicit none
          contains
            subroutine test_first
              assert_equals(1, 1, "asserts 1 is 1")
            end subroutine test_first
        END
        if iloop == 1
          f.write <<-END
          subroutine test_second
            assert_equals(3, 4, "asserts 3 is 4")
          end subroutine test_second
          END
        end
        f.write <<-END
          end module when_removed_test
        END
      }
  
      fp = FruitProcessor.new
      fp.load_files   dir
      fp.fruit_picker dir
  
      # puts "-------"
      if_test_first = false
      if_test_second = false
      File.open(basket, 'r') do |f|
        f.each{|line|
          # puts line
          if /call +run_test_case *\( *test_first *,/ =~ line
            if_test_first = true
          end
          if /call +run_test_case *\( *test_second *,/ =~ line
            if_test_second = true
          end
        }
      end

      assert(if_test_first, "routine test_first called")
      if iloop == 1
        assert(if_test_second, "routine test_second should called")
      else
        assert(! if_test_second, "routine test_second should not called")
      end
      # puts "-------"
    }
    File.unlink tester
  end



  def test_fruit_picker__comment
    dir = "for_test_picker__comment"
    basket        = dir + "/fruit_basket_gen.f90"
    tester        = dir + "/comment_test.f90"

    if (File.exist?(basket))
        File.unlink(basket)
    end

    File.open( tester, "w"){|f|
      f.write <<-END
        module comment_test !comment here
          implicit none
        contains
          subroutine test_first!comment
            assert_equals(1, 1, "asserts 1 is 1")
          end subroutine test_first

          subroutine test_second  !comment
            assert_equals(3, 4, "asserts 3 is 4")
          end subroutine test_second

          subroutine & !
   &test_thi&
     &rd
            assert_equals(3, 4, "asserts 3 is 4")
          end subroutine test_third
        end module comment_test
      END
    }

    fp = FruitProcessor.new
    fp.load_files   dir
    fp.fruit_picker dir

    if_test_first  = false
    if_test_second = false
    if_test_third  = false
    File.open(basket, 'r') do |f|
      f.each{|line|
        # puts line
        if /call +run_test_case *\( *test_first *,/ =~ line
          if_test_first = true
        end
        if /call +run_test_case *\( *test_second *,/ =~ line
          if_test_second = true
        end
        if /call +run_test_case *\( *test_third *,/ =~ line
          if_test_third = true
        end
      }
    end

    assert(if_test_first , "routine test_first called")
    assert(if_test_second, "routine test_second should called")
    assert(if_test_third , "routine test_third  should called")

    File.unlink tester
  end



  def test_fruit_picker__timestamp
    basket        = "for_test_picker/fruit_basket_gen.f90"

    if (File.exist?(basket))
        File.unlink(basket)
    end

    fp = FruitProcessor.new
    fp.load_files "for_test_picker"

    fp.fruit_picker "for_test_picker"
    # puts "modify time of " + basket + ": " + File.mtime(basket).to_s
    timestamp_basket = File.mtime(basket).to_i

    sleep(1)

    time_now = Time.now.to_i

    assert(timestamp_basket < time_now)

    # Basket file is overwritten with the same content.
    # Modification time intended to be that of the older basket file.
    fp.fruit_picker "for_test_picker"
    p File.mtime(basket)
    timestamp_basket_2 = File.mtime(basket).to_i


    assert_equal(timestamp_basket_2,
                 timestamp_basket,
      "Modification time is that of the older basket file.")
    assert(timestamp_basket_2 < time_now)
  end



  def test_fruit_picker__dirs
    [true, false].each{|if_shuffle|
      basket        = "fruit_basket_gen.f90"
      if (File.exist?(basket))
          File.unlink(  basket)
      end

      fp = FruitProcessor.new
      fp.load_files ["subdir", "subdir2"]
      files = fp.get_files
      assert(
        files.include?("subdir/in_subdir_test.f90"),
        "has subdir/..._test.f90"
      )
      assert(
        files.include?("subdir2/in_subdir2_test.f90"),
        "has subdir2/..._test.f90"
      )

      fp.shuffle = true if if_shuffle

      assert(!File.exist?(basket), "#{basket} absent")
      fp.fruit_picker ["subdir", "subdir2"]
      assert( File.exist?(basket), "#{basket} created")

      ok_subdir = false
      ok_subdir2 = false
      call_test_aaa = 0
      call_test_bbb = 0
      call_test_ccc = 0
      File.open(basket, 'r') do |f|
        while line = f.gets
  #p line
          ok_subdir  = true if line =~ /^ *subroutine +in_subdir_test_all_tests/
          ok_subdir2 = true if line =~ /^ *subroutine +in_subdir2_test_all_tests/

          call_test_aaa += 1 if line =~ /call *run_test_case *\( *test_aaa *,/
          call_test_bbb += 1 if line =~ /call *run_test_case *\( *test_bbb *,/
          call_test_ccc += 1 if line =~ /call *run_test_case *\( *test_method_ccc *,/
        end
      end
      assert(ok_subdir , "in_subdir_test_all_tests found")
      assert(ok_subdir2, "in_subdir_test_all_tests found")
      assert_equal(1, call_test_aaa, "calling test_aaa once")
      assert_equal(1, call_test_bbb, "calling test_bbb once")
      assert_equal(1, call_test_ccc, "calling test_ccc once")
    }
  end

  def test_fruit_picker
    basket        = "for_test_picker/fruit_basket_gen.f90"
    with_setup    = "for_test_picker/a_setup_test.f90"
    with_teardown = "for_test_picker/a_teardown_test.f90"
    if (File.exist?(basket))
        File.unlink(basket)
    end
    fp = FruitProcessor.new
    fp.load_files "for_test_picker"
    assert(fp.get_files.include?(with_setup))

    assert_equal("each", fp.get_spec_hash_filename(with_setup)['setup'])
    assert_equal(nil,    fp.get_spec_hash_filename(with_setup)['teardown'])

    assert_equal(nil,    fp.get_spec_hash_filename(with_teardown)['setup'])
    assert_equal("each", fp.get_spec_hash_filename(with_teardown)['teardown'])

    fp.fruit_picker "for_test_picker"

    done = {}
    File.open(basket, 'r') do |file|
      first_sub = nil
      last_sub = nil
      count_setup = 0
      count_teardown = 0
      while line = file.gets
        if line =~ /^ *subroutine +([a-zA-Z_]+)_all_tests/
          present_tester_file = $1
          first_sub = nil
          last_sub = nil
          count_setup = 0
          count_teardown = 0
        end
        if line =~ /^ *call +([a-zA-Z_]+)/
          first_sub = $1 if !first_sub
          last_sub = $1
          count_setup    += 1 if $1 == "setup"
          count_teardown += 1 if $1 == "teardown"
        end
        if line =~ /^ *end +subroutine +([a-zA-Z_]+)_all_tests/
          done[present_tester_file] = 1
          if present_tester_file == "a_setup_test"
            assert_equal("setup", first_sub)
            assert_equal(2, count_setup, "setup twice")
          end
          if present_tester_file == "a_teardown_test"
            assert_equal("teardown", last_sub)
            assert_equal(2, count_teardown, "teardown twice")
          end
          if present_tester_file == "both_setup_teardown_test"
            assert_equal("setup", first_sub)
            assert_equal("teardown", last_sub)
            assert_equal(2, count_setup, "setup twice")
            assert_equal(2, count_teardown, "teardown twice")
          end
        end
      end
    end
    assert(done['a_setup_test'])
    assert(done['a_teardown_test'])
    assert(done['both_setup_teardown_test'])
  end

  def test_load_files
    @fixture.load_files "."

    files = @fixture.get_files
    assert_equal(1, files.grep(/calculator_test\.f90$/).length, "detect .f90 files")
    assert_equal(1, files.grep(/myvector_test\.f03$/).length, "detect .f03 files")
  end

  def test_load_files__dirs
    @fixture.load_files [".", "subdir"]
    files = @fixture.get_files
    assert(
      files.include?("subdir/in_subdir_test.f90"),
      "has subdir/..._test.f90"
    )
  end


  def test_set_process_only
    fp = FruitProcessor.new
    assert_raises(RuntimeError, message = "raise if files set to process_only are not *_test.f??"){
      fp.process_only = ["in_subdir_test.f90", "in_subdir2_test.f03", "not_tester.f90"]
    }
    # assert_nothing_raised("no raise if files set to process_only are *_test.f??"){
    #   fp.process_only = ["in_subdir_test.f90", "in_subdir2_TEST.F03", "xxx_test.f08"]
    # }
  end

  def test_process_only__absent
    fp = FruitProcessor.new
    fp.process_only = ["in_subdir_test.f90", "in_subdir2_test.f90", "not_existing_test.f90"]
    assert_raises(RuntimeError, message = "absent file cause exception"){
      fp.load_files(["subdir/", "subdir2", "subdir2/"])
    }
  end


  def test_process_only__dirs
    fp = FruitProcessor.new
    fp.process_only = ["in_subdir_test.f90", "in_subdir2_test.f90"]
    fp.load_files(["subdir/", "subdir2", "subdir2/"])
    assert_equal(["subdir/in_subdir_test.f90", "subdir2/in_subdir2_test.f90"], fp.get_files)
  end

  def test_process_only
    fp = FruitProcessor.new
    fp.process_only = ["a_test.f90"]
    fp.process_only << "b_test.f90"

    results = fp.process_only
    assert_equal("a_test.f90", Pathname(results[0]).to_s)
    assert_equal("b_test.f90", Pathname(results[1]).to_s)

    fp.load_files "."
    results = fp.process_only
    assert_equal("a_test.f90", Pathname(results[0]).to_s)
    assert_equal("b_test.f90", Pathname(results[1]).to_s)

    spec_hash = fp.get_spec_hash_filename("./a_test.f90")
    assert_equal(
      ["test_aaa", "test_aaa2nd"],
      spec_hash["methods"]["name"],
      "test subroutine test_aaaa")
  end

  def test_test_module_name_from_file_path
    result = @fixture.test_module_name_from_file_path("./abc/def/ghi_jk.f90")
    assert_equal("ghi_jk", result)

    result = @fixture.test_module_name_from_file_path("mmm/nnn/ppp_qqq.f03")
    assert_equal("ppp_qqq", result)
  end

  def test_parse_module_name_of_file
    result = @fixture.parse_module_name_of_file("subdir3/a_filename_mismatch_test.f90")
    assert_equal("some_testermodule_test", result[0])
    assert_equal("second_module_test", result[1])
  end


  def test_module_name_consistent?
    result, err_msg = @fixture.module_name_consistent?("subdir3/a_filename_mismatch_test.f90")
    assert(! result, "module name should be inconsistent")
    assert(err_msg =~ /More than one/i, "More than one")

    result, err_msg = @fixture.module_name_consistent?("subdir3/no_tester_module_test.f90")
    assert(! result, "no test module")
    assert(err_msg =~ /no test module/i, "message ok")

    result, err_msg = @fixture.module_name_consistent?("subdir3/one_tester_module_test.f90")
    assert(result, "module name consistent")
    assert(err_msg == "", "no message if ok")
  end

  def test_parse_method_names
    fp = FruitProcessor.new
    filename = "def_upper_lower_test.f90"
    fp.parse_method_names(filename)
    method_names = fp.get_methods_of_filename(filename)

    assert_equal("test_lower", method_names[0])
    assert_equal("test_upper", method_names[1])
  end
  def test_parse_method_names__cause_error
    fp = FruitProcessor.new
    filename = "cause_error/def_upper_lower_test.f90"

    fp.parse_method_names(filename)
    method_names = fp.get_methods_of_filename(filename)

    fp.gather_specs(filename)
    spec_names =   fp.get_specs_of_filename(filename)

    assert_equal("test_lower", method_names[0])
    assert_equal("test_upper", method_names[1])

    assert_equal("lower", spec_names[0])
    assert_equal("upper", spec_names[1])
  end

  def test_load_files__cause_error
    fp = FruitProcessor.new
    dir = "cause_error/"
    fp.load_files(dir)

    filename_expected = "cause_error/def_upper_lower_test.f90"
    assert_equal(
      File.expand_path(filename_expected),
      File.expand_path(fp.get_files[0]),
      "a file in cause_error directry"
    )
  end



  def test_gather_specs_2
    fp = FruitProcessor.new

    filename = "./for_test_gather_specs.txt"
    fp.parse_method_names(filename)
    fp.gather_specs(filename)

    spec_names = fp.get_specs_of_filename(filename)

    assert_equal(0, /^abc [\n\r]+ DEF ghi [\n\r]+jkl$/ =~ spec_names[0])
    assert_equal("spec for 'test_aaaaa'", spec_names[1])
    assert_equal(
      0, /^calculation should produce 4\.0 when 2\.0 and 2\.0 [\n\r]+are [\n\r]+inputs/ =~ spec_names[2]
    )

    expected = "Spec string may given as Fortran's \"comment\" line."
    assert_equal(expected, spec_names[3])

    expected = "spec for \"test_abbaa\""
    assert_equal(expected, spec_names[4])
  end

  def test_warn_method_names
    fp = FruitProcessor.new
    warned_names = fp.warn_method_names("a_test.f90")
    assert(defined?(warned_names))
    if (warned_names)
      assert_equal(["test_warned_if_argument"], warned_names)
#      puts warned_names.to_s + " should be warned"
    end
  end

  def test_format_spec_fortran
    fp = FruitProcessor.new

    filename = "./for_test_gather_specs.txt"
    fp.parse_method_names(filename)
    fp.gather_specs(filename)

    spec_names = fp.get_specs_of_filename(filename)

    assert_equal(
             "  --- " + "abc &\n" +
      "  " + "      " + "& DEF ghi &\n" +
      "  " + "      " + "&jkl",
      fp.format_spec_fortran(spec_names[0], '  --- ')
    )
    assert_equal(
             "  --- " + "spec for ''test_aaaaa''",
      fp.format_spec_fortran(spec_names[1], '  --- ')
    )
    assert_equal(
             "  --- " + "calculation should produce 4.0 when 2.0 and 2.0 &\n" +
      "  " + "      " + "&are &\n" +
      "  " + "      " + "&inputs",
      fp.format_spec_fortran(spec_names[2], '  --- ')
    )
    assert_equal(
             "  --- " + "Spec string may given as Fortran''s \"comment\" line.",
      fp.format_spec_fortran(spec_names[3], '  --- ')
    )
    assert_equal(
             "  --- " + "spec for \"test_abbaa\"",
      fp.format_spec_fortran(spec_names[4], '  --- ')
    )
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
      assert_equal(0, /^\s*call\s+init_fruit/ =~ f.gets.chomp!)
      assert_equal(0, /^\s*call\s+init_fruit_xml/ =~ f.gets.chomp!)
      assert_equal(0, /^\s*call\s+fruit_basket\s*$/ =~ f.gets.chomp!)
    }
    timestamp_driver = File.mtime(@@driver).to_i
    sleep(1.1)
    time_now = Time.now.to_i
    assert(timestamp_driver < time_now)

    #create driver again. its timestamp should be same as older driver.
    @fixture.create_driver
    timestamp_driver_2 = File.mtime(@@driver).to_i

    assert_equal(timestamp_driver_2, timestamp_driver, 
      "modification time is that of the older driver file.")
  end

  def test_create_driver_mpi
    if File.exists?(@@driver_mpi)
      File.delete(@@driver_mpi)
    end
    @fixture.create_driver_mpi
    assert_equal(true, File.exists?(@@driver_mpi))
    File::open(@@driver_mpi){|f|
      assert_equal("program fruit_driver_mpi_gen", f.gets.strip!)
      assert_equal(0, /^use mpi$/              =~ f.gets.strip!)
      assert_equal(0, /^use fruit$/            =~ f.gets.strip!)
      assert_equal(0, /^use fruit_mpi$/        =~ f.gets.strip!)
      assert_equal(0, /^use fruit_basket_gen$/ =~ f.gets.strip!)
      assert_equal(0, /^integer :: ierror, size, rank$/  =~ f.gets.strip!)
      assert_equal(0, /^call MPI_INIT\(ierror\)$/    =~ f.gets.strip!)
      assert_equal(0, /^call MPI_COMM_SIZE\(MPI_COMM_WORLD, size, ierror\)$/ =~ f.gets.strip!)
      assert_equal(0, /^call MPI_COMM_RANK\(MPI_COMM_WORLD, rank, ierror\)$/ =~ f.gets.strip!)
      assert_equal(0, /^call init_fruit/      =~ f.gets.strip!)
      assert_equal(0, /^call fruit_init_mpi_xml/  =~ f.gets.strip!)
      assert_equal(0, /^call fruit_basket$/    =~ f.gets.strip!)
      assert_equal(0, /^call fruit_summary_mpi\s*\(size, rank\)$/ =~ f.gets.strip!)
      assert_equal(0, /^call fruit_summary_mpi_xml\s*\(size, rank\)$/ =~ f.gets.strip!)
      assert_equal(0, /^call fruit_finalize_mpi\s*\(size, rank\)$/ =~ f.gets.strip!)
      assert_equal(0, /^call MPI_FINALIZE\(ierror\)$/    =~ f.gets.strip!)
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
      assert_equal(true, File.exists?(f), "#{f} should exist")
    }
  end

  def test_gather_specs
    test_init

    sub_names = @fixture.get_methods_of_filename("./calculator_test.f90")
    specs     = @fixture.get_specs_of_filename(  "./calculator_test.f90")
    assert_equal(sub_names.length, specs.length)

    assert_equal(
      "test_calculator_should_produce_4_when_2_and_2_are_inputs",
      sub_names[0])
    assert_equal(
      "test_more_with_spec_in_spec_variable",
      sub_names[1])
    assert_equal(
      "calculation should produce 4.0 when 2.0 and 2.0 \nare \ninputs",
      specs    [1])

    assert_equal("test_calculator_should_remember_previous_calculation_results", sub_names[2])
    assert_equal("test_calculator_should_reset_when_reset_is_called", sub_names[3])

    sub_names = @fixture.get_methods_of_filename("./myvector_test.f03")
    specs     = @fixture.get_specs_of_filename(  "./myvector_test.f03")
    assert_equal(sub_names.length, specs.length)

    assert_equal("test_init", sub_names[0])
    assert_equal("test_always_ok", sub_names[1])
    assert_equal("test_always_two_fails_one_success", sub_names[2])
    assert_equal("test_all_elems", sub_names[3])

    sub_names = @fixture.get_methods_of_filename("./fruit_util_test.f90")
    specs     = @fixture.get_specs_of_filename(  "./fruit_util_test.f90")
    assert_equal(sub_names.length, specs.length)

    assert_equal("test_to_s_should_convert_int_to_string", sub_names[0])
    assert_equal(     "to s should convert int to string", specs    [0])
  end

#  def test_generate_spec
#    assert_equal(0, @fixture.get_specs.size)
#  end

end

class  FruitFortranFileTest < Minitest::Test
  def test_read_noarg_sub_name
    fortran_code = "fortran_cont_and_comment.f90"
    if (File.exist?(fortran_code))
        File.unlink(fortran_code)
    end
    File.open( fortran_code, "w"){|f|
      f.write <<-END
subroutine &
test_abcd !

   subrou&
   &tine test_ef&
gh

subroutine &
test_& !
 !comment line and a blank line may occur

&ijkl

subroutine test_mnop !

subroutine test_qrst

      END
    }
    sub_names = []
    FruitFortranFile.open(fortran_code, 'r') do |f|
      while sub_name = f.read_noarg_sub_name do
        sub_names.push( sub_name )
      end
    end
    assert_equal( "test_abcd", sub_names[ 0 ] )
    assert_equal( "test_efgh", sub_names[ 1 ] )
    assert_equal( "test_ijkl", sub_names[ 2 ] )
    assert_equal( "test_mnop", sub_names[ 3 ] )
    assert_equal( "test_qrst", sub_names[ 4 ] )
    assert_equal( 5, sub_names.size )

    if (File.exist?(fortran_code))
        File.unlink(fortran_code)
    end
  end

  def test_read_fortran_line
    fortran_code = "fortran_cont_and_comment.f90"
    if (File.exist?(fortran_code))
        File.unlink(fortran_code)
    end
    File.open( fortran_code, "w"){|f|
      f.write <<-END
subroutine &
test_abcd !

   subrou&
   &tine test_ef&
gh

subroutine &
test_& !
 !comment line and a blank line may occur

&ijkl

subroutine test_mnop !

subroutine test_qrst

string = "abc"
string = "ab& c"
string = "d!ef"
string = "abc!def&!" // & !comment
 &"ghi!"
      END
    }
    array = []
    string_lines = []
    FruitFortranFile.open(fortran_code, 'r') do |f|
      while line = f.read_fortran_line do
        if line.match("^\s*subroutine")
          line.sub!(/^\s+/, "")
          line.sub!(/\s*(\!.*)?[\n\r]*$/, "")
          array.push( line )
        elsif line.match("^\s*string ")
          string_lines.push line
        end
      end
    end
    assert_equal( "subroutine test_abcd", array[ 0 ] )
    assert_equal( "subroutine test_efgh", array[ 1 ] )
    assert_equal( "subroutine test_ijkl", array[ 2 ] )
    assert_equal( "subroutine test_mnop", array[ 3 ] )
    assert_equal( "subroutine test_qrst", array[ 4 ] )
    assert_equal( 5, array.size )

    assert_equal( "string = \"abc\"\n", string_lines[0] )
    assert_equal( "string = \"ab& c\"\n", string_lines[1] )
    assert_equal( "string = \"d!ef\"\n", string_lines[2] )
#fail#  assert_equal( "string = \"abc!def&!\" // \"ghi!\"\n", string_lines[3] )

    if (File.exist?(fortran_code))
        File.unlink(fortran_code)
    end
  end

end

