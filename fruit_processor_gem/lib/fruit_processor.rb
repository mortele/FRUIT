#!/usr/bin/env ruby

require 'rubygems'
require 'rake'
require "pathname"

# require 'misc'

class FruitProcessor
  attr_accessor :shuffle
  attr_accessor :xml_prefix

  #------
  # attr_accessor :process_only
  #------
  def process_only
    @process_only
  end
  def process_only=(val)
    @process_only = val

    val.each{|a_file|
      match = false
      @extensions.each{|fxx|
        if    a_file.match(/_test.#{fxx}/)
          match = true
        elsif a_file.match(/_test.#{fxx}/i)
          puts "Warning: _test.f?? in filename must be lower case while #{a_file} has upper case"
          match = true
        end
      }
      raise "#{a_file} not match with *_test.(" + @extensions.join("|") + ")" if !match
    }
  end
  #------

  def initialize
    @driver_program_name='fruit_driver_gen'
    @driver_program_mpi_name='fruit_driver_mpi_gen'
    @fruit_basket_module_name = 'fruit_basket_gen'

    @extensions = ["f90", "f95", "f03", "f08"]
    @spec_hash={}

    @shuffle = false
  end

  def get_spec_hash_filename file_name
    return @spec_hash[Pathname(file_name).cleanpath.to_s]
  end

  def get_methods_of_filename file_name
    return @spec_hash[file_name]["methods"]["name"]
  end

  def get_specs_of_filename file_name
    return @spec_hash[file_name]["methods"]["spec"]
  end

  def get_files
    return @files
  end

  def load_files dir="."
    return if @spec_hash.size != 0

    dirs = []
    if dir.instance_of?(Array)
      dirs = dir
    else
      dirs = [dir]
    end

    @files = []
    if @process_only
      @process_only.each{|f|
        found = false
        dirs.each{|a_dir|
          candi = (Pathname(a_dir) + f).to_s
          if File.exist?(candi)
            @files.concat(FileList[candi])
            found = true
          end
        }
        raise "File #{f} not found" if !found
      }
    else
      @extensions.each{|fxx|
        found = false
        dirs.each{|a_dir|
          @files.concat(
            FileList["#{a_dir}/*_test." + fxx] - FileList["#{a_dir}/~*_test." + fxx]
          )
        }
      }
    end

    @files.uniq!

    @files.each do |file|
      parse_method_names file
      gather_specs file
    end
  end

  def pre_process dir="."
    load_files dir
    fruit_picker dir
    create_driver dir
  end

  def pre_process_mpi dir="."
    load_files dir
    fruit_picker dir
    create_driver dir
    create_driver_mpi dir
  end


  def module_name_consistent? file
    test_module_name = test_module_name_from_file_path file

    mods = parse_module_name_of_file file

    if mods.size == 1 and mods.include?(test_module_name.downcase)
      return true, ""
    end

    if ! mods or mods.size == 0
      error_msg = "FRUIT Error: No test module (*_test) found in file " + file + "\n"
    elsif mods.size > 1
      error_msg =  "FRUIT Error: More than one tester modules in file #{file}\n"
      error_msg += "  existing modules (*_test): " + mods.join(", ") + "\n"
      error_msg += "  expected module:           " + test_module_name
    elsif ! mods.include?(test_module_name.downcase)
      error_msg  = "FRUIT Error: No test module #{test_module_name} found in file #{file}\n"
      error_msg += "  existing modules (*_test): " + mods.join(", ") + "\n"
      error_msg += "  expected module:           " + test_module_name
    end
    return false, error_msg
  end


  def fruit_picker dir="."
    dir = "." if dir.instance_of?(Array)

    test_subroutine_names=[]

    fruit_basket_file = (Pathname(dir) + "#{@fruit_basket_module_name}.f90").to_s

    #------ move existing file to _old
    fruit_basket_file_old = fruit_basket_file.gsub(/\.f90$/, "\.f90_old")

    if (File.exist?( fruit_basket_file_old ))
        FileUtils.rm(fruit_basket_file_old )
    end

    if (File.exist?( fruit_basket_file ))
      File.rename(fruit_basket_file, fruit_basket_file_old)
    end
    #------ move existing file to _old

    File.open(fruit_basket_file, 'w') do |f|
      f.write "module #{@fruit_basket_module_name}\n"
      f.write "  use fruit\n"
      f.write "contains\n"
    end

    File.open(fruit_basket_file, 'a') do |f|

      # error if @files is empty

      files_order = @files
      files_order = files_order.sort_by{ rand } if @shuffle

      files_order.each do |file|
        test_module_name = test_module_name_from_file_path file

        if_ok, error_msg = module_name_consistent? file
        #warn error_msg if (!if_ok) 
        raise error_msg if (!if_ok) 

        subroutine_name="#{test_module_name}_all_tests"
        test_subroutine_names << subroutine_name
        f.write "  subroutine #{subroutine_name}\n"
        f.write "    use #{test_module_name}\n"
                     #memo: this "use" will fail if tester filename and its module name mismatch
        f.write "\n"

        method_names = @spec_hash[file]['methods']['name']
        spec_names   = @spec_hash[file]['methods']['spec']

        if (method_names.length != spec_names.length)
          puts "Error in " + __FILE__ + ": number of methods and specs mismatch"
          puts "  methods:" + method_names.to_s 
          puts "  specs:" + spec_names.to_s
        end

        if @spec_hash[file]['setup'] != nil
          if @spec_hash[file]['setup']=='all'
            f.write "    call setup_before_all\n"
          end
        end

        if @xml_prefix
          f.write "    call set_prefix(\"#{@xml_prefix}\")\n"
        end

        method_names = method_names.sort_by{ rand } if @shuffle

        spec_counter = 0
        method_names.each do |method_name|
          if @spec_hash[file]['setup'] != nil
            if @spec_hash[file]['setup']=='each'
              f.write "    call setup\n"
            end
          end
          f.write "    write (*, *) \"  ..running test: #{method_name}\"\n"
          f.write "    call set_unit_name('#{method_name}')\n"
          f.write "    call run_test_case (#{method_name}, &\n"
          f.write "                      &\"#{method_name}\")\n"
          f.write "    if (.not. is_case_passed()) then\n"
          f.write "      write(*,*) \n"
          f.write "      write(*,*) '  Un-satisfied spec:'\n"
          f.write "      write(*,*) '#{format_spec_fortran(spec_names[spec_counter], '  -- ')}'\n"
          f.write "      write(*,*) \n"

          f.write "      call case_failed_xml(\"#{method_name}\", &\n"
          f.write "      & \"#{test_module_name}\")\n"
          f.write "    else\n"
          f.write "      call case_passed_xml(\"#{method_name}\", &\n"
          f.write "      & \"#{test_module_name}\")\n"
          f.write "    end if\n"

          if   @spec_hash[file]['teardown'] != nil
            if @spec_hash[file]['teardown']=='each'
              f.write "    call teardown\n"
            end
          end
          f.write "\n"
          spec_counter += 1
        end

        if   @spec_hash[file]['teardown'] != nil
          if @spec_hash[file]['teardown']=='all'
            f.write "    call teardown_after_all\n"
          end
        end

        f.write "  end subroutine #{subroutine_name}\n"
        f.write "\n"

      end
    end

    File.open(fruit_basket_file, 'a') do |f|
      f.write "  subroutine fruit_basket\n"
      test_subroutine_names.each do |test_subroutine_name|
        f.write "    call #{test_subroutine_name}\n"
      end
      f.write "  end subroutine fruit_basket\n"
      f.write "\n"
      f.write "end module #{@fruit_basket_module_name}"
    end

    #--- move fruit_basket_file_old to fruit_basket_file if their content are the same.
    if (File.exist?( fruit_basket_file_old ))
      lines_now = []
      open(fruit_basket_file    ){|f| lines_now = f.readlines }
      lines_old = []
      open(fruit_basket_file_old){|f| lines_old = f.readlines }

      if lines_old == lines_now
        File.rename(fruit_basket_file_old, fruit_basket_file)
      else
        FileUtils.rm(fruit_basket_file_old )
      end
    end
  end


  def parse_method_names file_name
    FruitFortranFile.open(file_name, 'r') do |f|
      @spec_hash[file_name]={}
      @spec_hash[file_name]['methods'] = {}
      @spec_hash[file_name]['methods']['name'] =[]
      @spec_hash[file_name]['methods']['spec'] =[]

      while subroutine_name = f.read_noarg_sub_name
        if subroutine_name.downcase == "setup"
          @spec_hash[file_name]['setup']='each'
          next
        end
        if subroutine_name.downcase == "setup_before_all"
          @spec_hash[file_name]['setup']='all'
          next
        end
        if subroutine_name.downcase == "teardown"
          @spec_hash[file_name]['teardown']='each'
          next
        end
        if subroutine_name.downcase == "teardown_after_all"
          @spec_hash[file_name]['teardown']='all'
          next
        end

        #The same condition must be used for storing
        #both subroutine name and spec string.
        #Otherwise number of subroutine names and specs mismatch.
        next if subroutine_name !~ /^test_/
        @spec_hash[file_name]['methods']['name'] << subroutine_name
      end
    end
  end

  def warn_method_names file_name
    warned = []
    FruitFortranFile.open(file_name, 'r') do |f|
      while subroutine_name = f.read_tester_name_with_arg
        warned << subroutine_name
      end
    end
    if warned
      return warned
    else
      return nil
    end
  end

  # look into all files lib_test_*.a in build_dir, then generate driver files
  def create_driver dir="."
    dir = "." if dir.instance_of?(Array)

    driver_new = (Pathname(dir) + "#{@driver_program_name}.f90").to_s

    #------ rename older driver
    driver_old = driver_new.gsub(/\.f90$/, "\.f90_old")

    if (File.exist?( driver_old))
        FileUtils.rm(driver_old)
    end

    if (File.exist?(driver_new))
      File.rename(  driver_new, driver_old)
    end
    #------ rename older driver

    File.open(driver_new, 'w') do |f|
      f.write "program #{@driver_program_name}\n"
      f.write "  use fruit\n"
      f.write "  use #{@fruit_basket_module_name}\n"
      f.write "  call init_fruit\n"
      f.write "  call init_fruit_xml\n"
      f.write "  call fruit_basket\n"
      f.write "  call fruit_summary\n"
      f.write "  call fruit_summary_xml\n"
      f.write "  call fruit_finalize\n"
      f.write "end program #{@driver_program_name}\n"
    end

    #------ restore older driver if newer driver has the same content.
    if (File.exist?( driver_old ))
      #### compare fruit_basket_file and fruit_basket_file_old.

      lines_now = []
      open(driver_new){|f|
        lines_now = f.readlines
      }
      lines_old = []
      open(driver_old){|f| 
        lines_old = f.readlines
      }

      if lines_old == lines_now
        File.rename(driver_old, driver_new)
      else
        FileUtils.rm(driver_old)
      end
    end
  end

  def create_driver_mpi dir = "."
    dir = "." if dir.instance_of?(Array)

    filename = (Pathname(dir) + "#{@driver_program_mpi_name}.f90")
    File.open(filename, 'w') do |f|
      f.write "program #{@driver_program_mpi_name}\n"
      f.write "  use mpi\n"
      f.write "  use fruit\n"
      f.write "  use fruit_mpi\n"
      f.write "  use #{@fruit_basket_module_name}\n"
      f.write "  integer :: ierror, size, rank\n"
      f.write "  call MPI_INIT(ierror)\n"
      f.write "  call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)\n"
      f.write "  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)\n"
      f.write "  call init_fruit (rank)\n"
      f.write "  call fruit_init_mpi_xml(rank)\n"
      f.write "  call fruit_basket\n"
      f.write "  call fruit_summary_mpi    (size, rank)\n"
      f.write "  call fruit_summary_mpi_xml(size, rank)\n"
      f.write "  call fruit_finalize_mpi   (size, rank)\n"
      f.write "  call MPI_FINALIZE(ierror)\n"
      f.write "end program #{@driver_program_mpi_name}\n"
    end
  end

  def gather_specs file
    spec=''
    FruitFortranFile.open(file, 'r') do |f|
      while subroutine_name = f.read_noarg_sub_name
        next if subroutine_name !~ /^test_/
        spec_var=nil

        while (inside_subroutine = f.gets)
          break if inside_subroutine =~ /^\s*end\s+subroutine/i
          break if inside_subroutine =~ /^\s*end *(!.*)?$/i

          if inside_subroutine =~ /^\s*\!FRUIT_SPEC\s*(.*)$/i
            spec_var = $1.chomp
            next
          end

          next if inside_subroutine !~ /^\s*character.*::\s*spec\s*=(.*)$/i
          spec_var = $1
          spec_var =~ /\s*(["'])(.*)(\1|\&)\s*(!.*)?$/
          spec_var = $2
          last_character = $3

          if last_character == '&'
            while (next_line = f.gets)
              next_line.strip!
              next_line.sub!(/^\&/, '')
              spec_var += "\n#{next_line.chop}"
              break if ! end_match(next_line, '&')
            end
          end
        end # end of inside subroutine lines

        if spec_var == nil
          spec=subroutine_name.gsub('test_', '').gsub('_', ' ')
        else
          spec = spec_var
        end

        @spec_hash[file]['methods']['spec'] << spec
      end # end of each subroutine name
    end # end of file open
  end

  def end_match (string, match)
    return false if string == nil or string.length ==1
    return string[string.length-1, string.length-1] == match
  end

  def spec_report
    load_files

    puts "\n"
    puts "All executable specifications from tests:"
    @spec_hash.keys.sort.each do |file|
      method_hash=@spec_hash[file]
      #@spec_hash[file]['methods']['spec']
      puts "  #{(test_module_name_from_file_path file).gsub('_test', '')}"
      puts "  --"

      method_hash.each_pair do |method, method_values|
        next if !method_values['spec']
        spaces = "    -- "

        method_values['spec'].each do |spec|
          puts format_spec(spec, spaces)
        end
      end
      puts "\n"
    end
  end

  def format_spec (spec, spaces, ending='')
    indent = "  " + spaces.gsub("-", " ")
    line = spec.gsub("\n", "#{ending}\n#{indent}")
    "#{spaces}#{line}"
  end

  def format_spec_fortran(spec, spaces)
    indent = "  " + spaces.gsub("-", " ")
    line = spec.gsub("\n", "&\n#{indent}&").gsub("'", "''")
    "#{spaces}#{line}"
  end

  def base_dir
    orig_path = Dir.pwd
    found_dir=''
    protection_counter = 0
    while true
      if File.exist? "ROOT_ANCHOR"
        found_dir=Dir.pwd
        break
      end
      if Dir.pwd == "/" or protection_counter > 100
        FileUtils.cd(orig_path)
        FileUtils.cd("../")
        found_dir=Dir.pwd
        break
      end
      FileUtils.cd("../")
      protection_counter +=1
    end
    FileUtils.cd(orig_path)
    return found_dir
  end

  def build_dir
    (Pathname(base_dir) + "build").to_s
  end

  def module_files(all_f90_files, build_dir)
    return [] if all_f90_files == nil or all_f90_files.size == 0
    module_with_path=[]
    # assume each source .f90 has a module.  This is a cleanup task, doesn't matter if removed non-existing file
    all_f90_files.ext('mod').each do |file|
      module_with_path << (Pathname(build_dir) + file).to_s
    end
    return module_with_path
  end

  def lib_base_files(lib_bases, build_dir=nil)
    if build_dir != nil
      puts "build_dir on lib_base_files is obsolete"
      build_dir = nil
    end
    return if lib_bases == nil
    lib_base_files =[]
    lib_bases.each do |pair|
      lib_base_files << Pathname(pair[1]) + "lib#{pair[0]}.a"
    end
    return lib_base_files
  end

  def lib_name_flag (lib_bases, build_dir)
    return if lib_bases == nil
    lib_name_flag = ''
    lib_bases.each { |pair| lib_name_flag += "-l#{pair[0]} " }
    return lib_name_flag
  end

  def lib_dir_flag(lib_bases, build_dir)
    return if lib_bases == nil
    lib_dir_flag = ''
    _libs=[]
    lib_bases.each do |pair|
      pair[1] = build_dir if pair[1] == nil
      _libs << pair[1]
    end
    _libs.uniq.each { |value|  lib_dir_flag += "-L#{value} " }
    return lib_dir_flag.strip
  end

  def inc_flag inc_dirs
    inc_dirs.collect {|item|
      "-I#{item}" if item.size > 0
    }.join(" ")
  end

  def test_module_name_from_file_path file_name
    test_module_name=""
    @extensions.each{|fxx|
      if file_name =~ /(.+)\.#{fxx}/
        test_module_name=$1
      end
    }
    Pathname(test_module_name).basename.to_s
  end

  def parse_module_name_of_file file_name
    mods = []
    FruitFortranFile.open(file_name, 'r') do |f|
      while module_name = f.read_mod_name
        if module_name =~ /^(\S+_test)$/i
          test_name = $1.downcase
          mods.push(test_name)
        end
      end
    end
    return mods
  end
end

class FruitFortranFile < File
  def read_noarg_sub_name
    while fortran_line = read_fortran_line do
      if fortran_line.match( /^\s*subroutine\s*(\w+)\s*(\!.*)?$/i )
        sub_name = $1
        return sub_name
      end
    end
    return nil
  end

  def read_tester_name_with_arg
    while fortran_line = read_fortran_line do
      if fortran_line.match( /^\s*subroutine\s*(test\w+)\s*\(/i )
        tester_with_arg = $1
        return tester_with_arg
      end
    end
  end

  def read_mod_name
    while fortran_line = read_fortran_line do
      if fortran_line.match( /^\s*module\s*(\w+)\s*(\!.*)?$/i )
        sub_name = $1
        return $1
      end
    end
    return nil
  end

  def read_fortran_line
    conti_line_end =   /\&\s*(\!.*)?[\n\r]*$/
    empty_line    = /^\s*(\!.*)?[\n\r]*$/

    #Skip empty lines
    line = ""
    while (line.match(empty_line))
      line = self.gets
      return line if (not line)
    end

    #Join FORTRAN's coitinuous lines ingoring comments (!) and empty lines.
    while (line.match(conti_line_end))
      line2 = self.gets
      break if (not line2)
      next  if line2.match( empty_line )
      line.sub!(conti_line_end, "")
      line2.sub!(/^\s*\&/, "")
      line = line + line2
    end
    return line
  end
end

