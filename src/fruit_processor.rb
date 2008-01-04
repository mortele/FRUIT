#!/usr/bin/env ruby

require 'rubygems'
require 'rake'

class FruitProcessor

  def initialize
    @driver_file='fruit_driver_gen_fruit'
    @files = FileList['*_test.f90']
    @source_file_names=[]
    @spec_hash={}
  end

  def pre_process
    create_module
    create_driver
  end

  def create_module
    @files.each do |file|
      fruit_file = file.gsub('.f90', '_gen_fruit.f90')
      file_name=file.gsub(".f90", "")
      module_name = "#{file_name}_gen_fruit"
      @source_file_names << module_name

      File.open(fruit_file, 'w') do |f| 
        f.write "module #{module_name}\n"
        f.write "  use fruit\n"
        f.write "   contains\n"
        f.write "     subroutine all_#{module_name}\n"
        f.write "       use #{file_name}\n"

        f.write "\n"

        method_names = grep_test_method_names(file)
        method_names.each do |method_name|
          f.write "       call setup\n"
          f.write "       write (*, *) \"  ..running #{method_name}\"\n"
          f.write "       call #{method_name}\n"
          # get failed count, added failed spec name into array
          f.write "       call teardown\n"
        end

        f.write "     end subroutine all_#{module_name}\n"
        f.write "end module #{module_name}\n"
      end
    end
  end

  def grep_test_method_names file
    names=[]
    File.open(file, 'r') do |source_file|
      source_file.grep( /^\s*subroutine\s*(\w+)\s*$/i ) do |dummy|
        subroutine_name=$1
        next if subroutine_name.downcase== "setup"
        next if subroutine_name.downcase== "teardown"
        names << subroutine_name
      end
    end
    names
  end

  def create_driver
    File.open("#{@driver_file}.f90", 'w') do |f| 
      f.write "program #{@driver_file}\n"
      f.write "  use fruit\n"
      @source_file_names.each do |name|
        f.write "  use #{name}\n"
      end
      f.write "  call initializeFruit\n"
      @source_file_names.each do |name|
        f.write "  call all_#{name}\n"
      end
      f.write "  call getTestSummary\n"
      # print all spec result array
      f.write "end program #{@driver_file}\n"
    end
  end

  def get_specs
    specs=[]
    spec=''

    @files.each do |file|
      File.open(file, 'r') do |infile|
        while (line = infile.gets)
          if line =~ /^\s*subroutine\s+test_(\w+)\s*$/i
            subroutine_name=$1
            spec_var=nil

            while (inside_subroutine = infile.gets)
              break if inside_subroutine =~ /end\s+subroutine/i
              next if inside_subroutine !~ /^\s*character.*::.*spec.*=(.*)$/i
              spec_var = $1.strip!

              spec_var = spec_var[1, spec_var.length-1]

              if end_match(spec_var, '&')
                spec_var.chop!
                while (next_line = infile.gets)
                  next_line.strip!
                  spec_var += "\n#{next_line.chop}"
                  break if ! end_match(next_line, '&')
                end
              end 
            end # end of inside subroutine lines

            if spec_var == nil
              spec=subroutine_name.gsub('_', ' ')
            else
              spec = spec_var
            end

            specs << spec
          end # end of test match
        end # end of each line in file
      end # end of file open
    end # end of each file
    specs
  end

  def end_match (string, match)
    return false if string == nil or string.length ==1
    return string[string.length-1, string.length-1] == match
  end

  def spec_report
    puts "All executable specifications from tests :"
    @files.each do |file|
      puts "  #{file.gsub('_test.f90', '')}"
      puts "  --"
      spaces = "    -- "
      indent = "  " + spaces.gsub("-", " ")
      get_specs.each do |spec|
        line = spec.gsub("\n", "\n#{indent}")
        puts "#{spaces}#{line}"
      end
    end
  end
end