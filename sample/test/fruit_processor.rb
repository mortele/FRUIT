#!/usr/bin/env ruby

require 'rubygems'
require 'rake'

class FruitProcessor

  def initialize
    @driver_file='fruit_driver_gen_fruit'
    @files = FileList['*_test.f90']
    @module_names=[]
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
      @module_names << module_name

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
      source_file.grep( /^\s*subroutine\s*(\w+)\s*$/i ) do 
        next if $1.downcase== "setup"
        next if $1.downcase== "teardown"
        names << $1
      end
    end
    names
  end

  def create_driver
      File.open("#{@driver_file}.f90", 'w') do |f| 
        f.write "program #{@driver_file}\n"
        f.write "  use fruit\n"
        @module_names.each do |name|
          f.write "  use #{name}\n"
        end
        f.write "  call initializeFruit\n"
        @module_names.each do |name|
          f.write "  call all_#{name}\n"
        end
        f.write "  call getTestSummary\n"
        f.write "end program #{@driver_file}\n"
      end
  end

  def generate_task_list
    puts "All executable specifications from tests :"
    @files.each do |file|
      puts "  #{file.gsub('_test.f90', '')}"
      puts "  --"
      grep_test_method_names(file).each do |method_name|
        puts "    -- #{method_name.gsub(/^test_/, '').gsub('_', ' ')}"
      end
    end
  end

end
