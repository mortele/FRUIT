#!/usr/bin/env ruby

require 'rubygems'
require 'rake'

class FruitProcessor

  def initialize
    @driver_file='fruit_driver_gen_fruit'
    @module_names=[]
  end

  def pre_process
    create_module
    create_driver
  end

  def create_module
    files = FileList['*_test.f90']

    files.each do |file|
      fruit_file = file.gsub('.f90', '_gen_fruit.f90')
      file_name=file.gsub(".f90", "")
      module_name = "#{file_name}_gen_fruit"
      @module_names << module_name

      File.open(fruit_file, 'w') do |f| 
        f.write "module #{module_name}\n"
        f.write "  use fruit\n"
        f.write "   contains\n"
        f.write "     subroutine all_#{file_name}\n"
        f.write "       use #{file_name}\n"

        f.write "\n"
        f.write "       call setup\n"
        f.write "       call test_calculator_should_produce_4_when_2_and_2_are_inputs\n"
        f.write "       call teardown\n"

        f.write "     end subroutine all_#{file_name}\n"
        f.write "end module #{module_name}\n"
      end

    end

  end

  def create_driver
      File.open("#{@driver_file}.f90", 'w') do |f| 
        f.write "program #{@driver_file}\n"
        f.write "  use fruit\n"
        @module_names.each do |name|
          f.write "  use #{name}\n"
        end
        f.write "  call initializeFruit\n"
        f.write "  call all_calculator_test\n"
        f.write "  call getTestSummary\n"
        f.write "end program #{@driver_file}\n"
      end
  end

end


