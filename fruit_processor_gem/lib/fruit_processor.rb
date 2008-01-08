#!/usr/bin/env ruby

require 'rubygems'
require 'rake'

class FruitProcessor
  
  def initialize
    @driver_file='fruit_driver_gen_fruit'
    @files = FileList['*_test.f90']
    @source_file_names=[]
    @spec_hash={}
    @files.each do |file|
      grep_test_method_names file
    end
    get_specs
    
  end
  
  def process_file
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
        
        method_names = @spec_hash[file]['methods']['name']
        
        if @spec_hash[file]['setup']=='all'
          f.write "       call setup_before_all\n"
        end
        
        method_names.each do |method_name|
          if @spec_hash[file]['setup']=='each'
            f.write "       call setup\n"
          end
          f.write "       write (*, *) \"  ..running #{method_name}\"\n"
          f.write "       call #{method_name}\n"
          # get failed count, added failed spec name into array
          if @spec_hash[file]['teardown']=='each'
            f.write "       call teardown\n"
          end
          f.write "\n"
        end
        
        if @spec_hash[file]['teardown']=='all'
          f.write "       call teardown_after_all\n"
        end
        
        f.write "     end subroutine all_#{module_name}\n"
        f.write "end module #{module_name}\n"
      end
    end
  end
  
  def grep_test_method_names file_name
    File.open(file_name, 'r') do |source_file|
      @spec_hash[file_name]={}
      @spec_hash[file_name]['methods'] = {}
      @spec_hash[file_name]['methods']['name'] =[]
      @spec_hash[file_name]['methods']['spec'] =[]
      
      source_file.grep( /^\s*subroutine\s*(\w+)\s*$/i ) do |dummy|
        subroutine_name=$1
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
        @spec_hash[file_name]['methods']['name'] << subroutine_name
      end
    end
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
          if line =~ /^\s*subroutine\s+(\w+)\s*$/i
            subroutine_name=$1
            next if subroutine_name !~ /^test_/
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
              spec=subroutine_name.gsub('test_', '').gsub('_', ' ')
            else
              spec = spec_var
            end
            
            @spec_hash[file]['methods']['spec'] << spec
            
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
    "#{base_dir}/build"
  end
  
  def module_files(all_f90_files, build_dir)
    return [] if all_f90_files == nil or all_f90_files.size == 0
    module_with_path=[]
    # assume each source .f90 has a module.  This is a cleanup task, doesn't matter if removed non-existing file
    all_f90_files.ext('mod').each do |file| 
      module_with_path << "#{build_dir}/#{file}" 
    end
    return module_with_path
  end
  
  def lib_base_files(lib_bases, build_dir)
    return if lib_bases == nil 
    lib_base_files =[]
    lib_bases.each_pair { |key, value| lib_base_files << "#{build_dir}/lib#{key}.a" }
    return lib_base_files
  end
  
  def lib_name_flag (lib_bases, build_dir)
    return if lib_bases == nil 
    lib_name_flag = ''
    lib_bases.keys.each { |key| lib_name_flag += "-l#{key} " }
    return lib_name_flag
  end
  
  def lib_dir_flag(lib_bases, build_dir)
    return if lib_bases == nil 
    lib_dir_flag = ''
    _libs=[]
    lib_bases.each_pair do |key, value|
      value = build_dir if value == nil
      _libs << value
    end
    _libs.uniq.each { |value|  lib_dir_flag += "-L#{value} " }
    return lib_dir_flag
  end
  
end
