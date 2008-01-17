#!/usr/bin/env ruby

require 'rubygems'
require 'rake'

class FruitProcessor
  
  def initialize
    @driver_program_name='fruit_driver_gen'
    @fruit_basket_module_name = 'fruit_basket_gen'
    @files = FileList['*_test.f90']
    @spec_hash={}
    @files.each do |file|
      grep_test_method_names file
    end
    get_specs
  end
  
  def process_file
  end
  
  def pre_process
    fruit_picker
    create_driver
  end
  
  # create one fruit basket in one directory, this can go multiple directories
  def fruit_picker lib_name=nil
    test_subroutine_names=[]
    fruit_file = "#{@fruit_basket_module_name}.f90"
    File.open(fruit_file, 'w') do |f| 
      f.write "module #{@fruit_basket_module_name}\n"
      f.write "  use fruit\n"
      f.write "contains\n"
    end
    
    File.open(fruit_file, 'a') do |f| 
      @files.each do |file|
        test_module_name=file.gsub(".f90", "")
        subroutine_name="#{test_module_name}_all_tests"
        test_subroutine_names << subroutine_name
        f.write "  subroutine #{subroutine_name}\n"
        f.write "    use #{test_module_name}\n"
        f.write "\n"
        
        method_names = @spec_hash[file]['methods']['name']
        
        if @spec_hash[file]['setup'] != nil
          if @spec_hash[file]['setup']=='all'
            f.write "    call setup_before_all\n"
          end
        end        
        
        spec_counter = 0
        method_names.each do |method_name|
          if @spec_hash[file]['setup'] != nil
            if @spec_hash[file]['setup']=='each'
              f.write "    call setup\n"
            end
          end
          f.write "    write (*, *) \"  ..running test: #{method_name}\"\n"
          f.write "    call set_unit_name ('#{method_name}')\n"
          f.write "    call #{method_name}\n"
          f.write "    if (.not. is_last_passed()) then\n"
          f.write "      write(*,*) \n"
          f.write "      write(*,*) '  Un-satisfied spec:'\n"
          f.write "      write(*,*) '#{format_spec(@spec_hash[file]['methods']['spec'][spec_counter], '  -- ', '&')}'\n"
          f.write "      write(*,*) \n"
          f.write "    end if\n"
          
          if @spec_hash[file]['setup'] != nil
            if @spec_hash[file]['teardown']=='each'
              f.write "    call teardown\n"
            end
          end
          f.write "\n"
          spec_counter += 1
        end
        
        if @spec_hash[file]['setup'] != nil
          if @spec_hash[file]['teardown']=='all'
            f.write "    call teardown_after_all\n"
          end
        end
        
        f.write "  end subroutine #{subroutine_name}\n"
        f.write "\n"
        
      end
    end
    
    File.open(fruit_file, 'a') do |f| 
      f.write "  subroutine fruit_basket\n"
      test_subroutine_names.each do |test_subroutine_name|
        f.write "    call #{test_subroutine_name}\n"
      end
      f.write "  end subroutine fruit_basket\n"
      f.write "\n"
      f.write "end module #{@fruit_basket_module_name}"
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
  
  # look into all files lib_test_*.a in build_dir, then generate driver files
  def create_driver build_dir=nil
    File.open("#{@driver_program_name}.f90", 'w') do |f| 
      f.write "program #{@driver_program_name}\n"
      f.write "  use fruit\n"
      f.write "  use #{@fruit_basket_module_name}\n"
      f.write "  call init_fruit\n"
      
      f.write "  call fruit_basket\n"
      
      f.write "  call fruit_summary\n"
      f.write "end program #{@driver_program_name}\n"
    end
  end
  
  def get_specs
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
              elsif end_match(spec_var, '\'')
                spec_var.chop!
              end 
            end # end of inside subroutine lines
            
            if spec_var == nil
              spec=subroutine_name.gsub('test_', '').gsub('_', ' ')
            else
              spec = spec_var
            end
            
            @spec_hash[file]['methods']['spec'] << spec
          end # end of test match
        end # end of each line in file
      end # end of file open
    end # end of each file
  end
  
  def end_match (string, match)
    return false if string == nil or string.length ==1
    return string[string.length-1, string.length-1] == match
  end
  
  def spec_report
    puts "\n"
    puts "All executable specifications from tests:"
    @spec_hash.keys.sort.each do |file|
      method_hash=@spec_hash[file]
      #@spec_hash[file]['methods']['spec']
      puts "  #{file.gsub('_test.f90', '')}"
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
