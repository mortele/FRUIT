require 'misc'

module RakeBase
  require 'rubygems'
  require 'fruit_processor'
  require 'rake/clean'
  require 'fileutils'

  include Rake::DSL if defined?(Rake::DSL)

  anchor_file_name='ROOT_ANCHOR'

  extensions = ["f90", "f95", "f03", "f08"]

  # Intel FORTRAN compiler tested on Linux
   $compiler = 'ifort'
   $option = "-check all -warn all"

  # GCC FORTRAN compiler tested on MacOs (10.6.8 Snow Leopard) and Windows Vista + cygwin
  #$compiler = "gfortran"
  #$option = "-Wall -Wextra -pedantic -fbounds-check -Wuninitialized -O"
  # With " -std=f95",
  # subroutines whose name is longer than 31 characters cause error.

  # G95 FORTRAN compiler tested on Linux and Windows Vista + cygwin
  #$compiler = "g95"
  #$option = "-Wall -Wobsolescent -Wunused-module-vars -Wunused-internal-procs -Wunused-parameter -Wunused-types -Wmissing-intent -Wimplicit-interface -pedantic -fbounds-check -Wuninitialized"

  if open("| which #{$compiler} 2>/dev/null"){|f| f.gets}
  #  puts "Fortran compiler " + $compiler + " exists."
  else
    puts "***** Fortran compiler " + $compiler + " not exists. *****"
    $compiler = "gfortran"
    $option = "-Wall -Wextra -pedantic -fbounds-check -Wuninitialized -O"
  end

  $goal = '' if !$goal

  $base_dir = FruitProcessor.new.base_dir if ! $base_dir
  $build_dir = FruitProcessor.new.build_dir if ! $build_dir

  $lib_bases = {} if !$lib_bases
  $inc_dirs = [] if !$inc_dirs

  #---------v
  if defined?(OBJ)
    p OBJ
  else
    SRC = FileList[]
    extensions.each{|fxx|
      SRC.concat(FileList['*.' + fxx])
    }
    SRC.sort!
    OBJ = SRC.ext('o')
  end
  #---------^

  #-------------v
  # assume a_test.fxx depends on a.fxx if a.fxx exists
  OBJ.each{|a_obj|
    a = a_obj.to_s
    b = a.sub(/_test\.o$/, "\.o")
    if a != b then
      extensions.each{|fxx|
        b_src = b.sub(/\.o$/, "\.#{fxx}")
        if File.exist?(b_src)
          puts "rake_base.rb: Assuming \"" + a + "\" depends on \"" + b + "\""
          file a => b
        end
      }
    end
  }
  #-------------^

  CLEAN.include(['*.o', '*.a', '*.mod', '*_gen.f90', '*fruit_driver', 'result*.xml',
    '*_gen.f90', FruitProcessor.new.module_files(SRC, $build_dir)])
  CLOBBER.include("#{$build_dir}/#{$goal}")

  task :default => [:deploy]

  # generated files must be built last
  #---------v
  # objs = FileList['*.f90'].ext('o')
  #--
  objs = FileList[]
  extensions.each{|fxx|
    objs.concat(FileList['*.' + fxx])
  }
  objs = objs.ext('o')
  #---------^
  if objs.include?'fruit_basket_gen.o'
    file 'fruit_basket_gen.o' =>  objs - ['fruit_basket_gen.o', 'fruit_driver_gen.o']
    file 'fruit_driver_gen.o' =>  'fruit_basket_gen.o'
  end

  # final goal link is depending on the libraries
  # file $goal => FruitProcessor.new.lib_base_files($lib_bases)
  # This is to resolve the .so files in LD_LIBRARY_PATH,
  # so if the .a file is not in the path, assume it is in one of the LD_LIBRARY_PATH
  FruitProcessor.new.lib_base_files($lib_bases).each do |lib|
    file $goal => lib if File.exist? lib
  end

  extensions.each{|fxx|
    rule '.o' => ['.' + fxx] do |t|
      Rake::Task[:dirs].invoke if Rake::Task.task_defined?('dirs')

      flag = $build_dir
      flag = '"-I' + flag + '"' if flag.size > 0

      sh "#{$compiler} #{$option} -c -o #{t.name} #{t.source} #{flag} #{FruitProcessor.new.inc_flag($inc_dirs)}"
      FileList["*.mod"].each do |module_file|
        if $build_dir != "" and $build_dir !~ /^\.\/*$/
          os_install File.expand_path(module_file), $build_dir
        end
      end
    end
  }

  file $goal => OBJ do
    if OBJ.size == 0
    elsif $goal =~ /.a$/
      sh "ar cr #{$goal} #{OBJ}"
    else
      #--------v
      #sh "#{$compiler} #{$option} \"-I#{$build_dir}\" -o #{$goal} #{OBJ} #{FruitProcessor.new.lib_name_flag($lib_bases, $build_dir)} \"#{FruitProcessor.new.lib_dir_flag($lib_bases, $build_dir)}\""
      #--
      lib_name_flag = FruitProcessor.new.lib_name_flag($lib_bases, $build_dir)

      lib_dir = FruitProcessor.new.lib_dir_flag($lib_bases, $build_dir)
      lib_dir = '"' + lib_dir + '"' if lib_dir.size > 0

      flag = $build_dir
      flag = '"-I' + flag + '"' if flag.size > 0

      sh "#{$compiler} #{$option} #{flag} -o #{$goal} #{OBJ} #{lib_name_flag} #{lib_dir}"
      #--------^
    end
  end

  # generate directories
  task :dirs do
    if $build_dir != "" and $build_dir !~ /^\.\/*$/
      Dir.mkdir $build_dir unless File.exist?($build_dir)
    end
  end

  task :deploy => $goal do
    if $goal.length > 0
      if $build_dir != "" and $build_dir !~ /^\.\/*$/
        os_install "#{Dir.pwd}/#{$goal}", "#{$build_dir}/#{$goal}"
      end
    end
  end

  task :anchor_root do
    FileUtils.touch anchor_file_name if not File.exist? anchor_file_name
  end

  task :gen do
    FruitProcessor.new.pre_process
  end

  task :spec do
    FruitProcessor.new.spec_report
  end

end
