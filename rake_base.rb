require 'misc'

module RakeBase
  require 'rubygems'
  require 'fruit_processor'
  require 'rake/clean'
  require 'fileutils'

  include Rake::DSL if defined?(Rake::DSL)

  anchor_file_name='ROOT_ANCHOR'
  
  # Intel FORTRAN compiler tested on Linux
  $compiler = 'ifort'
  $option = "-check all -warn all"
  
  # GCC FORTRAN compiler tested on MacOs (10.3, Tiger)
  #$compiler = "gfortran"
  #$option = "-Wall -pedantic -fbounds-check -Wuninitialized"
  
  # G95 FORTRAN compiler tested on Linux
  #$compiler = "g95"
  #$option = "-Wall -pedantic -fbounds-check -Wuninitialized"
  
  $goal = '' if !$goal
  
  $base_dir = FruitProcessor.new.base_dir if ! $base_dir
  $build_dir = FruitProcessor.new.build_dir if ! $build_dir
  
  $lib_bases = {} if !$lib_bases
  $inc_dirs = [] if !$inc_dirs
  
  SRC = FileList['*.f90'].sort
  OBJ = SRC.ext('o')
  
  CLEAN.include(['*.o', '*.a', '*.mod', '*_gen.f90', '*fruit_driver', 
    '*_gen.f90', FruitProcessor.new.module_files(SRC, $build_dir)])
  CLOBBER.include("#{$build_dir}/#{$goal}")
  
  task :default => [:deploy]
  
  # generated files must be built last
  objs = FileList['*.f90'].ext('o')
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
  
  rule '.o' => ['.f90'] do |t|
    Rake::Task[:dirs].invoke if Rake::Task.task_defined?('dirs')
    sh "#{$compiler} #{$option} -c -o #{t.name} #{t.source} -I#{$build_dir} #{FruitProcessor.new.inc_flag($inc_dirs)}"
    FileList["*.mod"].each do |module_file|
      os_install File.expand_path(module_file), $build_dir
    end
  end
  
  file $goal => OBJ do
    if OBJ.size == 0
    elsif $goal =~ /.a$/
      sh "ar cr #{$goal} #{OBJ}"
    else
      sh "#{$compiler} #{$option}  -I#{$build_dir} -o #{$goal} #{OBJ} #{FruitProcessor.new.lib_name_flag($lib_bases, $build_dir)} #{FruitProcessor.new.lib_dir_flag($lib_bases, $build_dir)}"
    end
  end
  
  # generate directories
  task :dirs do
    Dir.mkdir $build_dir unless File.exist?($build_dir)
  end  
  
  task :deploy => $goal do
    if $goal.length > 0
      os_install "#{Dir.pwd}/#{$goal}", "#{$build_dir}/#{$goal}"
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
