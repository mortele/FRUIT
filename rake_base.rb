# TODO
#  1. Figure out how to do filee dependency
#      1) can be applied to 1) library dependencies, (all .f90 in the dir are rebuilt if lib_base is newer
#     2) make fruit basket not have name zz_

module RakeBase
  require 'rubygems'
  require 'fruit_processor'
  require 'rake/clean'
  
  anchor_file_name='ROOT_ANCHOR'
  $compiler = 'ifort'
  
  $goal = '' if !$goal
  
  $base_dir = FruitProcessor.new.base_dir if ! $base_dir
  $build_dir = FruitProcessor.new.build_dir if ! $build_dir
  
  $lib_bases = {} if !$lib_bases
  
  SRC = FileList['*.f90'].sort
  OBJ = SRC.ext('o')
  
  CLEAN.include(['*.o', '*.a', '*.mod', '*_gen.f90', '*fruit_driver', FruitProcessor.new.module_files(SRC, $build_dir)])
  CLOBBER.include("#{$build_dir}/#{$goal}")
  
  task :default => [:deploy]
  
  rule '.o' => ['.f90'] do |t|
    Rake::Task[:dirs].invoke if Rake::Task.task_defined?('dirs')
    sh "#{$compiler} -c -o #{t.name} #{t.source} -module #{$build_dir}"
  end
  
  file $goal => OBJ do
    if OBJ.size == 0
    elsif $goal =~ /.a$/
      sh "ar cr #{$goal} #{OBJ}"
    else
      sh "#{$compiler} -o #{$goal} #{OBJ} -module #{$build_dir} #{FruitProcessor.new.lib_name_flag($lib_bases, $build_dir)} #{FruitProcessor.new.lib_dir_flag($lib_bases, $build_dir)}"
    end
  end
  
  task :dirs do
    Dir.mkdir $build_dir unless File.exist?($build_dir)
  end  
  
  task :deploy => $goal do
    ln_sf("#{Dir.pwd}/#{$goal}", "#{$build_dir}/#{$goal}" )
  end
  
  task :anchor_root do
    require 'fileutils'
    FileUtils.touch anchor_file_name
  end
 
end
