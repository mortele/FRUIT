module RakeBase
  require 'rubygems'
  require 'fruit_processor'
  require 'rake/clean'
  
  anchor_file_name='ROOT_ANCHOR'
  
  goal = '' if !goal
  
  $base_dir = FruitProcessor.new.find_base_dir if ! $base_dir
  $build_dir = "#{base_dir}/build" if ! $build_dir
  
  lib_bases = {} if !lib_bases
  
  SRC = FileList['*.f90']
  OBJ = SRC.ext('o')
  module_with_path=[]
  SRC.ext('mod').each do |file| 
    module_with_path << "#{$build_dir}/#{file}" 
  end
  
  lib_base_files =[]
  lib_bases.each_pair { |key, value| lib_base_files << "#{BUILD_DIR}/lib#{key}.a" }
  
  lib_name_flag = ''
  lib_bases.keys.each { |key| lib_name_flag += "-l#{key} " }
  
  lib_dir_flag = ''
  lib_bases.values.uniq.each {|value| lib_dir_flag += "-L#{value} " }
  
  CLEAN.include(['*.o', '*.a', '*.mod', module_with_path])
  CLOBBER.include("#{$build_dir}/#{goal}")
  
  rule '.o' => '.f90' do |t|
    Rake::Task[:dirs].invoke
    sh "#{COMPILER} -c -o #{t.name} #{t.source} -module #{$build_dir} #{lib_name_flag} #{lib_dir_flag}"
  end
  
  file goal   do
    sh "#{COMPILER} -o #{goal} #{OBJ} -module #{$build_dir} #{lib_name_flag} #{lib_dir_flag}"
  end
  
  task :dirs do
    Dir.mkdir $build_dir unless File.exist?($build_dir)
  end  
  
  task :deploy => goal do
    ln_sf("#{Dir.pwd}/#{goal}", "#{$build_dir}/#{goal}" )
  end
  
  task :anchor_root do
    require 'fileutils'
    FileUtils.touch anchor_file_name
  end
  
end
