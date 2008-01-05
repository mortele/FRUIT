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
  
  SRC = FileList['*.f90']
  OBJ = SRC.ext('o')
  module_with_path=[]
  SRC.ext('mod').each do |file| 
    module_with_path << "#{$build_dir}/#{file}" 
  end
  
  lib_base_files =[]
  $lib_bases.each_pair { |key, value| lib_base_files << "#{$build_dir}/lib#{key}.a" }
  
  lib_name_flag = ''
  $lib_bases.keys.each { |key| lib_name_flag += "-l#{key} " }
  
  lib_dir_flag = ''
  _libs=[]
  $lib_bases.each_pair do |key, value| 
    value = $build_dir if value == nil
    _libs << value
  end
  _libs.uniq.each { |value|  lib_dir_flag += "-L#{value} " }
  
  CLEAN.include(['*.o', '*.a', '*.mod', module_with_path])
  CLOBBER.include("#{$build_dir}/#{$goal}")
  
  task :default => [:deploy]
  task $goal => lib_base_files
  
  rule '.o' => '.f90' do |t|
    Rake::Task[:dirs].invoke
    sh "#{$compiler} -c -o #{t.name} #{t.source} -module #{$build_dir} #{lib_name_flag} #{lib_dir_flag}"
  end
  
  file $goal => OBJ do
    if $goal =~ /.a$/
      sh "ar cr #{$goal} #{OBJ}"
    else
      sh "#{$compiler} -o #{$goal} #{OBJ} -module #{$build_dir} #{lib_name_flag} #{lib_dir_flag}"
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
