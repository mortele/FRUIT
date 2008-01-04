module RakeFruit
  require 'rubygems'
  require 'fruit_processor'
  require 'rake/clean'
  
  anchor_file_name='ROOT_ANCHOR'
  
  goal = '' if !goal
  
  $build_dir = find_build_dir if ! $build_dir
  
  SRC = FileList['*.f90']
  OBJ = SRC.ext('o')
  module_with_path=[]
  SRC.ext('mod').each do |file| 
    module_with_path << "#{$build_dir}/#{file}" 
  end
  
  CLEAN.include(['*.o', '*.a', '*.mod', module_with_path])
  CLOBBER.include("#{$build_dir}/#{goal}")
  
  rule '.o' => '.f90' do |t|
    Rake::Task[:dirs].invoke
    sh "ifort -c -o #{t.name} #{t.source} -module #{$build_dir}"
  end
  
  task :dirs do
    Dir.mkdir $build_dir unless File.exist?($build_dir)
  end  
  
  task :deploy do
    ln_sf("#{Dir.pwd}/#{goal}", "#{$build_dir}/#{goal}" )
  end
  
  task :anchor_root do
    require 'fileutils'
    FileUtils.touch anchor_file_name
  end
  
  def find_build_dir
    orig_path = Dir.pwd
    found_dir=''
    protection_counter = 0
    while true
      if File.exist? anchor_file_name 
        found_dir=Dir.pwd
        break
      end
      if Dir.pwd == "/" or protection_counter > 100
        FileUtils.cd (orig_path)
        FileUtils.cd ("../")
        found_dir=Dir.pwd
        break
      end
      FileUtils.cd ("../")
      protection_counter +=1
    end
    FileUtils.cd (orig_path)
    return found_dir + "/build"
  end
end
