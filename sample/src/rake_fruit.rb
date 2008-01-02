module RakeFruit
  require 'rake/clean'

  SRC = FileList['*.f90']
  OBJ = SRC.ext('o')
  module_with_path=[]
  SRC.ext('mod').each do |file| 
    module_with_path << "#{BUILD_DIR}/#{file}" 
  end

  CLEAN.include(['*.o', '*.a', '*.mod', module_with_path])
  CLOBBER.include("#{BUILD_DIR}/#{GOAL}")

  rule '.o' => '.f90' do |t|
    Rake::Task[:dirs].invoke
    sh "ifort -c -o #{t.name} #{t.source} -module #{BUILD_DIR}"
  end

  task :dirs do
    Dir.mkdir BUILD_DIR unless File.exist?(BUILD_DIR)
  end  

  task :deploy do
    ln_sf("#{Dir.pwd}/#{GOAL}", "#{BUILD_DIR}/#{GOAL}" )
  end
end
