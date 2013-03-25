#!/usr/bin/env ruby
#require 'ftools'  #ftools is unavailable in ruby 1.9
require 'fileutils'

module Misc
  def get_os
    if RUBY_PLATFORM =~ /linux/i
      "Linux"
    elsif RUBY_PLATFORM =~ /darwin/i
      "Mac"
    elsif RUBY_PLATFORM =~ /mswin/i
      "Windows"
    elsif RUBY_PLATFORM =~ /cygwin/i
      "Cygwin"
    elsif RUBY_PLATFORM =~ /mingw/i
      "Mingw"
    end

    #sample: 
    #i386-cygwin
    #i386-mingw32
    #universal.x86_64-darwin10.8.0
  end
  
  def is_windows?
    get_os == "Windows"
  end
  
  def is_linux?
    get_os == "Linux"
  end
  
  def is_mac?
    get_os == "Mac"
  end
  
  def convert_windows_file_to_url path
    path.gsub(/\//, '\\') if is_windows?
  end
  
  def file_url file_path
    "file:///#{file_path}"
  end
  
  def svn_version
    if is_windows?
      ""
    else
      "Version: #{`svn info`.scan(/Revision: (\d+)/)}"
    end
  end
  
  def os_install from, to

# #until fruit3.0.2
#    if is_windows?
#      install(from, to)
#    else
#      ln_sf(from, to)
#    end

    begin
      FileUtils.ln_sf(from, to)
    rescue NotImplementedError
     FileUtils.install(from, to)
    end

  end
  
end
include Misc
