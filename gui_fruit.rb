#!/usr/bin/env ruby

require "rubygems"
require 'fruit_processor'
require 'rake/clean'
require 'open3'

include Rake::DSL if defined?(Rake::DSL)
require 'tk'

all_f = []
@dir_fruit_f90 = nil
@dir_rake_base = nil

EXTENSIONS = ["f90", "f95", "f03", "f08"]

#-------- bottom frame ------
frame_bot = TkFrame.new{
  pack("side" => "bottom")
}
button_exit = TkButton.new(frame_bot){
  text 'exit'
  pack('side' => "left", 'anchor' => 's', 'padx' => 20)
  command proc { exit }
}
btn_run = TkButton.new(frame_bot){
  text 'run FRUIT'
  pack('side' => "right", 'anchor' => 's', 'padx' => 20)
  state "disabled"
}
#-------- bottom frame ------

#-------- top frame ------
frame_top = TkFrame.new(nil){
  pack(:fill => :x, :side => :top)
}
btn_open_dir = TkButton.new(frame_top){
  text "Open Directry"
  pack(:side => :left)
}
btn_fruitf90 = TkButton.new(frame_top){
  text "location of fruit.f90"
  pack(:side => :left, 'padx' => 20)
  state "disabled"
}
btn_rake_base = TkButton.new(frame_top){
  text "location of rake_base.rb"
  pack(:side => :left, 'padx' => 20)
  state "disabled"
}
#-------- top frame ------

#======== middle paned ========v
paned_mid = TkPanedwindow.new(:orient => :horizontal){
  pack("side" => "top", "fill" => "both", "expand" => 1)
  showhandle 1
  sashwidth 3
  sashrelief "raised"
}


#--------- hidari frame --------
frm_left = TkFrame.new{ }

frm_left_sita = TkFrame.new(frm_left){
  pack("side" => "bottom")
}

frm_left_ue = TkFrame.new(frm_left){
  borderwidth 2
  relief "groove"
  pack(:side => :top)
}

scr_left = TkScrollbar.new(frm_left_ue).pack(:side => :right, :fill => :y)
text_left = TkText.new(frm_left_ue){
  pack(:side => :left, :fill => :y)
  bindtags(bindtags - [TkText])
  configure('takefocus', 0)
}
text_left.yscrollbar(scr_left)

btn_all   = TkButton.new(frm_left_sita){
  text 'all'
  pack('side' => 'left', 'anchor' => 's', 'padx' => 10)
}
btn_clear = TkButton.new(frm_left_sita){
  text 'clear'
  pack('side' => 'right', 'anchor' => 's', 'padx' => 10)
}
#--------- hidari frame --------

#--------- migi ----------
frm_right = TkFrame.new{
  borderwidth 2
  relief "groove"
}

scr_right = TkScrollbar.new(frm_right).pack("side" => "right", "fill" => "y")
textbox = TkText.new(frm_right){
  bindtags(bindtags - [TkText])
  configure('takefocus', 0)
  pack("side" => "top", "fill" => "both", "expand" => 1)
#---
#Either use 
#  bindtags(bindtags - [TkText])
#or
#  textbox.state "normal"
#    textbox.value += "...."
#  textbox.state "disabled"
#---
}
textbox.insert('end', 
  "Press [Open Directry] and open directry which contains " + 
  "tester programs\n"
)
textbox.see('end')
textbox.yscrollbar(scr_right)
textbox.tag_configure("tag_warn", :foreground=>'red')
#--------- migi ----------

paned_mid.add frm_left
paned_mid.add frm_right
#======== middle paned ========^


def reload_all_f(dirname, fp, text_left, 
  btn_fruitf90, btn_rake_base, btn_clear, btn_all, btn_run, textbox
)
  checks = []
  contents = []
  all_f = []

  text_left.value = dirname + "\n"

  dirname.gsub!(/ /, "\ ")
  FileUtils.cd(dirname){
    all_f = fp.get_files
  }
  textbox.insert('end', "Tester found:\n")
  textbox.see('end')

  all_f.each{|f|
    textbox.insert('end', "  " + File.basename(f) + "\n")
    textbox.see('end')
  
    #--- checkbox ---v
    check = TkVariable.new
    check.default_value = 1
    check.value = 1
    tcb = TkCheckButton.new(text_left){
      text File.basename(f)
      variable check
    }
    TkTextWindow.new(text_left, 'end', 'window'=>tcb)
    text_left.insert("end", "\n")
    checks << check
    #--- checkbox ---^

    methods = fp.get_methods_of_filename(f)

    methods.each{|method|
      text_left.insert('end', " "*4 + method + "\n")
      textbox.insert('end', " "*4 + method + "\n")
      textbox.see('end')
    }
  }

  #------ fruit.f90 exists? ------v
  FileUtils.cd(dirname){
    if not File.exist?("fruit.f90")
      textbox.insert('end', "fruit.f90 not found. ")
      if @dir_fruit_f90 and File.exist?(@dir_fruit_f90 + "/fruit.f90")
        textbox.insert('end', @dir_fruit_f90 + '/fruit.f90 is used. To change, ')
      end
      textbox.insert('end', "Press [location of fruit.f90] above\n")
      btn_fruitf90.state("normal")
      btn_fruitf90.command = 
        proc{
          @dir_fruit_f90 = find_file_using_btn(btn_fruitf90, "fruit.f90", textbox)
        }
      btn_fruitf90.bind('Return', 
        proc{
          @dir_fruit_f90 = find_file_using_btn(btn_fruitf90, "fruit.f90", textbox)
        }
      )
    end
  }
  #------ fruit.f90 exists? ------^
  #------ rake_base.rb exists? ------v
  FileUtils.cd(dirname){
    if not File.exist?("rake_base.rb")
      textbox.insert('end', "rake_base.rb not found. ")
      if @dir_rake_base and File.exist?(@dir_rake_base + "/rake_base.rb")
        textbox.insert('end', @dir_rake_base + '/rake_base.rb is used. To change, ')
      end
      textbox.insert('end', "Press [location of rake_base.rb] above\n")
      btn_rake_base.state("normal")
      btn_rake_base.command = 
        proc{
          @dir_rake_base = find_file_using_btn(btn_rake_base, "rake_base.rb", textbox)
        }
      btn_rake_base.bind('Return', 
        proc{
          @dir_rake_base = find_file_using_btn(btn_rake_base, "rake_base.rb", textbox)
        }
      )
    end
  }
  #------ rake_base.rb exists? ------^

  #------ behavior ------v
  btn_clear.command = proc { 
    checks.each{|check|
      check.value = "nil"
    }
  }
  btn_all.command = proc { 
    checks.each{|check|
      check.value = 1
    }
  }
  btn_run.command =
    proc {
      run_fruit(textbox, checks, dirname, fp, all_f, @dir_fruit_f90, @dir_rake_base)
   }
  btn_run.bind('Return', 
    proc {
      run_fruit(textbox, checks, dirname, fp, all_f, @dir_fruit_f90, @dir_rake_base)
    }
  )
  #------ behavior ------^
end

def writeout_rakefile(filename, all_f, dir_fruit_f90, dir_rake_base, textbox)
  if dir_fruit_f90
    if !File.exist?("fruit.f90")
      File.symlink(dir_fruit_f90 + "/fruit.f90", "fruit.f90")
    end
    if !File.exist?("fruit_util.f90")
      File.symlink(dir_fruit_f90 + "/fruit_util.f90", "fruit_util.f90")
    end
  end

  which_rake_estimate = "rake_estimate.rb"
  which_rake_base     = "rake_base.rb"
  if dir_rake_base
    which_rake_estimate = dir_rake_base + "/rake_estimate.rb"
    which_rake_base     = dir_rake_base + "/rake_base.rb"
  end

  files_needed = [which_rake_base, which_rake_estimate, "fruit.f90", "fruit_util.f90"]
  files_needed.each{|f|
    if !File.exist?(f)
      textbox.insert("end", f + " not found.\n", "tag_warn")
      return nil
    end
  }

  File::open(filename, 'w'){|f|
    f.write <<-'EOS'
require 'rubygems'
require 'fruit_processor'
require 'rake/clean'

include Rake::DSL if defined?(Rake::DSL)

fp = FruitProcessor.new
    EOS

    f.write "fp.process_only = "
    f.write all_f.inspect
    f.write "\n"

    f.write <<-'EOS'
fp.pre_process

$build_dir = ""  #If not set, build will be done in ../build/
$goal = "fruit_driver_gui.exe"

task :default => [:test]

task :test => $goal do
  sh "./#{$goal}"
end

task :valgrind => $goal do
  sh "valgrind --leak-check=full ./#{$goal}"
end

CLEAN.include($goal)

    EOS
    f.write <<-"EOS"
$main = "fruit_driver_gen.f90"
load "#{which_rake_estimate}"

load "#{which_rake_base}"
include RakeBase
    EOS
  }
  return "done"
end

def run_fruit(textbox, checks, dirname, fp, all_f, dir_fruit_f90, dir_rake_base)
  textbox.insert("end", "Running FRUIT\n")
  textbox.see('end')
  files_to_process = []
  checks.each_with_index{|check, i|
    if check
      if check.value == "1"
        files_to_process << all_f[i]
      end
    end
  }
  textbox.insert("end", "files_to_process:\n")
  if (files_to_process.size < 1)
    textbox.insert("end", "none\n")
    textbox.see('end')
    return nil
  else
    textbox.insert("end", FileList[files_to_process].to_s + "\n")
    textbox.see('end')
  end
  Tk.update

  FileUtils.cd(dirname){
    if_wrote = writeout_rakefile("rakefile_gui_tmp", files_to_process, dir_fruit_f90, dir_rake_base, textbox)
    return if not if_wrote
    Open3.popen3("rake clean -f rakefile_gui_tmp") do |stdin, stdout, stderr|
      stderr.each do |line|
        textbox.insert('end', line, "tag_stderr")
        textbox.see('end')
        textbox.tag_configure("tag_stderr", :foreground=>'red')
      end
    end
    Open3.popen3("rake -f rakefile_gui_tmp") do |stdin, stdout, stderr|
      stderr.each do |line|
        textbox.insert('end', line, "tag_stderr")
        textbox.see('end')
        textbox.tag_configure("tag_stderr", :foreground=>'red')
      end
      stdout.each do |line|
        textbox.insert('end', line)
        textbox.see('end')
      end
    end
  }
end

def open_directry
  dirname = Tk::chooseDirectory{
    initialdir "./"
  }
  if dirname == ""
    return nil 
  end

  fp = ""
  FileUtils.cd(dirname){
    fp = FruitProcessor.new
    fp.pre_process
  }
  return dirname, fp
end

def find_file_using_btn (btn, file_to_find, textbox)
  dirname_got = Tk::chooseDirectory{
    initialdir "./"
  }
  if dirname_got == ""
    return nil 
  end

  FileUtils.cd(dirname_got){
    if File.exist?(file_to_find)
      btn.state("normal")
      textbox.insert("end", file_to_find + " found.\n")
      return dirname_got
    end
  }
  return nil
end

def open_dir_and_update(text_left, btn_open_dir, btn_fruitf90, btn_rake_base, btn_clear, btn_all, btn_run, textbox)
  (dirname, fp) = open_directry
  return if !dirname

  textbox.value += "Reading Directry #{dirname}\n"
  textbox.see('end')

  reload_all_f(
    dirname, fp,
    text_left, btn_fruitf90, btn_rake_base, btn_clear, btn_all, btn_run, textbox
  )
  btn_run.focus
  btn_run.text("run FRUIT [Ret]")
  btn_run.state("normal")
  btn_open_dir.text("Open Directry")
end

def initial (
  text_left, btn_open_dir, btn_fruitf90, btn_rake_base, btn_clear, btn_all, btn_run, textbox
)
  btn_open_dir.command = proc{
    open_dir_and_update(
      text_left, btn_open_dir, btn_fruitf90, btn_rake_base, btn_clear, btn_all, btn_run, textbox
    )
  }

  btn_open_dir.bind('Return', 
    proc{
      open_dir_and_update(
        text_left, btn_open_dir, btn_fruitf90, btn_rake_base, btn_clear, btn_all, btn_run, textbox
      )
      Tk.callback_break
    }
  )
  btn_open_dir.focus
  btn_open_dir.text("Open Directry [Ret]")

  btn_fruitf90.command = proc{
    textbox.insert("end", 
      "This button intended to set path to fruit.f90. To be implemented\n"
    )
  }
end

initial(
  text_left, btn_open_dir, btn_fruitf90, btn_rake_base, btn_clear, btn_all, btn_run, textbox
)

Tk.mainloop

