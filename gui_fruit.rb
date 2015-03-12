#!/usr/bin/env ruby

# Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
# All rights reserved.
# Licensed under the 3-clause BSD license.

include Rake::DSL if defined?(Rake::DSL)

class GuiWindow
  attr_accessor :btn_exit, :btn_run
  attr_accessor :btn_open_dir, :btn_source_dir, :btn_fruitf90
  attr_accessor :textbox, :text_left, :btn_all, :btn_clear
  attr_accessor :checks
  attr_accessor :core

  require 'tk'

  def initialize
    #-------- bottom frame ------
    frame_bot = TkFrame.new{
      pack("side" => "bottom")
    }
    @btn_exit = TkButton.new(frame_bot){
      text 'exit'
      pack('side' => "left", 'anchor' => 's', 'padx' => 20)
      command proc { exit }
    }
    @btn_run = TkButton.new(frame_bot){
      text 'run FRUIT'
      pack('side' => "right", 'anchor' => 's', 'padx' => 20)
      state "disabled"
    }
    #-------- bottom frame ------
    
    #-------- top frame ------
    frame_top = TkFrame.new(nil){
      pack(:fill => :x, :side => :top)
    }
    @btn_open_dir = TkButton.new(frame_top){
      text "Tester Directry"
      pack(:side => :left)
    }

    @btn_source_dir = TkButton.new(frame_top){
      text "Source Directry"
      pack(:side => :left, 'padx' => 20)
      #state "disabled"
    }
    @btn_fruitf90 = TkButton.new(frame_top){
      text "location of fruit.f90"
      pack(:side => :left) ###, 'padx' => 20)
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
    @text_left = TkText.new(frm_left_ue){
      pack(:side => :left, :fill => :y)
      bindtags(bindtags - [TkText])
      configure('takefocus', 0)
    }
    @text_left.yscrollbar(scr_left)
    
    @btn_all   = TkButton.new(frm_left_sita){
      text 'all'
      pack('side' => 'left', 'anchor' => 's', 'padx' => 10)
    }
    @btn_clear = TkButton.new(frm_left_sita){
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

    @textbox = TkText.new(frm_right){
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
    @textbox.see('end')
    @textbox.yscrollbar(scr_right)
    @textbox.tag_configure("tag_warn", :foreground=>'red')
    @textbox.tag_configure("tag_stderr", :foreground=>'red')
    
    #--------- migi ----------
    
    paned_mid.add frm_left
    paned_mid.add frm_right
    #======== middle paned ========^
  end

  def add_text string
    @textbox.insert('end', string)
    @textbox.see('end')
    @textbox.update
  end

  def add_text_warn string
    @textbox.insert('end', string, "tag_warn")
    @textbox.see('end')
    @textbox.update
  end

  def add_text_err line 
    @textbox.insert('end', line, "tag_stderr")
    @textbox.see('end')
    @textbox.update
  end

  def enable_fruit_run
    @btn_run.focus
    @btn_run.text("run FRUIT [Ret]")
    @btn_run.state("normal")

    self.btn_open_dir_normal
  end

  def disable_fruit_run
    @btn_run.state = "disabled"
  end

  def fruit_running
    @btn_run.state = "disabled"
    @btn_run.text = "Running"
  end

  
  def fruit_run_done
    btn_run.state = "normal"
    btn_run.text("run FRUIT")
  end

  def btn_open_dir_activate
    @btn_open_dir.focus
    @btn_open_dir.text("Tester Directry [Ret]")
  end

  def btn_open_dir_normal
    @btn_open_dir.text("Tester Directry")
  end

  def btn_open_dir_setup
    @btn_open_dir.command = proc{
      @core.open_dir_and_update
    }
  
    @btn_open_dir.bind('Return', 
      proc{
        @core.open_dir_and_update
      }
    )
  end


  def btn_source_dir_setup
    @btn_source_dir.command = proc{
      @core.open_source_dir
    }
  end
  def enable_btn_source_dir
    @btn_source_dir.state("normal")
  end
  def disable_btn_source_dir
    @btn_source_dir.state("disabled")
  end
  

  def disable_fruitf90_select
    self.btn_fruitf90.state("disabled")
  end
  def enable_fruitf90_select
    self.btn_fruitf90.state("normal")
  end

  def choose_dir
    dirname = Tk::chooseDirectory{
      initialdir "./"
    }
    return dirname
  end

  def btn_fruitf90_setup
    self.btn_fruitf90.command = 
      proc{
        @core.choose_fruitf90dir(
          @core.find_file_using_btn(self.btn_fruitf90, "fruit.f90")
        )
      }
    self.btn_fruitf90.bind('Return', 
      proc{
        @core.choose_fruitf90dir(
          @core.find_file_using_btn(self.btn_fruitf90, "fruit.f90")
        )
      }
    )
  end

  def add_one_testf90 f
    self.add_text("  " + File.basename(f) + "\n")

    checks = []

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

    methods = @core.get_methods_of_filename(f)

    methods.each{|method|
      self.text_left.insert('end', " "*4 + method + "\n")
      self.add_text(               " "*4 + method + "\n")
    }
    return check
  end

  def set_btn_clear_and_run(checks, all_f)
    self.btn_clear.command = proc { 
      checks.each{|check|
        check.value = "nil"
      }
    }
    self.btn_all.command = proc { 
      checks.each{|check|
        check.value = 1
      }
    }
    self.btn_run.command =
      proc {
        @core.run_fruit(checks, all_f)
     }
    self.btn_run.bind('Return', 
      proc {
        @core.run_fruit(checks, all_f)
      }
    )
  end
  def show_dirname_of_tester tester_dirname
    self.text_left.value = tester_dirname + "\n"
  end
end


class GuiCore
  require "rubygems"
  require 'fruit_processor'
  require 'rake/clean'
  require 'open3'

  attr_accessor :window, :dir_fruit_f90, :source_dir

  @@extensions = ["f90", "f95", "f03", "f08"]
  @@lib_name = "src_gui_tmp"
  @@lib_filename = "lib" + @@lib_name + ".a"

  def get_methods_of_filename (f)
    methods = @fp.get_methods_of_filename(f)
    return methods
  end

  def show_dir_rake_base
    return @dir_rake_base
  end

  def open_dir_and_update
    tester_dir = @window.choose_dir 
    if !tester_dir or tester_dir == ""
      @window.add_text_warn("No directry selected.\n")
      return false
    end
    reload_all_f(tester_dir)
    return tester_dir
  end

  def open_source_dir
    source_dir = @window.choose_dir

    @source_dir = nil
    if !source_dir or source_dir == ""
      @window.add_text_warn("No directry selected.\n")
      return false
    end

    @window.add_text( "\nDirectry #{source_dir} selected.\n" )
    @source_dir = source_dir
    self.check_if_ready
    return true
  end

  def fetch_missing_mod(tester_dir, source_dir)
    self.if_source_exist(tester_dir) if !@esti
    
    missings = []
    FileUtils.cd(tester_dir){
      missings = @esti.missing_modules - ["fruit", "fruit_util"]
    }

    all_ok = true
    missings.each{|mod|
      candi = source_dir + "/" + mod + ".mod"
      link  = tester_dir + "/" + mod + ".mod"
      if File.exist?(candi)
        if !File.symlink? link
          @window.add_text "make symbolic link to " + candi + "\n"
          File.symlink(candi, link)
        end
      else
        @window.add_text_warn "module #{mod} missing.\n"
        all_ok = false
      end
    }
    return all_ok
  end

  def if_source_exist(tester_dir)
    if File.exist?(@dir_rake_base + "/rake_estimate.rb")
      require "#{@dir_rake_base}/rake_estimate"
    else
      raise "rake_estimate.rb needed at #{@dir_rake_base}"
    end

    missings = []
    FileUtils.cd(tester_dir){
      @esti = FruitRakeEstimate.new
      missings = @esti.missing_modules - ["fruit", "fruit_util"]
    }
    if missings.size == 0
      return true
    end

    @window.add_text_warn("Modules missing: " + missings.join(", ") + "\n")
    return false
  end


  def reload_all_f(tester_dir)
    checks = []

    return false if !File.exist? tester_dir 

    @window.add_text("Reading Directry #{tester_dir}\n")
    @window.show_dirname_of_tester(tester_dir)

    @tester_dir = tester_dir

    @fp = ""

    FileUtils.cd(tester_dir){
      @fp = FruitProcessor.new
      @fp.load_files
    }

    all_f = []
    FileUtils.cd(tester_dir){
      all_f = @fp.get_files
    }

    @ready_testers = false
    @ready_fruitf90 = false
    @ready_rake_base = false

    if all_f.size == 0
      @ready_testers = false
      @window.add_text_warn("Tester not found. Choose directry that contains test_*.f90, test_*.f95, test_*.f03, or test_*.f08.\n")
      @window.disable_fruit_run
      return []
    end

    FileUtils.cd(tester_dir){
      @fp.pre_process    ### here fruit_driver_gen.f90 is generated
    }

    @ready_testers = true
    @window.add_text("Tester found:\n")
    all_f.each{|f|
      check = @window.add_one_testf90(f)
      checks << check
    }
  
    @source_dir = nil
    if self.if_source_exist(@tester_dir)
      @window.disable_btn_source_dir
      @source_dir = ""
    else
      @window.add_text_warn("Tested source not found. \nPress [Source Directry] and choose a directry that contains sources tested by the testers.\n")
      @window.enable_btn_source_dir
    end
  
    self.check_if_ready

    @window.set_btn_clear_and_run(checks, all_f)

    return all_f
  end

  def if_ready_fruitf90
    return @ready_fruitf90 if @ready_fruitf90

    if (@tester_dir and
        File.exist?(@tester_dir + "/fruit.f90") 
    )
      @window.add_text(
        "#{@tester_dir}/fruit.f90 is used.\n")
      @ready_fruitf90 = @tester_dir
    elsif (
      @dir_fruit_f90 and 
      File.exist?(@dir_fruit_f90 + "/fruit.f90") 
    )
      @window.add_text(
        "#{@dir_fruit_f90}/fruit.f90 is used.\n")
      @ready_fruitf90 = @dir_fruit_f90
    else
      @window.add_text(
        "At #{@tester_dir}, fruit.f90 is not found.\n")
      if @dir_fruit_f90
        @window.add_text(
          "At #{@dir_fruit_f90}, fruit.f90 is not found.\n")
        @ready_fruitf90 = false
      end
    end
  end

  def choose_fruitf90dir dir_fruit_f90
    @dir_fruit_f90 = dir_fruit_f90
    self.check_if_ready
  end

  def check_if_ready
    if not @tester_dir
      @window.disable_fruit_run
      return false
    end

    if self.if_ready_fruitf90
      @window.enable_fruitf90_select
    else
      @window.add_text_warn("Press [location of fruit.f90] above\n")
      @window.enable_fruitf90_select
    end

    #------ rake_base.rb exists? ------v
    if not @ready_rake_base
      FileUtils.cd(@tester_dir){
        if File.exist?("rake_base.rb")
          @window.add_text(
            "rake_base.rb is found at " + File.expand_path(@tester_dir) +"\n"
          )
          @ready_rake_base = true
        elsif @dir_rake_base and File.exist?(@dir_rake_base + "/rake_base.rb")
          @window.add_text(
            @dir_rake_base + "/rake_base.rb is used.\n"
          )
          @ready_rake_base = true
        else
          @window.add_text_warn(
            "rake_base.rb is not found.\n"
          )
          @ready_rake_base = false
        end
      }
    end
    #------ rake_base.rb exists? ------^

    if @ready_testers and @ready_fruitf90 and @ready_rake_base and @source_dir
      @window.add_text("Ready to run FRUIT.\n")
      @window.enable_fruit_run
      return true
    else
      @window.disable_fruit_run
      return false
    end
  end


  def writeout_rakefile_source(source_dir, filename, build_dir = "./")
    filename = source_dir + "/" + filename

    File::open(filename, 'w'){|f|
      f.write <<-"EOS"
        require 'rubygems'
        require 'fruit_processor'

        include Rake::DSL if defined?(Rake::DSL)

        $goal='#{@@lib_filename}'
      EOS

      f.write "$build_dir='" + build_dir + "'\n"
      f.write <<-'EOS'
        load "#{FruitProcessor.new.base_dir}/rake_base.rb"
        load "#{FruitProcessor.new.base_dir}/rake_base_deps.rb"
      EOS
    }
  end
  
  def writeout_rakefile(filename, all_f, dir_fruit_f90, dir_rake_base)
    if dir_fruit_f90
      if !File.exist?("fruit.f90")
        File.symlink(dir_fruit_f90 + "/fruit.f90", "fruit.f90")
      end
      # if !File.exist?("fruit_util.f90")
      #   File.symlink(dir_fruit_f90 + "/fruit_util.f90", "fruit_util.f90")
      # end
    end
  
    which_rake_estimate = "rake_estimate.rb"
    which_rake_base     = "rake_base.rb"
    if dir_rake_base
      which_rake_estimate  = dir_rake_base + "/rake_estimate.rb"
      which_rake_base      = dir_rake_base + "/rake_base.rb"
      which_rake_base_deps = dir_rake_base + "/rake_base_deps.rb"
    end
  
    files_needed = [which_rake_base, which_rake_estimate, which_rake_base_deps, "fruit.f90"]
    files_needed.each{|f|
      if !File.exist?(f)
        @window.add_text_warn(f + " not found.\n")
        return nil
      end
    }
  
    File::open(filename, 'w'){|f|
      f.write <<-"EOS"
require 'rubygems'
require 'fruit_processor'
require 'rake/clean'
  
include Rake::DSL if defined?(Rake::DSL)
  
fp = FruitProcessor.new
      EOS
  
      f.write "fp.process_only = "
      f.write all_f.inspect
      f.write "\n"
  
      f.write <<-"EOS"
fp.pre_process
  
$build_dir = ""  #If not set, build will be done in ../build/
$goal = "fruit_driver_gui.exe"
      EOS

      if @source_dir and @source_dir != ""
        f.write "$lib_bases = [['#{@@lib_name}', '#{@source_dir}']]\n"
      end

      f.write <<-"EOS"
$main = "fruit_driver_gen.f90"
load "#{which_rake_estimate}"
  
load "#{which_rake_base}"
load "#{which_rake_base_deps}"
      EOS
      
      f.write <<-'EOS'
file 'fruit_basket_gen.' + $ext_obj => ['rakefile_gui_tester']
  
task :default => [:test]
  
task :test => $goal do
  sh "./#{$goal}"
  File.delete("fruit_basket_gen." + $ext_obj)
end
  
task :valgrind => $goal do
  sh "valgrind --leak-check=full ./#{$goal}"
  File.delete("fruit_basket_gen." + $ext_obj)
end
  
CLEAN.include($goal)
  
      EOS
    }
    return "done"
  end
  
  def run_fruit (checks, all_f)
    @window.add_text("Running FRUIT\n")
    files_to_process = []
    checks.each_with_index{|check, i|
      if check
        if check.value == "1"
          files_to_process << all_f[i]
        end
      end
    }
    @window.add_text("files_to_process:\n")
    if (files_to_process.size < 1)
      @window.add_text("none\n")
      return nil
    else
      @window.add_text(FileList[files_to_process].to_s + "\n")
    end
  
    if @source_dir and @source_dir != ""
      writeout_rakefile_source(@source_dir, "rakefile_gui_src", @tester_dir)
    end
  
    FileUtils.cd(@tester_dir){
      if_wrote = writeout_rakefile("rakefile_gui_tester", files_to_process, @dir_fruit_f90, @dir_rake_base)
      if not if_wrote
        return
      end
    }


    Thread.new{
      @window.fruit_running
      if @source_dir and @source_dir != ""
        FileUtils.cd(@source_dir){
#puts "Files in " + @source_dir
#p Dir.glob("*")
          Open3.popen3("rake -f rakefile_gui_src") do |stdin, stdout, stderr|
            stderr.each do |line|
              @window.add_text_err(line)
            end
            stdout.each do |line|
              @window.add_text(line)
            end
          end
        }
      end

      if (! self.fetch_missing_mod(@tester_dir, @source_dir))
        @window.add_text_warn("Somehow module still missing.\n")
        Thread.exit
      end

      FileUtils.cd(@tester_dir){
        Open3.popen3("rake -f rakefile_gui_tester") do |stdin, stdout, stderr|
          stderr.each do |line|
            @window.add_text_err(line)
          end
          stdout.each do |line|
            @window.add_text(line)
          end
        end
      }
      @window.fruit_run_done
    }
  end
  
  def find_file_using_btn (btn, file_to_find)
    dirname_got = @window.choose_dir

    if !dirname_got or dirname_got == ""
      @window.add_text_warn("No directry selected.\n")
      return nil 
    end
  
    FileUtils.cd(dirname_got){
      if File.exist?(file_to_find)
        btn.state("normal")
        @window.add_text(file_to_find + " found.\n")
        return dirname_got
      else
        @window.add_text_warn(file_to_find + " absent in " + dirname_got + ".\n")
      end
    }
    return nil
  end
  
  def initial (window)
    @dir_fruit_f90 = nil
    @dir_rake_base = File.expand_path(File.dirname(__FILE__))

    @window = window
    @window.add_text(
      "\nPress [Tester Directry] and open directry which contains " + 
      "tester programs\n"
    )
    @window.btn_open_dir_setup
    @window.btn_open_dir_activate

    @window.btn_source_dir_setup
    @window.disable_btn_source_dir

    @ready_testers = false
    @ready_fruitf90 = false
    @ready_rake_base = false

    @tester_dir = false
    @source_dir = nil
    @esti = nil

    @window.btn_fruitf90_setup
  end
end
  
if $0 == __FILE__
  window = GuiWindow.new
  core = GuiCore.new
  
  window.core = core
  core.initial(window)
  
  Tk.mainloop
end

#eof
