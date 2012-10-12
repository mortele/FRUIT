#!/usr/bin/env ruby

include Rake::DSL if defined?(Rake::DSL)

class GuiWindow
  attr_accessor :btn_exit, :btn_run, :btn_open_dir, :btn_fruitf90
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
      text "Open Directry"
      pack(:side => :left)
    }
    @btn_fruitf90 = TkButton.new(frame_top){
      text "location of fruit.f90"
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
    @btn_open_dir.text("Open Directry [Ret]")
  end

  def btn_open_dir_normal
    @btn_open_dir.text("Open Directry")
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

    methods = @core.fp.get_methods_of_filename(f)

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
        @core.run_fruit(checks, @core.fp, all_f)
     }
    self.btn_run.bind('Return', 
      proc {
        @core.run_fruit(checks, @core.fp, all_f)
      }
    )
  end
  def show_dirname_of_tester dirname
    self.text_left.value = dirname + "\n"
  end
end


class GuiCore
  require "rubygems"
  require 'fruit_processor'
  require 'rake/clean'
  require 'open3'

  attr_accessor :window, :dir_fruit_f90, :fp

  def show_dir_rake_base
    return @dir_rake_base
  end

  def open_dir_and_update
    dirname = @window.choose_dir 
    return if !dirname
  
    reload_all_f(dirname)

    return dirname
  end

  def reload_all_f(dirname)
    checks = []

    @window.add_text("Reading Directry #{dirname}\n")
    @window.show_dirname_of_tester(dirname)

    @dirname = dirname
#    @dirname.gsub!(/ /, "\ ")

    @fp = ""
    FileUtils.cd(dirname){
      @fp = FruitProcessor.new
      @fp.pre_process
    }

    all_f = []
    FileUtils.cd(dirname){
      all_f = @fp.get_files
    }

    @ready_testers = false
    @ready_fruitf90 = false
    @ready_rake_base = false

    if all_f.size == 0
      @ready_testers = false
      @window.add_text("Tester not found. Choose directry that contains test_*.f90, test_*.f95, test_*.f03, or test_*.f08.\n")
      @window.disable_fruit_run
    else
      @ready_testers = true
      @window.add_text("Tester found:\n")
      all_f.each{|f|
        check = @window.add_one_testf90(f)
        checks << check
      }
    end
  
    self.check_if_ready

    @window.set_btn_clear_and_run(checks, all_f)

    return all_f
  end

  def if_ready_fruitf90
    return @ready_fruitf90 if @ready_fruitf90

    if (@dirname and
        File.exist?(@dirname + "/fruit.f90") and 
        File.exist?(@dirname + "/fruit_util.f90")
    )
      @window.add_text(
        "#{@dirname}/fruit.f90 and \n#{@dirname}/fruit_util.f90 are used.\n")
      @ready_fruitf90 = @dirname
    elsif (
      @dir_fruit_f90 and 
      File.exist?(@dir_fruit_f90 + "/fruit.f90") and
      File.exist?(@dir_fruit_f90 + "/fruit_util.f90")
    )
      @window.add_text(
        "#{@dir_fruit_f90}/fruit.f90 and \n#{@dir_fruit_f90}/fruit_util.f90 are used.\n")
      @ready_fruitf90 = @dir_fruit_f90
    else
      @window.add_text(
        "At #{@dirname}, fruit.f90 or fruit_util.f90 is not found.\n")
      if @dir_fruit_f90
        @window.add_text(
          "At #{@dir_fruit_f90}, fruit.f90 or fruit_util.f90 is not found.\n")
        @ready_fruitf90 = false
      end
    end
  end

  def choose_fruitf90dir dir_fruit_f90
    @dir_fruit_f90 = dir_fruit_f90
    self.check_if_ready
  end

  def check_if_ready
    if not @dirname
      @window.disable_fruit_run
      return false
    end

    if self.if_ready_fruitf90
      @window.enable_fruitf90_select
    else
      @window.add_text("Press [location of fruit.f90] above\n")
      @window.enable_fruitf90_select
    end

    #------ rake_base.rb exists? ------v
    if not @ready_rake_base
      FileUtils.cd(@dirname){
        if File.exist?("rake_base.rb")
          @window.add_text(
            "rake_base.rb is found at " + File.expand_path(@dirname) +"\n"
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

    if @ready_testers and @ready_fruitf90 and @ready_rake_base
      @window.add_text("Ready to run FRUIT.\n")
      @window.enable_fruit_run
      return true
    else
      @window.disable_fruit_run
      return false
    end
  end
  
  def writeout_rakefile(filename, all_f, dir_fruit_f90, dir_rake_base)
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
        @window.add_text_warn(f + " not found.\n")
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
  file 'fruit_basket_gen.o' => ['rakefile_gui_tmp']
  
  task :default => [:test]
  
  task :test => $goal do
    sh "./#{$goal}"
    File.delete("fruit_basket_gen.o")
  end
  
  task :valgrind => $goal do
    sh "valgrind --leak-check=full ./#{$goal}"
    File.delete("fruit_basket_gen.o")
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
  
  def run_fruit (checks, fp, all_f)
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
  
    FileUtils.cd(@dirname){
      if_wrote = writeout_rakefile("rakefile_gui_tmp", files_to_process, @dir_fruit_f90, @dir_rake_base)
      if not if_wrote
        return
      end
    }
  
    Thread.new{
      @window.fruit_running
      FileUtils.cd(@dirname){

#        Open3.popen3("rake clean -f rakefile_gui_tmp") do |stdin, stdout, stderr|
#          stderr.each do |line|
#            @window.add_text_err(line)
#          end
#          stdout.each do |line|
#            @window.add_text(line)
#          end
#        end
  
        Open3.popen3("rake -f rakefile_gui_tmp") do |stdin, stdout, stderr|
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
    dirname_got = Tk::chooseDirectory{
      initialdir "./"
    }
    if dirname_got == ""
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
      "Press [Open Directry] and open directry which contains " + 
      "tester programs\n"
    )
    @window.btn_open_dir_setup
    @window.btn_open_dir_activate

    @ready_testers = false
    @ready_fruitf90 = false
    @ready_rake_base = false

    @dirname = false

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
