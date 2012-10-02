#!/usr/bin/env ruby

require "rubygems"
require 'fruit_processor'
require 'rake/clean'
require 'open3'

include Rake::DSL if defined?(Rake::DSL)
require 'tk'

class Gui_window
  attr_accessor :btn_exit, :btn_run, :btn_open_dir, :btn_fruitf90
    # :btn_rake_base
  attr_accessor :textbox, :text_left, :btn_all, :btn_clear
  attr_accessor :core

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

#    @btn_rake_base = TkButton.new(frame_top){
#      text "location of rake_base.rb"
#      pack(:side => :left, 'padx' => 20)
#      state "disabled"
#    }
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

  def btn_run_activate
    @btn_run.focus
    @btn_run.text("run FRUIT [Ret]")
    @btn_run.state("normal")

    self.btn_open_dir_normal
  end

  def btn_run_disable
    @btn_run.state = "disabled"
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
end


class Gui_core
  attr_accessor :window

  def reload_all_f(fp)
    text_left     = @window.text_left
    btn_fruitf90  = @window.btn_fruitf90 
    btn_clear     = @window.btn_clear
    btn_all       = @window.btn_all
    btn_run       = @window.btn_run
  
    checks = []
    contents = []
    all_f = []

    text_left.value = @dirname + "\n"
  
    @dirname.gsub!(/ /, "\ ")
    FileUtils.cd(@dirname){
      all_f = fp.get_files
    }
    @ready_testers = false
    @ready_fruitf90 = false
    @ready_rake_base = false

    if all_f.size == 0
      @ready_testers = false
      @window.add_text("Tester not found. Choose directry that contains test_*.f90, test_*.f95, test_*.f03, or test_*.f08.\n")
      @window.btn_run_disable
    else
      @ready_testers = true
      @window.add_text("Tester found:\n")
      all_f.each{|f|
        @window.add_text("  " + File.basename(f) + "\n")
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
          @window.add_text(       " "*4 + method + "\n")
        }
      }
    end
  
    self.check_if_ready

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
        run_fruit(btn_run, checks, @dirname, fp, all_f)
     }
    btn_run.bind('Return', 
      proc {
        run_fruit(btn_run, checks, @dirname, fp, all_f)
      }
    )
    #------ behavior ------^
  end

  def check_if_ready
    #------ fruit.f90 exists? ------v
    if not @ready_fruitf90
      FileUtils.cd(@dirname){
        if File.exist?("fruit.f90")
          @window.add_text(@dirname + '/fruit.f90 is used.' + "\n")
          @ready_fruitf90 = true
        else
          @window.add_text('fruit.f90 not found at "' + @dirname + '". ' + "\n")
          if @dir_fruit_f90 and File.exist?(@dir_fruit_f90 + "/fruit.f90")
            @window.add_text(@dir_fruit_f90 + '/fruit.f90 is used.' + "\n")
            @ready_fruitf90 = true
          elsif @dir_fruit_f90
            @window.add_text_warn('fruit.f90 not found at "' + @dir_fruit_f90 + '". ' + "\n")
            @ready_fruitf90 = false
          else
            @ready_fruitf90 = false
          end
        end
      }
    end

    if @ready_fruitf90
      @window.btn_fruitf90.state("disabled")
    else
      @window.add_text("Press [location of fruit.f90] above\n")
      @window.btn_fruitf90.state("normal")
    end
    #------ fruit.f90 exists? ------^
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
      @window.btn_run_activate
    else
      @window.btn_run_disable
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
  
  def run_fruit (btn_run, checks, dirname, fp, all_f)
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
  
    FileUtils.cd(dirname){
      if_wrote = writeout_rakefile("rakefile_gui_tmp", files_to_process, @dir_fruit_f90, @dir_rake_base)
      if not if_wrote
        return
      end
    }
  
    Thread.new{
      btn_run.state = "disabled"
      btn_run.text = "Running"
      FileUtils.cd(dirname){
        Open3.popen3("rake clean -f rakefile_gui_tmp") do |stdin, stdout, stderr|
          stderr.each do |line|
            @window.add_text_err(line)
          end
          stdout.each do |line|
            @window.add_text(line)
          end
        end
  
        Open3.popen3("rake -f rakefile_gui_tmp") do |stdin, stdout, stderr|
          stderr.each do |line|
            @window.add_text_err(line)
          end
          stdout.each do |line|
            @window.add_text(line)
          end
        end
      }
      btn_run.state = "normal"
      btn_run.text("run FRUIT")
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
  
  def open_dir_and_update
    (@dirname, fp) = open_directry
    return if !@dirname
  
    @window.add_text("Reading Directry #{@dirname}\n")
  
    reload_all_f(fp)
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

    @window.btn_fruitf90.command = 
      proc{
        @dir_fruit_f90 = find_file_using_btn(@window.btn_fruitf90, "fruit.f90")
        self.check_if_ready
      }
    @window.btn_fruitf90.bind('Return', 
      proc{
        @dir_fruit_f90 = find_file_using_btn(@window.btn_fruitf90, "fruit.f90")
        self.check_if_ready
      }
    )
  end
end
  
if $0 == __FILE__
  window = Gui_window.new
  core = Gui_core.new
  
  window.core = core
  core.initial(window)
  
  Tk.mainloop
end

#eof
