#!/usr/bin/env ruby

# Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
# All rights reserved.
# Licensed under the 3-clause BSD license.

class FruitRakeEstimate
  attr_accessor :extensions, :forward, :all_f, :identifiers, :source_dirs

  EXT_DEFAULT = ["f90", "f95", "f03", "f08"]
  IDENTIFIERS_DEFAULT = []

  def initialize
    @extensions = EXT_DEFAULT
    @identifiers = IDENTIFIERS_DEFAULT
    @source_dirs = [""]
  end

  def rake_dependency
    self.set_all_f
    self.set_forward
    self.apply_dependency
  end


  def set_all_f
    @all_f = []

    @extensions.each{|fxx|
      @source_dirs.each{|dir|
        Dir::glob("#{dir}*.#{fxx}").each{|filename|
          @all_f << filename
        }
      }
    }

    # puts "fortran files:"
    # puts all_f.join(" ")
  end

  def missing_modules
    forward, missings = self.set_forward_and_missings
    return missings
  end

  def set_forward
    forward, missings = self.set_forward_and_missings
    return forward
  end

  def set_forward_and_missings
    mod_in_f = {}
    f_uses_mod = {}

    if (!@all_f)
      set_all_f
    end

    @all_f.each{|filename|
      f_base = filename.sub(/^.*\//, "")

      f_uses_mod[ f_base ] = []


      macro_stack = []

      open(filename, 'r'){|f|
        f.each_line{|line|
          if if_macro_stack(macro_stack)
            if line =~ /(?:^|\r|\n)\s*use +(\w+)\b?/i
              f_uses_mod[ f_base ] << $1
            end
            if line =~ /(?:^|\r|\n)\s*module +(\w+)\b?/i
              mod = $1
              mod_in_f[ mod ] = f_base unless mod =~ /procedure/i
            end
          end

          if line =~ /(?:^|\r|\n)\s*#/
            macro_stack = parse_sharp_line_stack(line, macro_stack)
          end
        }
      }
    }
    @forward = {}
    missings = []
    @all_f.each{|f|
      f_base = f.sub(/^.*\//, "")

      @forward[ f_base ] = []

      f_uses_mod[f_base].uniq.each{|a_mod|
        if mod_in_f[a_mod]
          if mod_in_f[a_mod] != f_base
            @forward[ f_base ] << mod_in_f[ a_mod ]
          end
        else
          missings.push(a_mod)
        end
      }
    }
    return @forward, missings.uniq
  end

  def if_macro_stack(macro_stack)
    macro_stack.each{|item|
      if item =~ /^#not#(.*)$/
        not_item = $1
        return false if (@identifiers.include?(not_item))
      else
        return false if !(@identifiers.include?(item))
      end
    }
    return true
  end

  def parse_sharp_line_stack(line, macro_stack)
    if line =~ /(?:^|\r|\n)\s*#ifdef +(\w+)\b/i
      macro_stack.push($1)
    end

    if line =~ /(?:^|\r|\n)\s*#ifndef +(\w+)\b/i
      str = "#not#" + $1
      macro_stack.push(str)
    end

    if line =~ /(?:^|\r|\n)\s*#else\b/i
      if macro_stack.size == 0
        puts "FruitRakeEstimate: macro #else unexpected here."
      else
        id_now = macro_stack.pop
        if id_now =~ /^#not#(.*)$/
          id_now = $1
        else
          id_now = "#not#" + id_now
        end
        macro_stack.push(id_now)
      end
    end

    if line =~ /(?:^|\r|\n)\s*#endif\b/i
      if macro_stack.size == 0
        puts "FruitRakeEstimate: macro #else unexpected here."
      else
        macro_stack.pop
      end
    end

    if line =~ /(?:^|\r|\n)\s*#define +(\w+)\b/i
      puts "FruitRakeEstimate: macro #define is not supported."
    end
    return macro_stack
  end

  def apply_dependency
    puts "=========="
    puts "Dependencies Estimated:"
    @all_f.each{|f|
      f_base = f.sub(/^.*\//, "")
      next if @forward[f_base].size == 0

      needs = f_to_o(f_base)
      needed = []

      @forward[f_base].each{|f_needed|
        needed << f_to_o(f_needed)
      }
      file needs => needed if defined?(Rake)

      #for rake
      needed_str = "['" + needed.join("', '") + "']"
      print "  file '#{needs}' => #{needed_str}\n"

    #   #for makefile
    #   print needs + " : " + needed.join(" ") + "\n"
    }
    puts "=========="
  end


  def f_to_o(name)
    @extensions.each{|fxx|
      if name =~ /.#{fxx}/
        return name.sub(".#{fxx}", ".o")
      end
    }
    return nil
  end

  def get_needed(fortrans)
    if (!@forward)
      set_forward
    end

    f_add = []
    fortrans.each{|f|
      next if not @forward[f]
      @forward[f].each{|f_plus|
        f_add << f_plus if !fortrans.index(f_plus)
      }
    }
    if f_add.size == 0
      return fortrans.uniq
    end
    get_needed(fortrans + f_add)
  end

  def get_ordered(needed, ordered = [])
    f_add = []

    (needed - ordered).each{|f|

      next if ordered.index(f)
      if @forward[f].size == 0
        f_add << f
      elsif (ordered & @forward[f]).sort == (@forward[f]).sort
        f_add << f
      else
      end
    }
    if f_add.size == 0
      return ordered.uniq
    end
    get_ordered(needed, ordered + f_add)
  end

  def src_and_obj_for_main(main)
    if defined?(main) and main
      needed = get_needed( [main] )
      ordered_f = get_ordered(needed)

      #replacing OBJ with ordered_o
      if defined?(FileList)
        if ordered_f.size > 0
          src = FileList[]
          ordered_f.each{|f|
            src.concat(FileList[f])
          }
          obj = src.ext('o')
        end
      end
      return [src, obj]
    end
    return nil
  end
end

#----- execute the following if loaded within rake
if $0 =~ /rake$/
  estim = FruitRakeEstimate.new

  if $source_dirs
    estim.source_dirs = $source_dirs.push("").uniq
  elsif $source_dir
    estim.source_dirs = [$source_dir]
  else
    estim.source_dirs = [""]
  end
  estim.rake_dependency
  if $main
    SRC, OBJ = estim.src_and_obj_for_main($main)
  end
end

#eof
