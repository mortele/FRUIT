#!/usr/bin/env ruby


class FruitRakeEstimate
  attr_accessor :extensions, :forward, :all_f


  EXT_DEFAULT = ["f90", "f95", "f03", "f08"]

  def initialize
    @extensions = EXT_DEFAULT
  end

  def rake_dependency
    self.set_all_f
    self.set_forward
    self.apply_dependency
  end

  def set_all_f
    @all_f = []

    @extensions.each{|fxx|
      Dir::glob("*.#{fxx}").each{|filename|
        @all_f << filename
      }
    }

    #puts "fortran files:"
    #puts all_f.join(" ")
  end

  def set_forward
    mod_in_f = {}
    f_uses_mod = {}
  
    if (!@all_f)
      set_all_f
    end

    @all_f.each{|filename|
      f_uses_mod[ filename ] = []
      open(filename, 'r'){|f|
        f.each_line{|line|
          if line =~ /^\s*use +(\w+)\b?/i
            f_uses_mod[ filename ] << $1
          end
          if line =~ /^\s*module +(\w+)\b/i
            mod = $1
            mod_in_f[ mod ] = filename unless mod =~ /procedure/i
          end
          if line =~ /^\s*#if(n?)def +(\w+)\b/i
            puts "macro not supported."
          end
        }
      }
    }
    @forward = {}
    @all_f.each{|f|
      @forward[ f ] = []
      f_uses_mod[f].uniq.each{|a_mod|
        if mod_in_f[a_mod]
          @forward[ f ] << mod_in_f[ a_mod ]
        end
      }
    }
#p @forward
    return @forward
  end

  def apply_dependency
    puts "=========="
    puts "Dependencies Estimated:"

    @all_f.each{|f|
      next if @forward[f].size == 0
    
      needs = f_to_o(f)
      needed = []

      @forward[f].each{|f_needed|
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

          # puts "Files to be compiled for main #{main}:"
          # puts " " + obj.join(" ")
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
  estim.rake_dependency
  if $main
    SRC, OBJ = estim.src_and_obj_for_main($main)
  end
end
  
#eof
