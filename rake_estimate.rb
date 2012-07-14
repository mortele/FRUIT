#!/usr/bin/env ruby

extensions = ["f90", "f95", "f03", "f08"]

main = $main

all_f = []
extensions.each{|fxx|
  Dir::glob("*.#{fxx}").each{|filename|
    all_f << filename
  }
}
#puts "fortran files:"
#puts all_f.join(" ")

mod_in_f = {}
f_uses_mod = {}

all_f.each{|filename|
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

def f_to_o(name, extensions)
  extensions.each{|fxx|
    if name =~ /.#{fxx}/
      return name.sub(".#{fxx}", ".o")
    end
  }
  return nil
end

forward = {}
all_f.each{|f|
  forward[ f ] = []
  f_uses_mod[f].uniq.each{|a_mod|
    if mod_in_f[a_mod]
      forward[ f ] << mod_in_f[ a_mod ]
    end
  }
}

puts "=========="
puts "Dependencies Estimated:"
all_f.each{|f|
  next if forward[f].size == 0

  needs = f_to_o(f, extensions)
  needed = []
  forward[f].each{|f_needed|
    needed << f_to_o(f_needed, extensions)
  }
  file needs => needed if defined?(Rake)

  #for rake
  needed_str = "['" + needed.join("', '") + "']"
  print "  file '#{needs}' => #{needed_str}\n"

#   #for makefile
#   print needs + " : " + needed.join(" ") + "\n"
}

def get_needed(fortrans, forward)
  f_add = []
  fortrans.each{|f|
    next if not forward[f]
    forward[f].each{|f_plus|
      f_add << f_plus if !fortrans.index(f_plus)
    }
  }
  if f_add.size == 0
    return fortrans.uniq
  end
  get_needed(fortrans + f_add, forward)
end

def get_ordered(needed, forward, ordered = [])
  f_add = []
  (needed - ordered).each{|f|
    next if ordered.index(f)
    if forward[f].size == 0
      f_add << f
    elsif (ordered & forward[f]).sort == (forward[f]).sort
      f_add << f
    else
    end
  }
  if f_add.size == 0
    return ordered.uniq
  end 
  get_ordered(needed, forward, ordered + f_add)
end

if defined?(main) and main
  needed = get_needed( [main], forward )
  ordered_f = get_ordered(needed, forward)

  #replacing OBJ with ordered_o
  if defined?(FileList)
    if ordered_f.size > 0
      SRC = FileList[]
      ordered_f.each{|f|
        SRC.concat(FileList[f])
      }
      OBJ = SRC.ext('o')
      puts "=========="
      puts "Files to be compiled for main #{main}:"
      puts " " + OBJ.join(" ")
      puts "=========="
    end
  end
end

#eof
