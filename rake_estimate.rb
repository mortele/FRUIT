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
      if line =~ /^\s*use +(\w+)\b?/
        f_uses_mod[ filename ] << $1
      end
      if line =~ /^\s*module +(\w+)\b/
        mod = $1
        mod_in_f[ mod ] = filename unless mod =~ /procedure/ 
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
    forward[ f ] << mod_in_f[ a_mod ]
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
  needed_str = "['" + needed.join("', '") + "']"
  print "  file '#{needs}' => #{needed_str}\n"
  file needs => needed if defined?(Rake)
}

def get_needed(fortrans, forward)
  f_add = []
  fortrans.each{|f|
    forward[f].each{|f_plus|
      f_add << f_plus if !fortrans.index(f_plus)
    }
  }
  if f_add.size == 0
    return fortrans.uniq
  end
  get_needed(fortrans + f_add, forward)
end

needed = get_needed( [main], forward )

def get_ordered(needed, forward, ordered = [])
  f_add = []
  needed.each{|f|
    next if ordered.index(f)
    if forward[f].size == 0
      f_add << f
    elsif ordered & forward[f] == forward[f]
      f_add << f
    end
  }
  if f_add.size == 0
    return ordered.uniq
  end 
  get_ordered(needed, forward, ordered + f_add)
end

ordered_f = get_ordered(needed, forward)

#replacing OBJ with ordered_o
SRC = FileList[]
ordered_f.each{|f|
  SRC.concat(FileList[f])
}
OBJ = SRC.ext('o')

puts "=========="
puts "Files to be compiled:"
puts " " + OBJ.join(" ")
puts "=========="
      
#eof
