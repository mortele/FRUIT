#!/usr/bin/env ruby

extensions = ["f90", "f95", "f03", "f08"]

file_gives_mod = {}
mod_usedby_file = {}
extensions.each{|fxx|
  Dir::glob("*.#{fxx}").each{|filename|
    open(filename, 'r'){|f|
      f.each_line{|line|
        if line =~ /^\s*use +(\w+)\b?/
          mod = $1
          if mod_usedby_file[ filename ]
            mod_usedby_file[ filename ] << mod
          else
            mod_usedby_file[ filename ] = [mod]
          end
        end
        if line =~ /^\s*module +(\w+)\b/
          mod = $1
          if mod =~ /procedure/ 
            next
          end
          file_gives_mod[ mod ] = filename
        end
      }
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

puts "Dependencies Estimated:"
mod_usedby_file.each{|a|
  needs = f_to_o(a[0], extensions)
  a[1].uniq.each{|mod|
    needed = f_to_o(file_gives_mod[ mod ], extensions)
    if needed
      print "  file '#{needs}' => '#{needed}'\n"
      file needs => needed if defined?(Rake)
    end
  }
}



#eof
