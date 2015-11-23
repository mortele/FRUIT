
num_init = 0
num_summary = 0

num_failed_line = 0
num_failed_sum = 0
num_success_line = 0
num_success_sum  = 0

flag = 0
open("stdout", "r"){|f|
  f.each{|line|
    if line =~ /Test module initialized/i
      num_init += 1
    end

    if line =~ /Start of FRUIT summary:/
      num_summary += 1
    end
    flag = 1 if line =~ /Failed assertion messages/i
    flag = 2 if line =~ /end of failed assertion messages/i

    if line =~ /^ *Failed +: +([0-9]+)$/
      num_failed_line += 1
      num_failed_sum  += $1.to_i
    end

    if line =~ /^ *Successful +: +([0-9]+)$/
      num_success_line += 1
      num_success_sum  += $1.to_i
    end
  }
}
if num_init == 1
  print "OK, line [Test module initialized] once as expected\n"
else
  raise "line [Test module initialized] not appeared or appeared more than once"
end

if num_summary == 1
  print "OK, [Start of FRUIT summary] appeared once as expected.\n"
else
  raise "line [Start of FRUIT summary] not appeared or appeared more than once"
end

if (num_failed_sum  == 2100)
  print "OK, Number of fails is 2100.\n"
else
  raise "NG, Number of fails is not 2100."
end

if (num_failed_line == 1)
  print "OK, [Failed : ] line appeared once\n"
else
  ralse "NG, [Failed : ] line not appeared  or appeared more than once"
end

if (num_success_sum  == 4201)
  print "OK, Number of successes is 4201.\n"
else
  raise "NG, Number of successes is not 4201."
end

if (num_success_line == 1)
  print "OK, [Successful :] line appeared once\n"
else
  ralse "NG, [Successful :] line not appeared  or appeared more than once"
end

