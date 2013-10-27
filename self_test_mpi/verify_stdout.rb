
num_init = 0
num_summary = 0
num_rank_zero = 0
num_rank_one  = 0
num_failed_line = 0
num_failed_sum = 0
rank_zero_ok = 0
rank_one_ok = 0
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

    if line =~ /message: \[rank=  0\]/
      num_rank_zero += 1
      if flag == 1 
        print "OK, message is between the lines\n"
        rank_zero_ok = 1
      else
        print "NG, message is not between the lines. flag = ", flag, "\n"
        rank_zero_ok = 0
      end
    end

    if line =~ /message: \[rank=  1\]/
      num_rank_one += 1
      if flag == 1 
        print "OK, message is between the lines\n"
        rank_one_ok = 1
      else
        print "NG, message is not between the lines. flag =", flag, "\n"
        rank_one_ok = 0
      end
    end

    if line =~ /^ *Failed +: +([0-9]+)$/
      num_failed_line += 1
      num_failed_sum  += $1.to_i
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

if num_rank_zero == 1
  print "OK, [message: [rank=  0]] appeared once as expected.\n"
else
  raise "NG, [message: [rank=  0]] not appeared or appeared more than once"
end

if (num_failed_sum  == 3)
  print "OK, Number of fails is three\n"
else
  raise "NG, Number of fails is not three."
end

if (num_failed_line == 1)
  print "OK, [Failed : ] line appeared once\n"
else
  ralse "NG, [Failed : ] line not appeared  or appeared more than once"
end

if num_rank_one  == 1
  print "OK, [message: [rank=  1]] appeared once as expected.\n"
else
  raise "NG, [message: [rank=  1]] not appeared or appeared more than once"
end

if rank_one_ok == 0
  raise "rank=1 line not in correct place"
end
if rank_zero_ok == 0
  raise "rank=0 line not in correct place"
end
