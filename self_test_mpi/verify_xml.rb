
num_tests = 0
num_testcase_line = 0
open("result.xml", "r"){|f|
  f.each{|line|
    if line =~ /<testsuite .*tests="([0-9]+)"/
      num_tests = $1.to_i
    end
    if line =~ /<testcase +name.*>/
       num_testcase_line += 1
    end
  }
}

if (num_testcase_line == 3)
  print "OK, number of testcase lines is 3.\n"
else
  raise "Number of testcase lines is not 3."
end

if (num_tests == 3)
  print "OK, number of tests is 3.\n"
else
  raise "Number of tests is not 3 but #{num_tests}."
end
