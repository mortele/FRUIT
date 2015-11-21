
num_testsuites = 0
num_testcase_line = 0
open("result.xml", "r"){|f|
  f.each{|line|
    if line =~ /<testsuite .*tests="([0-9]+)"/
      num_testsuites = $1.to_i
    end
    if line =~ /<testcase +name.*>/
       num_testcase_line += 1
    end
  }
}

if (num_testcase_line == 1)
  print "OK, number of testcase line is 1.\n"
else
  raise "Number of testcase line is not 1."
end

if (num_testsuites == 1)
  print "OK, number of test suites is 1.\n"
else
  raise "Number of tests is not 1 but #{num_testsuites}."
end
