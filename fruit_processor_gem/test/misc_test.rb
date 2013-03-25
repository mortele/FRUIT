#!/usr/bin/env ruby
require 'test/unit'
require '../lib/misc'

class MiscTest < Test::Unit::TestCase
  include FileUtils

  def test_os_install
    from = "os_install_from.txt"
    to   = "os_install_to.txt"

    File.unlink(to) if File.exists?(to)

    os_install(from, to)
    assert(File.exists?(to), "file #{to} generated")

    File.unlink(to) if File.exists?(to)
  end
end
