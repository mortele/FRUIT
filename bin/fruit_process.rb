#!/usr/bin/env ruby

require 'rubygems'
require 'fruit_processor'

#if __FILE__ == $0
processor = FruitProcessor.new
ARGV[0] == nil ? dir = "." : dir = ARGV[0]
processor.pre_process dir
#end