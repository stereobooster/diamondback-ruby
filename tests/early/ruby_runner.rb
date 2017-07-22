#!/usr/bin/ruby

require 'test/unit'
require 'output.rb'

class EarlyTests < Test::Unit::TestCase

  def testSuccess
    test_succeed
  end

  def testFailure
    begin
      test_failure 
    rescue RuntimeError => e
      assert_equal(e.message, "early")
    end
  end
    
end
