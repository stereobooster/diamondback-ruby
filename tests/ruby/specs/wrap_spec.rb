
require 'druby/contract/wrap'

$DRUBY_ABORT_ON_CONTRACT=false

class A
  def no_args()
  end

  def plus2(x)
    x + 2
  end
end

module CompletelyEqual
  def be_completely_equal(other)
    simple_matcher("be equal") do |given|
      given.==(other).should be_true
      given.===(other).should be_true
      given.equal?(other).should be_true
      given.eql?(other).should be_true
      true
    end
  end

  def wrap(x)
    file,line = DRuby::Utils.find_caller(caller(1))
    DRuby::Contract::Wrap.wrap(file,line,nil,x)
  end

  def wrap_equal(orig)
    wrapped = wrap(orig)
    wrapped.should be_completely_equal(orig)
    orig.should be_completely_equal(wrapped)
  end

end

describe DRuby::Contract::Wrap do

  include CompletelyEqual

  it "should be equal to an unwrapped boxed object" do
    wrap_equal(A.new)
  end

  it "should be equal for fixnums" do
    wrap_equal(3)
  end

  it "should be equal for symbols" do
    wrap_equal(:sym)
  end

  it "should be equal for floats" do
    wrap_equal(3.0)
  end

  it "should be equal for true and false" do
    wrap_equal(true)
    wrap_equal(false)
  end

  it "should be equal for nil" do
    wrap_equal(true)
    wrap_equal(false)
  end

  it "should have the same hash value for boxed objects" do
    a = A.new
    wrap(a).hash.should equal(a.hash)
  end

  it "should have the same hash value for unboxed objects" do
    a = :sym
    wrap(a).hash.should equal(a.hash)
  end

  it "should raise a contract violation when called with a non-existant method" do
    a = A.new
    w = wrap(a)
    proc {w.boom}.should raise_error(DRuby::Contract::ContractViolation)
  end

  it "should raise a contract violation when passed the wrong number of arguments" do
    a = A.new
    w = wrap(a)
    proc {w.no_args("boom")}.should raise_error(DRuby::Contract::ContractViolation)
  end

  it "should not change the instance of a wrapped object" do
    a = A.new
    wrap_a = wrap(a)
    wrap_a.should be_a_kind_of(A)
    wrap_a.should be_an_instance_of(A)
    wrap_a.class.should eql(A)
  end

  it "should wrap classes" do
    klass = wrap(A)
    a = klass.new
    a.no_args
  end

  it "should respond to existing methods" do
    a = wrap(A.new)
    a.no_args
    a.should respond_to(:no_args)
  end

  it "should accept messages to existing methods" do
    a = wrap(A.new)
    a.no_args
    a.plus2(3)
  end

  it "should maintain arithmatic with wrapped fixnum receiver" do
    three = wrap(3)
    (three + 2).should eql(5)
  end

  it "should maintain arithmatic with wrapped fixnum argument" do
    three = wrap(3)
    (2 + three).should eql(5)
  end

  it "should maintain arithmatic with wrapped float receiver" do
    three = wrap(3.0)
    (three + 2.0).should eql(5.0)
  end

  it "should maintain arithmatic with wrapped float argument" do
    three = wrap(3.0)
    (2.0 + three).should eql(5.0)
  end

  it "should wrap singleton methods defined after the wrapper" do
    a = wrap(A.new)
    def a.single() end
    proc {a.single("boom")}.should raise_error(DRuby::Contract::ContractViolation)
  end

  it "should wrap instance methods defined after the wrapper" 
#   do
#     a = wrap(A.new)
#     class A; def inst() end; end
#     proc {a.inst("boom")}.should raise_error(DRuby::Contract::ContractViolation)
#   end
end
