require 'druby'
require 'druby/contract/signature'

sig = DRuby::Contract::PolyMethod.new([[:t,nil]],DRuby::Contract::MonoMethod.new(:id,DRuby::Contract::Params.new([DRuby::Contract::PolyVar.new(:t)],[]),nil,DRuby::Contract::PolyVar.new(:t),DRuby::Contract::Origin.new('./contracts/succeed/polymethod_identity.rb',4,nil)),DRuby::Contract::Origin.new('./contracts/succeed/polymethod_identity.rb',4,nil))

DRuby::Contract::Registry.register('A',:'id',sig)

sig = DRuby::Contract::InterMethod.new(:choose,[DRuby::Contract::MonoMethod.new(:choose,DRuby::Contract::Params.new([DRuby::Contract::ObjectType.new({},{:foo => DRuby::Contract::MonoMethod.new(:foo,DRuby::Contract::Params.new([],[]),nil,DRuby::Contract::ClassType.new('String'),DRuby::Contract::Origin.new('./contracts/succeed/intersection_object.rb',13,nil))})],[]),nil,DRuby::Contract::ClassType.new('String'),DRuby::Contract::Origin.new('./contracts/succeed/intersection_object.rb',13,nil)),
DRuby::Contract::MonoMethod.new(:choose,DRuby::Contract::Params.new([DRuby::Contract::ObjectType.new({},{:bar => DRuby::Contract::MonoMethod.new(:bar,DRuby::Contract::Params.new([],[]),nil,DRuby::Contract::ClassType.new('String'),DRuby::Contract::Origin.new('./contracts/succeed/intersection_object.rb',13,nil))})],[]),nil,DRuby::Contract::ClassType.new('String'),DRuby::Contract::Origin.new('./contracts/succeed/intersection_object.rb',13,nil))],DRuby::Contract::Origin.new('./contracts/succeed/intersection_object.rb',13,nil))


DRuby::Contract::Registry.register('C',:'choose',sig)

sig = DRuby::Contract::InterMethod.new(:foo,[DRuby::Contract::MonoMethod.new(:foo,DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('String'),
DRuby::Contract::ClassType.new('String')],[]),nil,DRuby::Contract::ClassType.new('String'),DRuby::Contract::Origin.new('./contracts/succeed/intersection_num_args_difference.rb',6,nil)),
DRuby::Contract::MonoMethod.new(:foo,DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('String'),
DRuby::Contract::ClassType.new('Fixnum'),
DRuby::Contract::ClassType.new('String')],[]),nil,DRuby::Contract::ClassType.new('Fixnum'),DRuby::Contract::Origin.new('./contracts/succeed/intersection_num_args_difference.rb',6,nil))],DRuby::Contract::Origin.new('./contracts/succeed/intersection_num_args_difference.rb',6,nil))


DRuby::Contract::Registry.register('A',:'foo',sig)

sig = DRuby::Contract::InterMethod.new(:foo,[DRuby::Contract::MonoMethod.new(:foo,DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('A')],[]),nil,DRuby::Contract::ClassType.new('A'),DRuby::Contract::Origin.new('./contracts/succeed/intersection_class.rb',12,nil)),
DRuby::Contract::MonoMethod.new(:foo,DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('B')],[]),nil,DRuby::Contract::ClassType.new('B'),DRuby::Contract::Origin.new('./contracts/succeed/intersection_class.rb',12,nil))],DRuby::Contract::Origin.new('./contracts/succeed/intersection_class.rb',12,nil))


DRuby::Contract::Registry.register('Object',:'foo',sig)

sig = DRuby::Contract::InterMethod.new(:str,[DRuby::Contract::MonoMethod.new(:str,DRuby::Contract::Params.new([],[]),DRuby::Contract::Block.new(DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('String')],[]),DRuby::Contract::ClassType.new('Fixnum')),DRuby::Contract::ClassType.new('Fixnum'),DRuby::Contract::Origin.new('./contracts/succeed/intersection_block_difference.rb',5,nil)),
DRuby::Contract::MonoMethod.new(:str,DRuby::Contract::Params.new([],[]),DRuby::Contract::Block.new(DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('String')],[]),DRuby::Contract::ClassType.new('String')),DRuby::Contract::ClassType.new('String'),DRuby::Contract::Origin.new('./contracts/succeed/intersection_block_difference.rb',5,nil))],DRuby::Contract::Origin.new('./contracts/succeed/intersection_block_difference.rb',5,nil))


DRuby::Contract::Registry.register('A',:'str',sig)

sig = DRuby::Contract::InterMethod.new(:str,[DRuby::Contract::MonoMethod.new(:str,DRuby::Contract::Params.new([],[]),DRuby::Contract::Block.new(DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('String')],[]),DRuby::Contract::ClassType.new('Fixnum')),DRuby::Contract::ClassType.new('Fixnum'),DRuby::Contract::Origin.new('./contracts/fail/intersection_block_difference_fail1.rb',5,nil)),
DRuby::Contract::MonoMethod.new(:str,DRuby::Contract::Params.new([],[]),DRuby::Contract::Block.new(DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('String')],[]),DRuby::Contract::ClassType.new('String')),DRuby::Contract::ClassType.new('String'),DRuby::Contract::Origin.new('./contracts/fail/intersection_block_difference_fail1.rb',5,nil))],DRuby::Contract::Origin.new('./contracts/fail/intersection_block_difference_fail1.rb',5,nil))


DRuby::Contract::Registry.register('A',:'str',sig)

sig = DRuby::Contract::InterMethod.new(:str,[DRuby::Contract::MonoMethod.new(:str,DRuby::Contract::Params.new([],[]),DRuby::Contract::Block.new(DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('String')],[]),DRuby::Contract::ClassType.new('Fixnum')),DRuby::Contract::ClassType.new('Fixnum'),DRuby::Contract::Origin.new('./contracts/fail/intersection_block_difference_fail2.rb',5,nil)),
DRuby::Contract::MonoMethod.new(:str,DRuby::Contract::Params.new([],[]),DRuby::Contract::Block.new(DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('String')],[]),DRuby::Contract::ClassType.new('String')),DRuby::Contract::ClassType.new('String'),DRuby::Contract::Origin.new('./contracts/fail/intersection_block_difference_fail2.rb',5,nil))],DRuby::Contract::Origin.new('./contracts/fail/intersection_block_difference_fail2.rb',5,nil))


DRuby::Contract::Registry.register('A',:'str',sig)

sig = DRuby::Contract::InterMethod.new(:str,[DRuby::Contract::MonoMethod.new(:str,DRuby::Contract::Params.new([],[]),DRuby::Contract::Block.new(DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('String')],[]),DRuby::Contract::ClassType.new('Fixnum')),DRuby::Contract::ClassType.new('Fixnum'),DRuby::Contract::Origin.new('./contracts/fail/intersection_block_difference_fail3.rb',5,nil)),
DRuby::Contract::MonoMethod.new(:str,DRuby::Contract::Params.new([],[]),DRuby::Contract::Block.new(DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('String')],[]),DRuby::Contract::ClassType.new('String')),DRuby::Contract::ClassType.new('String'),DRuby::Contract::Origin.new('./contracts/fail/intersection_block_difference_fail3.rb',5,nil))],DRuby::Contract::Origin.new('./contracts/fail/intersection_block_difference_fail3.rb',5,nil))


DRuby::Contract::Registry.register('A',:'str',sig)

sig = DRuby::Contract::InterMethod.new(:foo,[DRuby::Contract::MonoMethod.new(:foo,DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('A')],[]),nil,DRuby::Contract::ClassType.new('A'),DRuby::Contract::Origin.new('./contracts/fail/intersection_class_fail1.rb',12,nil)),
DRuby::Contract::MonoMethod.new(:foo,DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('B')],[]),nil,DRuby::Contract::ClassType.new('B'),DRuby::Contract::Origin.new('./contracts/fail/intersection_class_fail1.rb',12,nil))],DRuby::Contract::Origin.new('./contracts/fail/intersection_class_fail1.rb',12,nil))


DRuby::Contract::Registry.register('Object',:'foo',sig)

sig = DRuby::Contract::InterMethod.new(:foo,[DRuby::Contract::MonoMethod.new(:foo,DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('A')],[]),nil,DRuby::Contract::ClassType.new('A'),DRuby::Contract::Origin.new('./contracts/fail/intersection_class_fail2.rb',12,nil)),
DRuby::Contract::MonoMethod.new(:foo,DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('B')],[]),nil,DRuby::Contract::ClassType.new('B'),DRuby::Contract::Origin.new('./contracts/fail/intersection_class_fail2.rb',12,nil))],DRuby::Contract::Origin.new('./contracts/fail/intersection_class_fail2.rb',12,nil))


DRuby::Contract::Registry.register('Object',:'foo',sig)

sig = DRuby::Contract::InterMethod.new(:foo,[DRuby::Contract::MonoMethod.new(:foo,DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('A')],[]),nil,DRuby::Contract::ClassType.new('A'),DRuby::Contract::Origin.new('./contracts/fail/intersection_class_fail3.rb',12,nil)),
DRuby::Contract::MonoMethod.new(:foo,DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('B')],[]),nil,DRuby::Contract::ClassType.new('B'),DRuby::Contract::Origin.new('./contracts/fail/intersection_class_fail3.rb',12,nil))],DRuby::Contract::Origin.new('./contracts/fail/intersection_class_fail3.rb',12,nil))


DRuby::Contract::Registry.register('Object',:'foo',sig)

sig = DRuby::Contract::InterMethod.new(:foo,[DRuby::Contract::MonoMethod.new(:foo,DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('String'),
DRuby::Contract::ClassType.new('String')],[]),nil,DRuby::Contract::ClassType.new('String'),DRuby::Contract::Origin.new('./contracts/fail/intersection_num_args_difference_fail1.rb',6,nil)),
DRuby::Contract::MonoMethod.new(:foo,DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('String'),
DRuby::Contract::ClassType.new('Fixnum'),
DRuby::Contract::ClassType.new('String')],[]),nil,DRuby::Contract::ClassType.new('Fixnum'),DRuby::Contract::Origin.new('./contracts/fail/intersection_num_args_difference_fail1.rb',6,nil))],DRuby::Contract::Origin.new('./contracts/fail/intersection_num_args_difference_fail1.rb',6,nil))


DRuby::Contract::Registry.register('A',:'foo',sig)

sig = DRuby::Contract::InterMethod.new(:foo,[DRuby::Contract::MonoMethod.new(:foo,DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('String'),
DRuby::Contract::ClassType.new('String')],[]),nil,DRuby::Contract::ClassType.new('String'),DRuby::Contract::Origin.new('./contracts/fail/intersection_num_args_difference_fail2.rb',6,nil)),
DRuby::Contract::MonoMethod.new(:foo,DRuby::Contract::Params.new([DRuby::Contract::ClassType.new('String'),
DRuby::Contract::ClassType.new('Fixnum'),
DRuby::Contract::ClassType.new('String')],[]),nil,DRuby::Contract::ClassType.new('Fixnum'),DRuby::Contract::Origin.new('./contracts/fail/intersection_num_args_difference_fail2.rb',6,nil))],DRuby::Contract::Origin.new('./contracts/fail/intersection_num_args_difference_fail2.rb',6,nil))


DRuby::Contract::Registry.register('A',:'foo',sig)

sig = DRuby::Contract::PolyMethod.new([[:t,nil]],DRuby::Contract::MonoMethod.new(:id,DRuby::Contract::Params.new([DRuby::Contract::PolyVar.new(:t)],[]),nil,DRuby::Contract::PolyVar.new(:t),DRuby::Contract::Origin.new('./contracts/fail/polymethod_identity_fail1.rb',4,nil)),DRuby::Contract::Origin.new('./contracts/fail/polymethod_identity_fail1.rb',4,nil))

DRuby::Contract::Registry.register('A',:'id',sig)

sig = DRuby::Contract::PolyMethod.new([[:t,nil]],DRuby::Contract::MonoMethod.new(:id,DRuby::Contract::Params.new([DRuby::Contract::PolyVar.new(:t)],[]),nil,DRuby::Contract::PolyVar.new(:t),DRuby::Contract::Origin.new('./contracts/fail/polymethod_identity_fail2.rb',4,nil)),DRuby::Contract::Origin.new('./contracts/fail/polymethod_identity_fail2.rb',4,nil))

DRuby::Contract::Registry.register('A',:'id',sig)

