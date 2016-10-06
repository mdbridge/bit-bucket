###
### Voice commands for programming in Ruby
###

include "gnu.vch";
include "letters.vch"; 
include "string.vch";

Place(before, after) := $before $after {left_ Len($after) };


## 
## Inline Ruby blocks:
## 

Block(arguments) := 
    {shift+space} Place("{ " When($arguments,"|$arguments| "),  " }");

Ruby block                           = Block(x);
Ruby block (zero=""|two="x,y")       = Block($1);
Ruby block <letter> [comma <letter>] = Block($1 When($2,",$2"));



## 
## Multiline Ruby "blocks":
## 

  # This works with and without elastic indent mode:
Feed() := {tab}{enter}{tab};


       Ruby end = end Feed();
insert Ruby end = {home}{ctrl+o} end{tab} {down}{home}{tab};


PushEnd()  := {end} Feed()        end{tab} {up}{end};
PushEnd2() := {end} Feed() Feed() end{tab} {up}{end}{tab};

End in Ruby block = {end} {shift+space} do PushEnd()   Place(" |", "|");
End in Do 	  = {end} {shift+space} do PushEnd2();
End in End        = {end}                  PushEnd2();

Ruby (define=def|class|module) = $1 PushEnd() {space};



## 
## Other Ruby-specific syntax:
## 

(readable Ruby="attr_reader"| writable Ruby="attr_writable"| Ruby="attr_accessor")
    attribute = {tab}$1 " ";



## 
## Rspec
## 

RSpec let <_anything>	 = {tab} Place("let(:" Replace($1, " ","_") ") { ", " }");
RSpec let <_anything> do = {tab}       "let(:" Replace($1, " ","_") ") do" PushEnd2();

before each block = {tab} "before :each do" PushEnd2();

<to> := ( to | to not = not_to | not to = not_to );
	       
<predicate> := (
                 equal       = "eq("          # same value

               | be nil      = "be_nil"
               | be true     = "be true"
               | be false    = "be false"

               | match       = "match /"      # regex matching
               | match array = "match_array " # match array modulo element order

               | receive     = "receive(:"
               );

expect <to> <predicate> = {tab} Place("expect(", ").$1 $2") Empty();

allow          <to> receive = {tab} Place("allow(", ").$1 receive(:") Empty();
allow <letter> <to> receive = {tab} "allow(@$1).$2 receive(:";


expect exception = {tab} Place("expect { ", " }.to raise_error") Empty();



## 
## Using irb:
## 

repeat load = {ctrl+r} load {ctrl+e} {enter};

# see also "blue load" in _switch.vcl...
