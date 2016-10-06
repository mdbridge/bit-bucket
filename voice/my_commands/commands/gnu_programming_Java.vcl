### 
### Gnuemacs voice commands for dealing with Java
### 
###   See also gnu_programming_C_style.vcl
### 

include "gnu.vch";
include "letters.vch";



### 
### Additional Java-specific statement abbreviations:
### 

system print line = {home}{tab} "System.out.println();" {left_2};



### 
### Inserting debugging statements:
### 

InsertBreak(statement) := {home}{ctrl+o} "$statement // mdl-break" 
		       	  Leap(u, '^') {Del};

insert          break = InsertBreak("System.out.println(^);");
insert non-line break = InsertBreak('System.out.print("^");');

 # insert debugging printf where line needs to be turned into a block:
insert single   break = "{end}{enter}} // mdl-break" {up}
       	      	        InsertBreak("System.out.println(^);");


  # remove debugging printf:
remove break = Leap(D, "// mdl-break") {home}{ctrl+u}1{ctrl+k};



###
### Finding Java code points via regular expressions:
###

  # whitespace, both breakable (i.e., can include newline) and not:
WS(repetition)  := "[ {ctrl+q}{tab}]"                 $repetition;
BWS(repetition) := "[ {ctrl+q}{tab}{ctrl+q}{ctrl+j}]" $repetition;

  # identifiers (do not contain '.' or '['):
Identifier(prefix) := $prefix "[a-zA-Z0-9_]*";

FieldID()         := Identifier("[a-zA-Z_]");
ClassID()         := Identifier("[A-Z]");
MethodID()        := Identifier("[a-z_]");     # disjoint with ClassID

  # list of formals:
Formals()  := "([^)]*)";

  # this matches all Java types, but excludes "new" and "return":
JavaType() := "[A-Zbcdfilsv][][a-zA-Z._0-9]*";	


##
## Find occurrences of given Java method(s):
##

#
# Match a line starting with whitespace and any modifier key words;
# we use this to avoid (most) text inside comments and quotations.
#
RoutinePrefix() := "^\([a-z {ctrl+q}{tab}]*[ {ctrl+q}{tab}]\)?";

MethodOccurs(pattern) := OccursNoFold(
	  RoutinePrefix() JavaType() BWS(+) $pattern WS(*) Formals()
	);

  # spell beginning of method name:
method <letter> [<letter> [<letter>]] = MethodOccurs(Identifier($1$2$3));

  # predefined method names:
<method_name> := (
       main
     | marshal
     | toString
     | unmarshal
     | usage
);
method <method_name>              = MethodOccurs($1);

  # all methods:
show methods                      = MethodOccurs(MethodID());

  # user entered name (case-sensitive):
teleport method =
	Do(mdl-occur-no-fold)
	  RoutinePrefix() JavaType() BWS(+) 
	  Mark()
	  WS(*) Formals()
	  Exchange();


##
## Find occurrences of given Java constructors(s):
##

#
# find all non-anonymous constructor declarations and possibly some
# anonymous constructor declarations
#
#   (Formals does not match the argument list of every anonymous
# constructor declaration)
#
show constructors = OccursNoFold(
	  RoutinePrefix() ClassID() WS(*) Formals() BWS(*)
	    # skip constructor applications that are also not declarations:
	  "\({{}\|throws\)"
	);


##
## Find occurrences of given Java field(s):
##

FieldModifier()  := "\(private\|public\|protected\|static\|final\|volatile\)";
  # one or more field modifiers:
FieldModifiers() := WS(*) "\(" FieldModifier() WS(+) "\)+";

FieldPrefix()    := '^\([ ]\{0,4\}\|\(' FieldModifiers() "\)\)";


  # This should find any reasonable field except package fields of
  # nested classes (omitted because they are indistinguishable from
  # variable declarations):
FieldOccurs(pattern) := OccursNoFold(
	  FieldPrefix() JavaType() WS(+) 
	  "\(" FieldID() "[][]*" WS(*) "," WS(*) "\)*"
	  $pattern "[][]*"
	  "\(" WS(*) "," WS(*) FieldID() "[][]*\)*"
	  WS(*) "[;=]"
	);

  # all fields except package fields of nested classes:
show fields               = FieldOccurs(FieldID());

  # spell beginning of field name (<letter> is always lowercase for now):
field <letter> [<letter>] = FieldOccurs(Identifier($1$2));
