include "string.vch";

UpperCase(x) := Replace(Upper($x), " ", "");

  # does not handle all cap-words correctly:
#CamelCase(x) := Mid(Replace(Capitalize("x" $x), " ", ""),1);

UpperCamelCase(x) := 
    EvalTemplate('"".join([x[0].capitalize()+x[1:] for x in %s.split()])', $x);

CamelCase(x) := Mid(UpperCamelCase("x" $x),1);


Keyword(text) := $text " <" CamelCase(Variable.Get("x")) "> ";
UpperKeyword(text) := $text " " UpperCamelCase(Variable.Get("x")) " ";

Punctuation(text) := $text CamelCase(Variable.Get("x")) " ";


<slot> := (
       abstract     = Keyword("abstract")
     | boolean      = Keyword("boolean")
     | break        = Keyword("break")
     | case         = Keyword("case")
     | catch        = Keyword("catch")
     | class        = UpperKeyword("class")
     | continue     = Keyword("continue")
     | double       = Keyword("double")
     | else         = Keyword("else")
     | extends      = Keyword("extends")
     | false        = Keyword("false")
     | final        = Keyword("final")
     | finally      = Keyword("finally")
     | float        = Keyword("float")
     | for symbol   = Keyword(for)                  # to distinguish from <prn> 4
     | if           = Keyword("if")
     | implements   = Keyword("implements")
     | import       = Keyword("import")
     | instance of  = Keyword(instanceof)
     | int          = Keyword("int")
     | interface    = Keyword("interface")
     | long         = Keyword("long")
     | new          = UpperKeyword("new")
     | null         = Keyword("null")
     | package      = Keyword("package")
     | param        = Keyword("param")
     | private      = Keyword("private")
     | protected    = Keyword("protected")
     | public       = Keyword("public")
     | return       = Keyword("return")
     | short        = Keyword("short")
     | static       = Keyword("static")
     | super        = Keyword("super")
     | switch       = Keyword("switch")
     | synchronized = Keyword("synchronized")
     | this         = Keyword("this")
     | throw        = Keyword("throw")
     | throws       = Keyword("throws")
     | true         = Keyword("true")
     | try          = Keyword("try")
     | void         = Keyword("void")
     | while        = Keyword("while")

     | "paren"      = Punctuation("(")
     | slide        = Punctuation(")")
     | dot          = Punctuation(".")
     | comma        = Punctuation(", ")
     | gets       = Punctuation("= ")
     | double equals       = Punctuation("== ")
     | semicolon    = Punctuation(";")

       # pulled into anything:
     | "left paren" = Punctuation("(")
     | close paren  = Punctuation(")")
     | right paren  = Punctuation(")")
     | equals       = Punctuation("= ")

);

Handle(command, argument) := Variable.Set(x, $argument) $command;

<slot> <_anything> = Handle($1, $2) ;
<slot> <_anything> <slot> <_anything> = Handle($1, $2) Handle($3, $4);
<slot> <_anything> <slot> <_anything> <slot> <_anything> =
     Handle($1, $2) Handle($3, $4) Handle($5, $6);
