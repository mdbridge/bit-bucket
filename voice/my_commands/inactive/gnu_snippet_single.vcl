### 
### Single-step fetching of pre-specified patterns (experimental):
### 

<snippet> := (
           snippet = {ctrl+y}
    | copy snippet = ""
);

Snippet(line, selector, command)          :=
          SaveExcursion(LineMod($line) $selector {esc}w) $command;

Snippet2(line, go_start, go_end, command) := 
          Snippet($line, $go_start SavePoint(ScratchRegister2())
                         $go_end Mark() RestorePoint(ScratchRegister2())
			 Exchange(),
	          $command);


## 
## Selecting snippets by paired delimiters (ignoring nesting/balance):
## 

<delimiter> := ( quote      = '""' | star    = '**' | dollar = '$$' 
	       | apostrophe = "''" | paren   = '()' | brace  = '{}'
	       | bend       = '<>' | bracket = '[]' | bar    = '||' );

<snippet> <r> <delimiter>        =
     Snippet2($2, Leap(D, Left($3,1))        , Leap(d, Right($3,1)) {right},$1);

<snippet> <r> inside <delimiter> =
     Snippet2($2, Leap(D, Left($3,1)) {right}, Leap(d, Right($3,1))        ,$1);


## 
## <<<>>>
## 

  # <<<>>>
<snippet> <r> identifier <_anything>        =
        Snippet2($2, 
                LeapRegex(D, '[a-zA-Z_0-9]*$3[a-zA-Z_0-9]*'), 
                  LeapRegex(d, '[^a-zA-Z_0-9]'),
                $1);

  # <<<>>>
<snippet> <r> symbol <_anything>        =
        Snippet2($2, 
                LeapRegexRaw(D, '[^ {ctrl+q}{ctrl+i}]*$3[^ {ctrl+q}{ctrl+i}]*'),  
                  LeapRegex(d, ' '),
                $1);


# 
# To Do?
#
#   snippet 3 third quote
#   nested?  (x (e) r)
#   "call"  x(...)
#   code below for long words/symbols

#Snippet(line, target) := SaveExcursion(
#		         LineMod($line) LeapRegexRaw(D, $target)
#			 Mark()
#			 LeapRegexRaw(D+, $target)
#			 {esc}w
#		      );
#
#
##<snippet> <r>            = Snippet($2, '\b[a-zA-Z][a-zA-Z0-9]*\b') $1;
#
#<snippet> <r> under 1 = Snippet($2, 
#	  '\b[a-z][a-z0-9]*_[a-z][a-z0-9]*\b') $1;
#
#<snippet> <r> camel 2 = Snippet($2, 
#	  '{esc}c\b[a-zA-Z][a-z0-9]*[A-Z][a-z0-9]*\b') $1;
#
#<snippet> <r> word <letter>
#           = Snippet($2, '\b$3[a-z0-9]*\b') $1;
