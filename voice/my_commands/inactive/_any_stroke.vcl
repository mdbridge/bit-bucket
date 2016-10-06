### 
### <<<>>>
### 

# spell ([Cap] a)...
#    all caps that afterwards...


# lost capitals here...
stroke <base>                      = {$1};
stroke <base> <base>               = {$1} {$2};
stroke <base> <base> <base>        = {$1} {$2} {$3};
stroke <base> <base> <base> <base> = {$1} {$2} {$3} {$4};


stroke <base>                      times 1..20 = Repeat($2,{$1});
stroke <base> <base>               times 1..20 = Repeat($3,{$1} {$2});
stroke <base> <base> <base>        times 1..20 = Repeat($4,{$1} {$2} {$3});
stroke <base> <base> <base> <base> times 1..20 = Repeat($5,{$1} {$2} {$3} {$4});


  # <<<>>>
dig down 1..20             =            Repeat($1, {Del}{down});
dig up   1..20             =            Repeat($1, {Del}{up});

dig down 1..20 times 2..20 = Repeat($1, Repeat($2, {Del}){down});
dig up   1..20 times 2..20 = Repeat($1, Repeat($2, {Del}){up});

  # <<<>>>
include "printables.vch";
include "numbers.vch";
key <prn> <my0to99> = Repeat($2, PrintablesToKeys($1));


<base> := (
       space = " "
     |  !  | bang = !
     | '"' | open quote = '"' | quote = '"'
     | "#" | pound = "#"
     | "$" | dollar = "\$"
     |  %  | percent = %
     |  & 
     | "'" | apostrophe = "'" | single quote = "'"
     | "(" | paren = "("
     | ")" | close paren = ")"
     |  *  | asterisk = * | star = *
     |  +  | plus = +
     | ","
     |  -  | minus = -
     |  .  | dot = .
     | / 

       # digits:
     | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

     | ":" | ";" 
     | "<" | less than = "<" | less = "<"
     | "=" | equal = "=" | equals = "="
     | ">" | greater than = ">" | greater = ">"
     | ? | @ 


     | "[" | bracket = "["
     | backslash = "\"
     | "]" | close bracket = "]"
     | ^ | _ | `

          # lowercase letters:
     | A.=a | B.=b | C.=c | D.=d | E.=e | F.=f | G.=g | H.=h | I.=i
     | J.=j | K.=k | L.=l | M.=m | N.=n | O.=o | P.=p | Q.=q
     | R.=r | S.=s | T.=t | U.=u | V.=v | W.=w | X.=x | Y.=y | Z.=z

     | Alpha    = a
     | Bravo    = b
     | Charlie  = c
     | Delta    = d
     | echo     = e
     | foxtrot  = f
     | golf     = g
     | Hotel    = h
     | India    = i
     | Juliett  = j
     | kilo     = k
     | Lima     = l
     | Mike     = m
     | November = n
     | Oscar    = o
     | Papa     = p 
     | Quebec   = q
     | Romeo    = r
     | Sierra   = s
     | tango    = t
     | uniform  = u
     | Victor   = v
     | whiskey  = w
     | xray     = x
     | Yankee   = y
     | Zulu     = z

     | "{" | "brace" = "{"
     | "|" | vertical bar = "|" | bar = "|"
     | "}" | close brace = "}"
     | "~"

     | up	 = up
     | down      = down
     | left	 = left
     | right	 = right

     | page up   = PgUp
     | page down = PgDn

     | home	 = home
     | end       = end

     | backspace = backspace | back = backspace
     | delete    = Del | erase = Del

     | space	 = space
     | tab	 = tab
     | enter     = enter
#     | escape    = esc
);
