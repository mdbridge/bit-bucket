### 
### These files comprise a torture test for any Vocola lexer
### 


## 
## Quotations outside of a context statement:
## 

"double"        'single'
"white space"   'white	space'

"double"'single'"double"

x"double"x'single'x

""         ''
"1""2''"   '2""1'''
""""       ''''

not a context " ( ) [ ] = := | , : "   ' ( ) [ ] = := | , : '
              " ; "   ' ; '   " # "    ' # '


"broken
'broken#comment
"""

repair = "missing;
repair = "missing ;	 

repair (a = " |
        b = "foo" ) = baz;


"