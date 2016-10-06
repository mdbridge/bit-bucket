### 
### Check inter-token positioning:
### 

# context statements:



# references:

test      = $1;
test      = xx$1;
test      = $1xx;
test 1..9 = xx$1xx$2yy$3oo;
test 1..9 = xx$1$2yy$3oo;
test      = $$$1;
test      = \$1\\$1\\$1\$\$\$$+$1\$;

test      = "$1";
test      = """""$1";
test      = "$1xx";
test 1..9 = "''$1x""$2yy$3oo";
test 1..9 = "xx$1$2yy$3oo";
test      = "$$$1";
test      = "\$1\\$1\\$1\$\$\$$+$1\$";

test      = '$1';
test      = 'x''$1';
test      = '$1xx';
test 1..9 = 'xx$1xx$2yy$3oo';
test 1..9 = 'x""$1$2yy$3oo';
test      = '$$$1';
test      = '\$1\\$1\\$1\$\$\$$+$1\$';

foo(x,y) := $z;
foo(x,y) := xx$_q;
foo(x,y) := \$1\\$1\\$1\$\$\$$+$z\$;
foo(x,y) := xx$x$z;
foo(x,y) := $$$z;

foo(x,y) := "x''x$x""$z";
foo(x,y) := 'x""x$x''$z';
