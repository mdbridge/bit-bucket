### 
### Tests for NatLink bugs:
### 

## 
## Test handling of words containing backslashes:
## 

# 
# These behave like the lists denote the empty string:
# 

<y> := ( "/" | "\\" );
test type <y> = $1;

<z> := ( "/" | "\" );
test foo <z> = $1;

<x> :=  (white |"yellow " | "orange ");
Fred test <x> = $1;


# 
# These aren't recognizable:
# 

test Fred "\" = George;

omega test "red " = blue;
Delta test (white |"yellow " | "orange ") = $1;


# 
# this one works fine:
# 

alpha test = normal;
