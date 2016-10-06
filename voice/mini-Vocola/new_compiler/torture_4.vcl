### 
### These files comprise a torture test for any Vocola lexer
### 


## 
## Bare words outside of context statements:
## 

x
azAZ09!@$%^&*-_+~./?`<>\{}

ctrlchar           loc
unicodeäÂ¿here  loc

not in a context
colon:inside :colon_starts ::x


## 
## Subtypes of bare words outside of context statements:
## 

<variable>      <not>variable
name
12..34          12..34not_range  not12..34
include
$ref            no$ref $noref& "$noref"
