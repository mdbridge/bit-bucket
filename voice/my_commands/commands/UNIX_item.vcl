### 
### Grammar for controlling the shell via numbering files
### 
### See also UNIX_long_item.vcl for non-combinable commands
### 

include "control.vch";
include "numbers.vch";
include "directories.vch";
include "item_operation.vch";

$set MaximumCommands 4;


## 
## Command beginnings:
## 

<command> [<option> [<option>]] item [1..9 hundred] <my0to99> =
    "$1$2$3" Item2($4,$5);

<g> <git>			item [1..9 hundred] <my0to99> = 
    "git $2" Item2($3,$4);

sub <Subversion>		item [1..9 hundred] <my0to99> = 
    "svn $1" Item2($2,$3);


##
## Additional items/directories:
##

item [1..9 hundred] <my0to99> [(slash=/)] = Item2($1,$2) $3;

directory <UNIX> = {space} SHELL($1)/;


##
## Command finishers:
##

slap = {enter} Empty();# <<<>>>
