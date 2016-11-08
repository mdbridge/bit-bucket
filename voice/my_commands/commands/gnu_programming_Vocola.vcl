### 
### Gnuemacs voice commands for dealing with Vocola
### 

include "gnu.vch";
include "extended_string.vch";

#include "macro_files.vch";
include "command_files.vch";
include "include_files.vch";
include "vocabulary_files.vch";



##
## Commands for writing Vocola macros  (see also _any_Vocola.vcl)
##

Vocola mode = Do(vocola-mode);

Vocola include                = 'include .vch";' {left_6} '"' Empty();
Vocola include <include_file> = 'include "' Split($1,"/",-1)  '";' {enter};



## 
## Loading macro/command source files:
## 

  # I didn't bother to properly handle x.vcl
PCFilename(command_filename) := Replace4(.\ $command_filename,
					\browser, \Chrome_,
                                        \gnu_,    \emacs__, 
					\UNIX_,   \xwin___,
					\x_,      \xwin__);


#buffer <macro_file>      macros     = 
#    FindFile(UNIX(~/voice/my_commands/mini-commands/ $1{enter} ));
buffer <command_file>    commands   = 
    FindFile(UNIX(~/voice/my_commands/commands/      $1{enter} ));
buffer <include_file>    includes   = 
    FindFile(UNIX(~/voice/my_commands/commands/      $1{enter} ));
buffer <vocabulary_file> vocabulary = 
    FindFile(UNIX(~/voice/my_vocabulary/vocabulary/  $1{enter} ));


buffer <command_file> commands on PC = 
    FindFile(PC( ~/NatLink/Vocola/ PCFilename($1)) {enter} );

buffer <include_file> includes on PC = 
    FindFile(PC( ~/NatLink/Vocola/ $1)             {enter} );

buffer scrap commands =
    FindFile(PC( ~/NatLink/Vocola/_scrap.vcl) {enter} );



buffer <command_file> commands compiled = 
    FindFile(PC( 'C:\NatLink\NatLink\MacroSystem\'
                       Replace( PCFilename($1), ".vcl", "_vcl*.py")) {enter} );

buffer scrap commands compiled =
    FindFile(PC( 'C:\NatLink\NatLink\MacroSystem\_scrap_vcl.py') {enter});


# 
# The below commands use dir edit, killing all but the relevant files:
# 

DirEdit(pattern) :=
	FindFile(UNIX( ~/voice/my_commands/commands {enter} )) U g
	%m $pattern {enter} tk
	%m "__" {enter} %m "~" {enter}  k
	{up_2}{m_2}k;    # kill "." and ".." lines...


<type> := ( global   = "^_" 
          | anywhere = "^_any[_\.]" 
          | gnu      = "^gnu[_\.]" 
          | home     = "@mdl"
          | Unix     = "^UNIX[_\.]" 
          | work     = "@lillibridg3"
          | X        = "^x[_\.]" 
          );

<type> commands = DirEdit($1);
