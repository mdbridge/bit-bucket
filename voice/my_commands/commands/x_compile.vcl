### 
### Voice commands for compiling within Emacs
### 

include "gnu.vch";


Compile(command) := Do(mdl-save-visiting-buffer)
                    Do(compile) EraseToStart() $command {enter};

Compile2(directory,command) := Compile("(cd $directory; $command)");


## 
## Frequent compilation targets:
## 

compile my vocabulary = Compile2(~/voice/my_vocabulary, make);


CompileCommands(command) := Compile2(~/voice/my_commands, $command);

      compile my commands = CompileCommands(make);
force compile my commands = CompileCommands("make clean; make all");

         load my commands = CompileCommands(make)
	                    Wait(1500) HeardWord(import, commands);


## 
## Navigating the resulting compilation errors:
## 

next     error [1..10] = When($1,{ctrl+u}$1) {ctrl+x}`;
previous error         =         {ctrl+u}-1  {ctrl+x}`;

  # try and skip warnings:
first error = {ctrl+x}b *compilation* {enter} {ctrl+x}1 {esc}< 
      	      Leap(d, ": error:") {home} {right};
