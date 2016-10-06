### 
### Voice commands for working on the Vocola tool (not with it)
### 

include "string.vch";
include "locale_PC.vch";

NatLink() := "C:\NatLink\NatLink";


## 
## Importing then installing Vocola:
## 

include "import.vch";

  # need to make first
import Vocola = SyncRsync("-force -replace-dir", 
                          @foil:~/voice/Vocola_development/Vocola_2/
		            build/Vocola/*,
		          ~/setup/Vocola);

install Vocola = ShellExecute(PC(~/setup/Vocola/install.bat));


import safe Vocola = SyncRsync("-force -replace-dir", 
                          @foil:~/voice/Vocola_development/Vocola_2/
		            build/Vocola/*,
		          ~/setup/Vocola_safe);

install safe Vocola = ShellExecute(PC(~/setup/Vocola_safe/install.bat));


## 
## Running the Python compiler manually:
## 

Compile(compiler, Vocola_source, destination) :=
    'C:\Python27\python.exe "' $compiler 
          '" -extensions "C:\NatLink\NatLink\Vocola\extensions\extensions.csv" '
	  '-numbers zero,one,two,three,four,five,six,seven,eight,nine '
	  '-log_stdout '
          '-suffix _vcl -f "' $Vocola_source '" "' 
          $destination '"';


run compiler manually =
    Compile(UNIX(foil:~/voice/Vocola_development/Vocola_2/src/exec/vcl2py.py),
            PC(~\NatLink\Vocola), NatLink() \macrosystem);
#           PC(~\NatLink\Vocola), PC(~/scratch/foo));


## 
## Miscellaneous:
## 

# tramp/ange-ftp to this server doesn't work because it uses Windows
# NT, which has a different directory format




  # for testing runtime error output:
please divide by 0..10 = Eval(1/$1);


## 
## Experimental:
## 

download Vocola documentation =
    prompt{enter} "mget *.asp"{enter} prompt{enter};

# copyright in masterPage.html

#upload Vocola documentation =
#"put Extensions.asp" {enter}
#"put UsingExtensions.asp" {enter}
#"put CreatingExtensions.asp" {enter}
#;

include "numbers.vch";

Item(n) := {space} \$_$n {ctrl+x}\$;

upload Vocola item <my0to99> =
    Item($1);

upload unofficial item <my0to99> =
    Item($1);
