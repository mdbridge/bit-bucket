### 
### Grammar for controlling the shell via numbering files
### 
### See also UNIX_item.vcl
### 

include "numbers.vch";
include "directories.vch";
include "machines.vch";
include "import.vch";

include "item_operation.vch";


## 
## Numbering files
## 

<kind> := (all=-A | recent=-t | directory=--group-directories-first);

  # the choice of which kind of files to number is sticky:
number [<kind>] files = 'set _o="$1"; CD .'{enter} Empty();



## 
## Navigating directories:
## 

descend [item] [1..9 hundred] <my0to99> = "CD" Item2($1,$2) {enter} Empty();

move back = "back; CD ."{enter} Empty();  # only works once



## 
## Command beginnings:
## 

shell <command> [<option> [<option>]] = $1$2$3 ' ';

shell <g> <git>                       = "git $2 ";

shell sub <Subversion>		      = "svn $1 ";

  # <<<>>>
(shell="" | bar= " | ") grep [(India=-i|Victor=-v)] <_anything> = 
    $1 "grep " When($2, "$2 ") When($3,'"$3" ');



##
## Command templates:
##

<transport> := (copy = cp | move = mv);

shell <transport> to   <UNIX> [slash <COM>] = 
    Place("$1 "," $2/" When($3,/$3));

shell <transport> from <UNIX> [slash <COM>] = 
    "set here=`pwd`{enter}"
    "CD " $2 When($3,/$3) "; cd \$here{ctrl+x}\$" {enter} Wait(500)
    Place("(cd " $2 When($3,/$3) "; $1 ",' "\$here")');


##
## Tar file creation (list and extract are regular operations):
##

shell tar create = "tar cvf ";

  # tarring up a subdirectory:
tar create item [1..9 hundred] <my0to99> = 
    "tar cvf" Item2($1,$2) .tar Item2($1,$2);


## 
## Macros for scp:  <<<>>>
## 

  # add -p to preserve times, permissions:
<scp> := ( secure copy = "scp" | secure recursive copy = "scp -r"
         | recursive secure copy = "scp -r" 
	 | secure clone = "scp -p -r");

CopyFrom(scp, origin) := Place("$scp " SHELL($origin), " .");
CopyTo  (scp, target) := Place("$scp ", "  " SHELL($target));

<scp> to   <machine> [/ <UNIX> [/ <COM>]] = 
    CopyTo  ($1,"$2:" When($3,$3/,~/) When($4,$4/));
old <scp> from <machine> [/ <UNIX> [/ <COM>]] = 
    CopyFrom($1,"$2:" When($3,$3/) When($4,$4/));


# <<<>>>
Quoted(pathname) := Replace('"' $pathname '"', '"~/', '~/"');
<scp> from <machine> [/ <UNIX> [/ <COM>]] = 
    Variable.Set(:remote, "$2:" When($3,$3/,~/) When($4,$4/))
    "source ~/bin/number_remote_files $2 '" 
         Quoted(When($3,$3/,~/) When($4,$4/))  
       "'" {enter} Wait(500)
    Place("$1 "," .");

FItem(n)               := \$_$n {ctrl+x}\$;
FItem2(hundreds, ones) := FItem(Eval(When($hundreds,$hundreds,0)*100 + $ones));

far item [1..9 hundred] <my0to99> = Variable.Get(:remote) FItem2($1,$2) {space};


##
## Opening local copies of files or directories
##

ItemToLocal(filename) :=
    Clipboard.Set(xyzzy)
    "set _dir=`pwd | sed 's|.*/home/[^/]*/*|%%/|'`"{enter}
    "echo -n `whoami`@`hostname --fqdn`:\$_dir @@" 
        $filename " | xclip"{enter}
    Clipboard.WaitForNew(xyzzy)
      # this passes @|mdl@foil.strangled.net:~/my/directory/$filename:
    MakeLocal('@|' Replace2(Clipboard.Get(), " @@ ",/, "%%","~"));


Windows open item [1..9 hundred] <my0to99> = 
    ItemToLocal(Item2($1,$2))
    AppBringUp(windows-open, Local());

Windows copy item [1..9 hundred] <my0to99> = 
    ItemToLocal(Item2($1,$2))
    AppBringUp(windows-open, Directory(Local()));
