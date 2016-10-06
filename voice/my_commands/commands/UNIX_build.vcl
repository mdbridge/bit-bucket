### 
### Voice commands for dealing with source code repositories
### 
###   See also UNIX_build_git.vcl for git commands,
###            x_item.vcl for per file operations.
### 

include "string.vch";
include "control.vch";

include "projects.vch";


<bar_more> := (bar more = " | more");


## 
## Make commands:
## 

<action> := ( make
	    | build		   = "make -j4"
            | build all		   = "make -j4 all"
            | clean		   = "make clean"
	    | clean up		   = "cleanup"
	    | recursively clean up = "cleanup_tree"
            );

<action> here = $1 {enter};



## 
## Subversion commands:
## 
##   These are for the current subtree rooted at at .; see
## x_item.vcl for per file operations.
## 

<svn> := ( update | status | full status="status -v -u" 
      	 | shallow status="status --depth=immediates" 
      	 | diff | commit 
         | log | full log="log -v" 
	 | edit ignore properties="propedit svn:ignore ."
	 | ignore="propedit svn:ignore ."
	 | help | info );

sub <svn> here [<bar_more>] = "svn $1" $2{enter};

changed status here = "svn status | grep -v '^?'" {enter};

# svn propset svn:mime-type "application/octet-stream" <file>



## 
## Commit messages:
## 

# with GIT, 1st line should be brief summary, then blank line, then details

  # use once have written log message:
confirm commit = 
    If(Window.Match("work emacs|foil emacs|green emacs|orange emacs"),
       Vocola.Error("attempted to accidentally close a main window"))
    {ctrl+x}{ctrl+s} {ctrl+x}{ctrl+c};

commit via xterm = 
    "setenv EDITOR     'emacs -nw'" {enter}
    "setenv GIT_EDITOR 'emacs -nw'" {enter};
