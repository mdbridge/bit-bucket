### 
### Gnuemacs commands I no longer use:
### 


#  move to end of N word from cursor:
LeftNRight       =           {esc}b {esc}f;
LeftNRight 2..20 = {ctrl+u}$1{esc}b {esc}f;

#  move to start of N word from cursor:
RightNLeft             =           {esc}f {esc}b;
RightNLeft 2..20       = {ctrl+u}$1{esc}f {esc}b;

RightNLeft 1..20 1..20 = {ctrl+u}$1{esc}f {ctrl+u}$2{left};


#
# Destroy macros
#

  # kill the rest of the current line; joins the next line if at end of line
destroy = {ctrl+k};

 # kill next N  lines :
destroy <my0to99> =
	{ctrl+u} $1 {ctrl+k};

