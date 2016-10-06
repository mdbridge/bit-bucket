### 
### General voice commands for Gnuemacs
### 
### Additional core commands can be found in:
### 
###   gnu_buffer.vcl, gnu_window.vcl, gnu_file.vcl
###   gnu_short.vcl
###   gnu_movement.vcl, gnu_locate.vcl
###   gnu_range.vcl, gnu_snippet.vcl
###   gnu_filling.vcl
###   gnu_spacing.vcl
###   gnu_repeated.vcl
###   

include "gnu.vch";
include "DNS.vch";
include "letters.vch";


## 
## General modifiers:
## 

prefix [<my0to99>] = {ctrl+u}$1;

meta X <_anything> [(slap={enter})] = 
    {esc}x When($1,Lower(Replace($1," ","-")) {tab}$2, NoCaps());


## 
## Undoing operations:
## 

undo [that] = {ctrl+x}u;


##
## Getting help:
##

help key [<letter> [(slap={enter})]] = {ctrl+h} When($1,$1$2,?);

help for (function=f|variable=v) = {ctrl+h}$1{enter};


apropos [phrase <_anything>]     = {ctrl+h}a When($1,$1{enter});

apropos documentation            = Do(apropos-documentation);


## 
## Exiting/suspending Emacs:
## 

exit    emacs = {ctrl+x}{ctrl+c} Empty();
answer (yes|no) = $1{enter};

suspend emacs = {ctrl+x}{ctrl+z};       # I rebind {ctrl+z}


## 
## Dynamic completion:
## 

complete       = {esc}/;      # repeat immediately for more choices...
completeNSpace = {esc}/ " ";


## 
## Character sets:
## 

which    character = "{ctrl+x}=";
describe character = Do(describe-char);

  # external character set names:
<external> := ( UTF      = utf-8
              | UTF 8    = utf-8
              | Latin    = iso-8859-1
              | Windows  = windows-1252
              | big five = Big5
              | raw      = raw-text
              | emacs    = emacs-mule
              | ISO 7    = iso-8859-7
              | EUC      = euc-kr-unix
              );

  # internal Emacs character set names:
<internal> := ( raw text  = raw-text-unix 
              | UTF       = mule-utf-8-unix 
              | UTF 8     = mule-utf-8-unix 
              | undecided = undecided-unix 
              | Latin     = iso-latin-1 
              | windows   = windows-1252 
              );

encode as        <external> = Do2(encode-coding-region, $1);
decode (as|from) <external> = Do2(decode-coding-region, $2);

# encode as Windows, decode as UTF
    	 
  # choose character set for next I/O command like write region:
with coding system <internal> = {ctrl+x}{enter}c $1{enter};

  # teleport non-ASCII is better but does not show possible character sets:
#  # kludge to show non-ASCII characters in buffer, what character sets
#  # can encode them:
#show non-ASCII = {esc}< {esc}> {ctrl+x}{enter}c undecided-unix{enter} 
#	         Do2(write-region, ~/Tmp/crap) y Wait(200) {ctrl+g};


## 
## Miscellaneous commands:
## 

<section> := (all=-a | 1 | 2 | 3 | 4 | 5);
man page [<section>] [for <_anything>] = Do(man) When($1,"$1 ") 
      When($2,$2 Mark() {home} ToggleRegion() {enter});
  # for all case:
(next=n|previous=p) man page = {esc}$1;


I spell buffer = Do(ispell-buffer);

shell command  = {esc}!;
# see also shell {command|transform} <range>

show hostname  = Shell(hostname);
show calendar  = Do(calendar);

untabify region = Do(untabify);

write    region = Do(write-region);

  # g for refresh, k to kill process under cursor
show processes  = Do(proced);

convert buffer to ASCII = {esc}< {esc}> 
                          {ctrl+u}{esc}'|ruby ~/http/convert_to_ASCII.rb';
