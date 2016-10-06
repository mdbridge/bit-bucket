### 
### Voice commands using converting my e-mail from BABYL to MBOX format:
### 

include "gnu.vch";


## 
## Viewing the complained about message from the complaint:
## 

mail 0..9                = $1 j;
mail 0..9 0..9           = $1$2 j;
mail 0..9 0..9 0..9      = $1$2$3 j;
mail 0..9 0..9 0..9 0..9 = $1$2$3$4 j;


  # assume on complaint line
GetFolder() := {home} Leap(D+, "Rmail/") Leap(d, ":") {esc}w;

<op> := (convert | convert two=convert2);

<op> folder = GetFolder()
	HeardWord(black, area) Wait(1000) "$1 " {shift+Ins}
	Wait(10000) {enter};


  # goes to complained about message in ~/Rmail:
fall down hole = GetFolder()
     	{right} Leap(d, ":") CopyToRegister(~)
	Do(rmail-input) ~/Rmail/     {ctrl+y} {enter}
	Do(mdl-rmail-show-message) YankFromRegister(~) {enter};


Line(n) := {esc}< LeapRaw3(d+, {ctrl+q}{ctrl+j} {ctrl+q}{ctrl+j}, 1)
	{ctrl+u} $n {down};

body line 0..9                = Line($1);
body line 0..9 0..9           = Line($1$2);
body line 0..9 0..9 0..9      = Line($1$2$3);
body line 0..9 0..9 0..9 0..9 = Line($1$2$3$4);


## 
## Getting encoding information:
## 

describe current coding system = {esc}x describe-current-coding-system{enter};

             describe character = "{ctrl+x}=";
(full|fully) describe character = Do(describe-char);
describe character (full|fully) = Do(describe-char);

show buffer status = {ctrl+h}v enable-multibyte-characters {enter};


## 
## Editing messages (all require already in Rmail edit mode):
## 

# 
# Converting subject lines to use only ASCII:
# 

SubjectRegion() := {esc}< LeapRaw3(d+, "{ctrl+q}{ctrl+j}Subject:", 1)
		   {right} {ctrl+space}{end}{ctrl+x}{ctrl+x};

test me = SubjectRegion();

convert characters = {esc}':(rfc2047-encode (point) (mark))' {enter};


# 
# converting a message to be a MIME message:
# 

  # external character set names:
<coding> := ( UTF      = utf-8
            | Latin    = iso-8859-1
            | Windows  = windows-1252
            | big five = Big5
            | raw      = raw-text
            | emacs    = emacs-mule
	    | ISO 7    = iso-8859-7
	    | EUC      = euc-kr-unix
	    );

declare as <coding> = e {esc}<
	LeapRaw3(d, "{ctrl+q}{ctrl+j}{ctrl+q}{ctrl+j}", 1){right}
"X-MDL: added MIME headers below:"{enter}
#"MIME-Version: 1.0"{enter}
'Content-Type: text/plain; charset="$1"'{enter}
"Content-Transfer-Encoding: 8bit"{enter};


# 
# adding/replacing a charset specification:
# 

  # when a single-part MIME message, but headers don't give a charset:
specify as <coding> = e {esc}<
	Leap(d, "Content-Type:")
	{ctrl+o}
	"X-MDL: added charset below:" {down}{home}
	LeapRegexRaw3(d, "[;{ctrl+q}{ctrl+j}]", 1)
	'; charset="$1"';

add character set <coding> = '; charset="$1"';

replace with character set <coding> = {end} Leap4(u, ';', 1,  x)
	'; charset="$1"';


# 
# other changes:
# 

add encoding = {home}{ctrl+o} "Content-Transfer-Encoding: 8bit";

Duplicate(count) := {home}{ctrl+space} {end}{esc}w 
                    Repeat($count, {down}{home}{ctrl+o} {ctrl+y}{home});


new coding system = {esc}< Duplicate(1) X {up}{end}{esc}b{left};

<emacs> := ( raw text = raw-text-unix | UTF = mule-utf-8-unix 
	   | undecided = undecided-unix | Latin = iso-latin-1 
	   | windows = windows-1252 );


new coding system <emacs> = {esc}< Duplicate(1) X {up}
    {home}{ctrl+u}3{esc}f{right_2}
    {ctrl+space}{end}{ctrl+w} $1;


force coding [system] <emacs> = e 
    {esc}< Duplicate(1) X {up}
    {home}{ctrl+u}3{esc}f{right_2}
    {ctrl+space}{end}{ctrl+w} $1
    {ctrl+c}{ctrl+c}
    #{ctrl+x}b{enter}
    #{down_2}
;

next coding <emacs> = e 
    {esc}< Duplicate(1) X {up}
    {home}{ctrl+u}3{esc}f{right_2}
    {ctrl+space}{end}{ctrl+w} $1
    {ctrl+c}{ctrl+c}
    n
;


#mudge = HeardWord(next, coding, windows);
mudge 1..20 times = Repeat($1, HeardWord(next, coding, windows));

#stab = HeardWord(next, coding, UTF);
stab 1..20 times = Repeat($1, HeardWord(next, coding, UTF));




## 
## 
## 



encode as        <coding> = {esc}x encode-coding-region {enter} $1{enter};
decode (as|from) <coding> = {esc}x decode-coding-region {enter} $2{enter};

# encode as Windows, decode as UTF
    	 

with coding system <emacs> = {ctrl+x}{enter}c $1{enter};

write region = Do(write-region);

## 
## 
## 



exercise bug = {esc}x unrmail{enter} ~/mail/analysis/test{enter} 
	 ~/mail/analysis/test.mail{enter};

show (temp=" *temp*" | unrmail =" unrmail") buffer = {ctrl+x}{ctrl+b} $1{enter};



detect encoding = {esc}':' 
	"(detect-coding-region (point) (- (point-max) 1))"{enter};

detect priority encoding = {esc}':' 
	"(detect-coding-with-priority (point) (- (point-max) 1) "
		    "'((coding-category-emacs-mule . emacs-mule)))"
        {enter};		   

fix windows = e {esc}< {ctrl+space} {esc}> HeardWord(decode, as, Windows)
{esc}< {ctrl+space} {esc}> {ctrl+x}{enter}c undecided-unix{enter} Do2(write-region, ~/Tmp/crap) y Wait(200) {ctrl+g};






#show non-ASCII = {ctrl+x}{enter}c undecided-unix{enter} Do2(write-region, ~/Tmp/crap);

show non-ASCII = {esc}< {ctrl+space} {esc}> {ctrl+x}{enter}c undecided-unix{enter} 
	         Do2(write-region, ~/Tmp/crap) y Wait(200) {ctrl+g};



show info = {ctrl+u}0t 
     	    Do2(mdl-occur, 'charset\|coding-system\|[^{ctrl+q}{ctrl+a}-~]');



TypeRsync(source, destination, which, options) :=
                  '"' PCfromPC(~pf32/cwRsync/bin/rsync.exe) '" '
		  ' -z $options '
                  '"lillibridgem@ts-rhel5.hpl.hp.com:$source/$which" '
		  '"/cygdrive/c/' 
                       Replace(Replace($destination, "\", "/") , "C:/", "") '"';

    Rsync(source, destination, which, options) :=
                 ShellExecute(TypeRsync($source, $destination, $which, $options));

import webpages = Rsync(~/http/pages,  PCfromPC(~/scratch/pages), "*", "-t --progress -r");
