### 
### Gnuemacs commands for operating using snippets (parts of a line)
### 

include "gnu.vch";

include "string.vch";
include "letters.vch";


### 
### Manual cutting and pasting:
### 

# in gnu_short.vcl: marking, {copy,destroy} region, yank

MarkIt        = Mark();           # set mark at the point

Exchange Mark = Exchange();       # exchange mark and cursor


yank in place = {ctrl+u}{ctrl+y}; # as yank, but leaves point at start

yank again    = {esc}y;



### 
### Fetching snippets:
### 

# in gnu_short.vcl: fetch {rest,start,region}


yank above = {left}{right} {ctrl+p} Mark() {end}{esc}w Exchange() 
	     {ctrl+n}{ctrl+y};



### 
### Copying pre-specified patterns of text:
### 

  # attempt to copy the nearest e-mail address:
copy address =
	LeapRegex(d, "[-a-z0-9._?=]*@")                        HighMark()
	LeapRegex(d, "[^-a-z0-9._?=@]\|\(\.[^-a-z0-9._?=@]\)") HighCopy();


# 
# Copy next URL after point; leaves point at end of URL:
# 
CopyURL() := LeapRegex(d, '\(http\|https\|file\):')      HighMark()
             LeapRegex(d, '\.?["<> ,)]\|\.?$')           Wait(100) HighCopy();

CopySplitURL() := LeapRegex(d, '\(http\|https\|file\):') HighMark()
	{down}{home}
	LeapRegex(d, '\.?["<> ,)]\|\.?$')
	LeapRegex(u, '\(http\|https\|file\):')           Wait(100) HighCopy();

  # attempt to copy the nearest URL:
copy       URL  = CopyURL();
copy split URL  = CopySplitURL();



### 
### Viewing URLs:
### 

include "URLs.vch";

Ready() := Clipboard.Set("x");

# 
# attempt to view the URL in the kill ring (aka, ^y) in the lookup browser:
# 
ViewURL()          := Clipboard.WaitForNew("x",5) Lookup(Clipboard.Get());
ViewURLIn(browser) := Clipboard.WaitForNew("x",5) LookupIn($browser, 
							   Clipboard.Get());

view       URL [<r>] = Ready() When($1,LineMod($1)) CopyURL() ViewURL();
view split URL       = Ready() CopySplitURL()        ViewURL();

view       URL (in|with) <browser> = Ready() CopyURL()             ViewURLIn($2);
view split URL (in|with) <browser> = Ready() CopySplitURL()        ViewURLIn($2);



### 
### Interaction with other programs:
### 

## 
## Turn on and off emacs's export/import of killed values:
## 

close borders = Elisp("(setq interprogram-cut-function   nil)")
		Elisp("(setq interprogram-paste-function nil)");

open  borders = Elisp("(setq interprogram-cut-function   'x-select-text)")
		Elisp("(setq interprogram-paste-function "
		           # below is an alias for x-selection-value in 24+;
			   # switch at some point as old name obsolete in 24.1
                          "'x-cut-buffer-or-selection-value)");


#(global-set-key [(escape) (meta w)]
#  (lambda ()
#    (interactive)
#    (eval-expression
#      '(setq interprogram-cut-function
#             'x-select-text))
#    (kill-ring-save (region-beginning) (region-end))
#    (eval-expression
#      '(setq interprogram-cut-function nil))))
#
#(global-set-key [(escape) (control y)]
#  (lambda ()
#    (interactive)
#    (eval-expression
#      '(setq interprogram-paste-function
#             'x-cut-buffer-or-selection-value))
#    (yank)
#    (eval-expression
#      '(setq interprogram-paste-function nil))))
