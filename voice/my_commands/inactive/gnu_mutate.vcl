### 
### Experimental replacement correction box
### 
### Return commands located in notepad_mutate.vcl
### 

## Note: set selection commands to not bring up the correction menu

## 
## Note: "mutate that" doesn't work with DNS 9.5 because of the
## {shift+ctrl+a} hack.  <<<>>>
## 

## "alter that" is an existing command that comments out a line then
## duplicates it

include "gnu.vch";

include "environment.vch";



ToBox() := AppBringUp(notepad) WaitForWindow("Untitled - Notepad");
#ToBox() := AppBringUp(PCfromPC(~/win32pad_1_5_10_4/win32pad.exe)) 
#	   WaitForWindow("[Untitled] - win32pad");

WaitClipboard() := IfHome( Wait(4000), Wait(100) );


mutate that = 
	HeardWord(select, that)
	{shift+left}{shift+right}   # cancel possible pending ^c...
	{esc}w  
	WaitClipboard()
	ToBox()
	{ctrl+a} {ctrl+v} 
	{ctrl+a} SendSystemKeys({NumKey-}); #HeardWord(correct, that);

mutate line =
	{shift} {home}{shift+end}
	{esc}w  
	{home}{shift+end}  # re-highlight line <<<>>>
	WaitClipboard()
	ToBox()
	{ctrl+a} {ctrl+v} 
	{ctrl+a} SendSystemKeys({NumKey-}); #HeardWord(correct, that);

#try selecting = HeardWord(select, that); 
