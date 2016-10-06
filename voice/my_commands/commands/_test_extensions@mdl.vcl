# -*- encoding: windows-1252 -*-
### 
### Voice commands for testing my extensions:
### 

include "string.vch";


## 
## Environment:
## 

       type environment variable (home=HOME|unknown) = Env.Get($1);
safely type environment variable (home=HOME|unknown) = Env.Get($1, "UNKNOWN");



## 
## Date:
## 

what       day  is it = Date.Now();
what short day  is it = Date.Now("%m/%d/%y");
what       time is it = Date.Now("%I:%M %p");



## 
## Module variable:
## 

<v> := (first | second | third);

         set  <v> value 1..10 = Variable.Set  ($1, $2);
       unset  <v> value       = Variable.Unset($1);

         show <v> value       =  "$1 = " Variable.Get($1, UNDEFINED) "!{enter}";
unsafely show <v> value       =  "$1 = " Variable.Get($1)            "!{enter}";



## 
## Module clipboard:
## 

  # try naïve here:
set clipboard to <_anything> = Clipboard.Set($1);

  # repr shows Windows-1252 as \xFF...; paste to verify naïve:
show clipboard = MsgBoxConfirm(EvalTemplate("repr(%s)", Clipboard.Get()), 64, 
		               "Current contents of clipboard");

show clipboard with default = 
    MsgBoxConfirm(EvalTemplate("repr(%s)", Clipboard.Get("DEFAULT")), 64, 
                  "Current contents of clipboard or default");


UTF8(text) := EvalTemplate('u"$text".encode("UTF-8")');

paste Greek Delta    = Clipboard.SetUTF8(UTF8(\u03b4)) {ctrl+v};
paste Greek sentence = Clipboard.SetUTF8(UTF8("Naïve.  A lowercase Greek delta is written \u03b4 and an uppercase one is written \u0394.")) {ctrl+v};

  # this escapes all non-ASCII, using \xFF for Windows-1252 and
  # \uUUUU otherwise
show Unicode clipboard =
    MsgBoxConfirm(EvalTemplate("%s.decode('utf-8').encode('unicode-escape')",
                               Clipboard.GetUTF8()), 64, 
		  "Current Unicode contents of clipboard");

show quoted Unicode clipboard =
    MsgBoxConfirm(EvalTemplate("repr(%s.decode('utf-8').encode('unicode-escape'))",
                               Clipboard.GetUTF8()), 64, 
		  "Current quoted Unicode contents of clipboard");


save    clipboard [1..10] = When($1,Clipboard.Save(clip_$1),   Clipboard.Save());
restore clipboard [1..10] = When($1,Clipboard.Restore(clip_$1),Clipboard.Restore());


wait for clipboard change =
    Clipboard.Set("xyzzy")
    Clipboard.WaitForNew("xyzzy")
    MsgBoxConfirm(Clipboard.Get(), 64, "Clipboard changed!");
wait quickly for clipboard change =
    Clipboard.Set("xyzzy")
    Clipboard.WaitForNew("xyzzy", 2)
    MsgBoxConfirm(Clipboard.Get(), 64, "Clipboard changed!");



## 
## Window:
## 

show window ID    = MsgBoxConfirm(Window.ID(),    64, "Window ID");
show window class = MsgBoxConfirm(Window.Class(), 64, "Window class");
show window title = MsgBoxConfirm(Window.Title(), 64, "Window title");
show window app   = MsgBoxConfirm(Window.App(),   64, "Window app");

show window info  = MsgBoxConfirm(
    Window.ID() ":"			 Eval("chr(13)")
    "  title: 	 "    Window.Title()     Eval("chr(13)")
    "  class: 	 "    Window.Class()     Eval("chr(13)")
    "  app:         " Window.App()	 Eval("chr(13)")
    "  top-left:   "  Window.GetPosition() Eval("chr(13)")
    "  size:       "  Window.GetSize() Eval("chr(13)")
    "  minimized?: "  Window.Minimized() Eval("chr(13)")
    "  maximized?: "  Window.Maximized()   Eval("chr(13)")
    , 64, "Window info");

list windows <_anything> = When($1, Window.ListWindows($1), Window.ListWindows());


<action> := (minimize = Window.Minimize() | maximize = Window.Maximize() |
	     restore = Window.Restore()   | close = Window.Close());

try and <action> window = $1 TTSPlayString(Window.Success());


  # need more specification kinds here ...  <<<>>>
<target> := ("foil emacs"|nonexistent|prompt=app>cmd.exe);

try raw switch to <target> =
    Window.Go_($1)
    " success=" Window.Success() {enter};

try switch to <target> =
    Window.Go($1)
    " success=" Window.Success() {enter};


please sir is this Emacs = If(Window.Match("emacs"), 
                              MsgBoxConfirm("Yes", 64, "Is this emacs?"),
                              MsgBoxConfirm("No",  64, "Is this emacs?"));


time switching = Repeat(50, Window.Go("command prompt Alpha")
                            Window.Go("command prompt Bravo")) done;



## 
## Subprocess:
## 

include "locale_PC.vch";

invoke in background =
    Subprocess.Async(PC(~pf32\AutoHotkey\AutoHotkey.exe), 'AutoHotkey.exe', 
       PC(~/AutoHotkey/subprocess_test.ahk), 99, arg2, arg3) foreground;

bad background invoke = Subprocess.Async(nowhere, nowhere);


invoke 0..10 in foreground =
    Subprocess.Sync(PC(~pf32\AutoHotkey\AutoHotkey.exe), 'AutoHotkey.exe', 
       PC(~/AutoHotkey/subprocess_test.ahk), $1, arg2, arg3) foreground;

unchecked invoke 0..10 in foreground =
    Subprocess.Sync(PC(~pf32\AutoHotkey\AutoHotkey.exe-), 'AutoHotkey.exe', 
       PC(~/AutoHotkey/subprocess_test.ahk), $1, arg2, arg3) foreground;
  
  # does not set exit code, but rather runtime errors out...
bad foreground invoke = Subprocess.Sync(nowhere, nowhere);

clean bad foreground invoke = Subprocess.Sync(nowhere-, nowhere);

print exit code = MsgBoxConfirm(Variable.Get(exit_code), 0, title);


wait for me = Subprocess.System("timeout 5 && timeout 5");


time spawning = Repeat(100, 
    Subprocess.Sync(PC(~pf32\AutoHotkey\AutoHotkey.exe), 'AutoHotkey.exe',
       PC(~/AutoHotkey/subprocess_test.ahk), -1)) done;



## 
## example2 file, currently disabled:
## 

#carefully show (home=HOME|unknown) = Env.Get2($1);
#
#please multiply (0=Math.Mult() | 1=Math.Mult(3) | 2 = Math.Mult(2,3) | bad=Math.Mult(x)) = $1;
#
#test polynomial = Polynomial.Eval(1,2,3);
#
#please add <_anything> = File.Append("C:\Users\Mark\Documents\test.txt", $1);



## 
## Vocola:
## 

#$set MaximumCommands 2;

test            abort = Vocola.Abort() should not be reached;
test            error = Vocola.Error(message!!) should not be reached;
test functional error = Eval(1 * Vocola.Error(stop!)) not here;

test alert = Vocola.Alert(watch) 3 Vocola.Alert(out);
test print = Vocola.Print(watch) 3 Vocola.Print(out);



################################################################################

## 
## Mouse:
## 


## 
## Symbols:
## 

code for monster <_anything> = Symbols.Monster($1);


## 
## Auto (experimental use of winGuiAuto): <<<>>>
## 

#dump <_anything> = Auto.dump($1);
#
#dump the unnamed = Auto.dump ("iTunes");
#dump another unnamed = Auto.dump ("DragonPad");
#dump yet another unnamed = Auto.dump ("Firefox");

#try minimize = Auto.clickButton(iTunes,"Minimize");

#Try DJ = Auto.clickStatic(iTunes,"iTunes DJ");


AwaitChange(actions) :=
    Variable.Set(:target, Window.ID())
    $actions
    Repeat(5where0, 
        If(Window.Match(ID> Variable.Get(:target)), Wait(100)));


wait for change = AwaitChange("") Vocola.Print("changed!");
