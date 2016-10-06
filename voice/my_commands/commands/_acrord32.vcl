### 
### Global voice commands for Adobe reader
### 

include "control.vch";
include "string.vch";
include "environment.vch";
include "locale_PC.vch";

include "import.vch";
include "switch.vch";
include "projects.vch";


  # *Adobe Reader and *Adobe Acrobat Reader DC
WaitFor() := WaitForWindow('*Adobe *Reader*', "", 2000);


## 
## Remembering windows showing PDFs:
## 

SetWindow(PDF) := Variable.Set(Adobe:$PDF, Window.ID());

GoWindow(PDF, if_successful) :=
    When(Variable.Get(Adobe:$PDF, ""),
        Window.Go_(ID> Variable.Get(Adobe:$PDF))
	If(Window.Success(),
	    WaitFor()  # in case window ID reused...
	    $if_successful));


## 
## Viewing PDFs:
## 

ViewRemotely(PDF) := 
    GoWindow($PDF, Wait(100) {alt+f4} Wait(2000))  # extra wait for Adobe exiting
    AppBringUp($PDF, UNIX($PDF)) 
    WaitFor() Wait(100) SetWindow($PDF);


Current()   := Variable.Get(Adobe:counter, 1);
Increment() := Variable.Set(Adobe:counter, Eval('(1 + ' Current() ')%3'));
Local(PDF)  := ~/scratch/imported_ Left(Experimental.Hash($PDF),6) _ Current().pdf;

ViewLocally(PDF) := 
    GoWindow($PDF, Wait(200) {alt+f4} Wait(200))
    Increment()
    SyncRsync("-force", @$PDF, Local($PDF))
    Subprocess.Run(
        IfHome(
            PC("~pf32/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe"),
	    PC("~pf32/Adobe/Reader 11.0/Reader/AcroRd32.exe")
        ), "/A pagemode=none " PC(Local($PDF))
     )
#    Subprocess.System(
#          # START runs Adobe reader in the background:
#        'START '
#	     # the following may help close the bookmarks toolbar:
#	     # (add ",page=#,fit=FitH after none"?)
# 	   '"downloading PDF..." "' 
#           IfHome(
#             PC("~pf32/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe") '" /A pagemode=none ',
#             PC("~pf32/Adobe/Reader 11.0/Reader/AcroRd32.exe") '" /A pagemode=none '
#	   )
#	   PC(Local($PDF))
#    )
    WaitFor() Wait(100) SetWindow($PDF);


view          <PDF> [1..20 [(down={PgDn})]] = 
    ViewLocally($1)   When($2, {shift+ctrl+n}$2{enter})  $3;
view          <PDF> [fit [page] 1..20] = 
    ViewLocally($1)   When($2, {ctrl+0} {shift+ctrl+n}$2{enter});
view remotely <PDF> [1..20 [(down={PgDn})]] = 
    ViewRemotely($1)  When($2, {shift+ctrl+n}$2{enter})  $3;


show    <PDF> = GoWindow($1, "");
destroy <PDF> = GoWindow($1, Wait(100) {alt+f4} Wait(200));

  # for testing:
manually import <PDF> = Rsync("-type -force", @$1, ~/scratch/imported.pdf);
