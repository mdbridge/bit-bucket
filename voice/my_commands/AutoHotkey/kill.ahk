;
; Killing stupid Firefox container process:
;

#k::
Process,Close,plugin-container.exe
return


;
; Shortcuts for killing then restarting Dragon:
;

#+k::
GroupAdd, dragon, ahk_exe natspeak.exe
GroupAdd, dragon, ahk_exe dragonbar.exe
GroupAdd, dragon, ahk_exe loggerservice.exe
GroupAdd, dragon, ahk_exe dgnsvc.exe
GroupAdd, dragon, ahk_exe dgnuiasvc.exe
GroupAdd, dragon, ahk_exe dgnuiasvc_x64.exe
GroupAdd, dragon, ahk_exe dnsspserver.exe
GroupClose, dragon, A
;Process,Close,natspeak.exe
;Process,Close,dragonbar.exe
MsgBox, 4144, Dragon Sanctum,Dragon has been killed, 3
return

#+r::
Run C:\Program Files (x86)\Nuance\NaturallySpeaking12\Program\natspeak.exe
return

#+d::
Run C:\Program Files (x86)\Nuance\NaturallySpeaking14\Program\natspeak.exe
return



;
; Enable shift mouse wheel to scroll horizontally in PowerPoint:
;

#IfWinActive, ahk_exe powerpnt.exe
+WheelUp::ComObjActive("PowerPoint.Application").ActiveWindow.SmallScroll(0,0,0,3) 
+WheelDown::ComObjActive("PowerPoint.Application").ActiveWindow.SmallScroll(0,0,3,0)


;
; Experiments:
;

!^(::WinMove,A,,0,0,1280,1100
!^)::WinMove,A,,640,0,1280,1100
!^<::WinMove,A,,0,0,960,1100
!^>::WinMove,A,,960,0,960,1100
