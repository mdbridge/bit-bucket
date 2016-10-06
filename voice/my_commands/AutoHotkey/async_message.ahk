;;; 
;;; Produce an always-on-top error dialog box, which fades after a few seconds
;;; 
;;; This program should be called asynchronously.
;;; 

#NoTrayIcon


Title = %1%
Error = %2%
Delay = %3%  ; in seconds, can be a decimal

MsgBox, 4144, %Title%, %Error%, %3%

ExitApp, 0
