#NoTrayIcon

;;;
;;; Test subprocess for my Subprocess extension
;;;

; Timing case (no-op):
arg = %1%
if (arg = -1) {
  Exit 0
}


Sleep, 5000
MsgBox, 4144, SwitchTo, %1%|%2%|%3%|
Send, *

Exit %1%
