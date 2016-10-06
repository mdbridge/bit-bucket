;;; 
;;; Major mode for editing Vocola 2.X files
;;; 
;;; 
;;; Author: Mark Lillibridge
;;;
;;; (c) Copyright 2016 Hewlett Packard Enterprise Development LP
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; 

;;
;; *****************************************************************************
;;
;;  To add font-lock support for Vocola files, simply add the line
;;  (require 'vocola-mode) to your .emacs file. Make sure generic-mode.el
;;  is visible in your load-path as well.
;;  
;; *****************************************************************************

(eval-when-compile 
  (require 'generic))


;;;###autoload
(define-generic-mode 'vocola-mode
   ; Vocola comments always start with '#':
   (list "#")

   ; "keywords"; we include built-in commands here because the default
   ; built-in face is indistinguishable from normal text:
   (list
      ; Vocola keywords:
    "include"
      ; Vocola built-in commands:
    "Eval"
    "EvalTemplate"
    "Repeat"
    "Unimacro"
      ; Dragon calls:
    "ActiveControlPick"
    "ActiveMenuPick"
    "AppBringUp"
    "AppSwapWith"
    "Beep"
    "ButtonClick"
    "ClearDesktop"
    "ControlPick"
    "DdeExecute"
    "DdePoke"
    "DllCall"
    "DragToPoint"
    "GoToSleep"
    "HeardWord"
    "HTMLHelp"
    "MenuCancel"
    "MenuPick"
    "MouseGrid"
    "MsgBoxConfirm"
    "PlaySound"
    "RememberPoint"
    "RunScriptFile"
    "SendKeys"
    "SendDragonKeys"
    "SendSystemKeys"
    "SetMicrophone"
    "SetMousePosition"
    "SetNaturalText"
    "ShellExecute"
    "ShiftKey"
    "TTSPlayString"
    "Wait"
    "WaitForWindow"
    "WakeUp"
    "WinHelp"
    )

   
   '(
     ; Vocola's special characters:
     (":="                                    0 'font-lock-warning-face)
     ("[][()=|,;]"                            0 'font-lock-warning-face)
     ("\\(:\\)[ \n\t]"                        1 'font-lock-warning-face)

     ; extension calls:
     ("\\([a-zA-Z_][a-zA-Z_0-9]*\\.[a-zA-Z_0-9.]*\\)[ \t]*("  
                                              1 'font-lock-keyword-face)
     ; user function calls:
     ("\\([a-zA-Z_][a-zA-Z_0-9.]*\\)[ \t]*("  1 'font-lock-function-name-face)

     ("<_anything>"                           0 'font-lock-constant-face)
     ("<[a-zA-Z90-9_]+>"                      0 'font-lock-variable-name-face)
     ("[0-9]+\\.\\.[0-9]+"                    0 'font-lock-variable-name-face)

     ; Vocola references; not 100% reliable due to no preceeding \:
     ("\\([^\\]\\|^\\)\\(\\(\\$[0-9]+\\)+\\)" 2 'font-lock-type-face t)
     ("\\([^\\]\\|^\\)\\(\\(\\$[a-zA-Z_]+[a-zA-Z0-9_]*\\)+\\)" 
                                              2 'font-lock-type-face t)
    )

   ; which files are Vocola files:
   (list "\\.vcl\\'" "\\.vch\\'")

   ; additional setup processing:
   (list 'vocola-mode-setup-function)

   "Mode for Vocola files")


(defun vocola-mode-setup-function ()
    ; singles quotes delimit strings:
  (modify-syntax-entry ?' "\"")
    ; \ does not escape quotes:
  (modify-syntax-entry ?\\ ".")
)


; DNS uses Windows-1252 as it's coding system so Vocola does the same:
(if (>= emacs-major-version 22)
    (progn
      (modify-coding-system-alist 'file "\\.vch\\'" 'windows-1252)
      (modify-coding-system-alist 'file "\\.vcl\\'" 'windows-1252)
))


; 
; TODO:
;
; hack syntax table so - is a word?
; make sure \$foo etc. works
; indenting?
; color formal arguments?
; color _ when a space?
; color {...} ?
; add version number, change log
; fixup instructions for use
; imenu?


(provide 'vocola-mode)
