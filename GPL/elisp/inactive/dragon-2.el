;;;
;;; dragon.el: code to smooth out the Dragon NaturallySpeaking/Gnuemacs 
;;; interaction
;;;
;;; E.g., deals with problems due to the fact that gnuemacs is a 
;;; nonstandard application in Dragon NaturallySpeaking's eyes.
;;;
;;;
;;; Author: Mark Lillibridge
;;;
;;; (c) Copyright 2004-2016 Hewlett Packard Enterprise Development LP
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

;; Version: 2


;;
;; Dragon's correction code occasionally tries to use control C to copy 
;; the current selection for some reason (it doesn't appear to ever try 
;; and paste it back).  
;;
;; Attempt to tolerate this by making control C a nop when used before 
;; the (shifted) left/right arrow keys.
;;
;; (This particular problem can also be solved by using cua-mode, but
;; that changes many user-visible command bindings.)
;;

; bind to control c prefix:
(define-key mode-specific-map [left] 'backward-char)
(define-key mode-specific-map [right] 'forward-char)


;;
;; Attempt to allow voice editing of interactive search strings:
;;
;; Any attempt to select or correct the search string should cause the string 
;; to be edited in the mini-buffer.  "Type" new line to exit with a 
;; non-incremental search and line feed to resume the incremental search.
;;
;; Side effect: The left arrow key can no longer be used to implicitly exit 
;; from an incremental search.
;;

; We use the following function to get the effect of search-exit-option 
; being set to 'edit just for the left arrow key that Dragon uses to 
; start selecting a selection:
;
(defun mdl-isearch-other-control-char-with-edit-exit ()
  "\
Behaves like isearch-other-control-char when search-exit-option is 
set to 'edit, at least for simple one key functions."
  (interactive)
  (let* ((key (this-command-keys))
	 (keylist (listify-key-sequence key)))
    (apply 'isearch-unread keylist)
    (isearch-edit-string)))

; make the (shifted) left arrow key in incremental search mode start 
; editing the search string in the mini-buffer before it takes effect:
(define-key isearch-mode-map [left]
    'mdl-isearch-other-control-char-with-edit-exit)


;;
;; Rebind space to self-insert-command in mini-buffer to avoid confusing
;; Dragon correction routines:
;;
;; (Otherwise, spaces in voice typos get eaten, preventing correct that.)
;;
(define-key minibuffer-local-completion-map " " 'self-insert-command)
(define-key minibuffer-local-completion-map "\t" 'minibuffer-complete)
(define-key minibuffer-local-must-match-map " " 'self-insert-command)
(define-key minibuffer-local-must-match-map "\t" 'minibuffer-complete)


;;
;; Define shift-space as an _elastic space_.
;;
;; Intended for use in voice macros, typing an elastic space types a space 
;; only when it is not already preceded by white space.
;;
;; Most voice macros that start with a space should be 
;; changed to start with an elastic space instead.  This avoids getting 
;; an extra space when they are invoked in emacs at a point where there is 
;; already a space.
;;
;; Example: I define "plus" as "{shift+space}+ "
;;
;; Note that because most applications treat shift-space as an ordinary 
;; space, such macros still work outside of emacs, abet without the 
;; anti-space redundancy feature.
;;
(defun mdl-elastic-space ()
  "Insert a space iff there is no space/tab/newline preceding the point."
  (interactive)
  (if (char-equal (preceding-char) 32)      ; space
      (insert "")
    (if (char-equal (preceding-char) 9)     ; tab
	(insert "")
      (if (char-equal (preceding-char) 10)  ; newline
	  (insert "")
	(insert " ")))
  )
)

; bind shift-space to elastic-space:
(global-set-key [?\S- ] 'mdl-elastic-space)


;;
;; Code for KillASpace macros for fixing up extra spaces sometimes 
;; inserted by Dragon NaturallySpeaking.
;;
;; Also useful for retroactively compounding words and converting 
;; space-padded symbols (" + ") to their non-space-padded form.
;; Indeed, it can be easier to intentionally dictate text with extra 
;; spaces then kill them by these macros.
;;
;; Originally written by John MacCormick
;;

(defun mdl-kill-spaces (n)
  "Kill the last n spaces."
  (interactive "Nnumber of spaces to kill: ")
  (save-excursion
    (let ((i 0))
      (while (< i n)
	(search-backward " ")
	(delete-char 1)
	(setq i (+ i 1))))))

(define-key esc-map "" 'mdl-kill-spaces)
