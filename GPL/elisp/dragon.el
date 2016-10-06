;;;
;;; dragon.el: code to smooth out the Dragon NaturallySpeaking/Gnuemacs 
;;;            interaction
;;;
;;;
;;;     This code does not make Gnuemacs fully select-and-say, but you
;;; will be able to select and correct your dictation to Emacs since
;;; you last issued a voice command or clicked the mouse.  It also
;;; allows you to correctly dictate text containing non-ASCII
;;; characters like accented letters to Emacs.
;;;
;;;
;;; Author:  Mark Lillibridge
;;; Version: 13
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

(eval-when-compile 
  (require 'pc-select)
  (require 'sgml-mode))
(eval-when-compile 
  (if (< emacs-major-version 22)
      (defmacro with-no-warnings (&rest body) (cons 'progn body))))


;;; 
;;; Correctly handle Dragon's selection and correction of recently
;;; dictated text via standard Windows keys (e.g., shifted arrow keys,
;;; deleting a selection by a single backspace).
;;; 

;; 
;; Dragon expects to be able to make Windows-type selections via the
;; shifted arrow keys and then delete them with a single backspace.
;; 
;; How to do this depends on the Gnuemacs version:
;; 
;;       For recent versions (23+), by default shift arrow keys create an
;;   active region via transient mark mode (on by default).  However,
;;   typing text/backspace does not delete that region unless we turn on
;;   delete-selection-mode.
;; 
;;       For earlier versions, turning on PC selection mode enables
;;   shift arrow keys, transient mark mode, and delete-selection-mode.
;; 
;; Note: neither of these binds control C to copy, control X to cut,
;; and the like.
;; 

(if (>= emacs-major-version 23)
    (delete-selection-mode)
  (progn
    ; don't bind extra PC compatibility keys:
    (setq pc-select-selection-keys-only 1)
    ; as a side effect this makes {ctrl+x}{ctrl+x} deactivate mark:
    (with-no-warnings (pc-selection-mode))))


;;
;; Dragon's correction code occasionally tries to use control C to copy 
;; the current selection for some reason (it doesn't appear to ever try 
;; and paste it back).  
;;
;; Attempt to tolerate this by making control C a nop when used before 
;; the (shifted) left/right arrow keys and backspace.
;;

(dolist (key '([left] [right] [S-left] [S-right]))
  ; bind to control c prefix:
  (define-key mode-specific-map key (lookup-key (current-global-map) key)))
(define-key mode-specific-map [backspace] 'backward-delete-char-untabify)

; 
; SGML and derived modes (e.g., HTML mode) define control C <left/right>
; as commands, which interferes with these bindings.
;
; Here we override those bindings to prevent interference with correction.
; If you need those commands, rebind them to other keys.
;
(add-hook 'sgml-mode-hook
	  (lambda ()
	    (dolist (key '([left] [right] [S-left] [S-right]))
	      (define-key sgml-mode-map 
		(vconcat [?\C-c] key)
		(lookup-key (current-global-map) key)))
	    ))


;;
;; Rebind space to self-insert-command in mini-buffer to avoid confusing
;; Dragon correction routines:
;;
;; (Otherwise, spaces in voice typos get eaten, preventing correct that.)
;;
(define-key minibuffer-local-completion-map " "  'self-insert-command)
(define-key minibuffer-local-completion-map "\t" 'minibuffer-complete)
(define-key minibuffer-local-must-match-map " "  'self-insert-command)
(define-key minibuffer-local-must-match-map "\t" 'minibuffer-complete)


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



;;; 
;;; Allow Dragon to enter non-ASCII characters:
;;; 

;;
;;     Dragon attempts to insert non-ASCII characters (e.g., accented
;; letters) using Window's hold down alt key and press 4 numeric
;; keypad digits method.  The following code implements the part of
;; this input method used by Dragon for Gnuemacs.  In particular, it
;; assumes input of the form {alt+NumKey0} {alt+NumKeyA} {alt+NumKeyB}
;; {alt+NumKeyC}, A, B, & C digits, to enter the Windows-1252 character
;; with number ABC (in decimal).  E.g., use {alt+NumKey0}
;; {alt+NumKey2} {alt+NumKey3} {alt+NumKey9} to enter an i with 2 dots
;; above it (character number 239).
;;
;;     This code works whether or not Emacs is using the alt key as a
;; meta-modifier (this turns A-kp-0 into M-kp-0, for example).
;;
;;     If you're using an X server, you'll need to make sure that at
;; least the left alt key (the one used by Dragon for inputting
;; characters) is passed to X applications.  For Reflection X, this is
;; settings->keyboard->Alt key reserved for Windows->Left: no check
;; mark.  I recommend passing the right alt key to Windows if possible
;; so that you can use the alt+space menu in macros via
;; {RightAlt+space}.  Unfortunately, the PC version of Gnu emacs
;; doesn't seem to be able to capture only the left alt key.
;;
;;     This code was inspired by code originally written by Jonathan
;; of the VoiceCoders list.
;;

(defun mdl-get-digit (event)
  "Convert events with form M-kp-n or A-kp-n, n in 0..9, to
the integer n.  For other events, returns some number between 0
and 9 inclusive."
  (if (symbolp event)
      (let* ((as-string (symbol-name event))
	     (char      (aref as-string (- (length as-string) 1))))
	(if (< char 48)
	    0
	  (if (< char 58)
	      (- char 48)
	    0)))
    0))

(defun mdl-convert-Windows-1252-to-Unicode (code)
  (or (assoc-default code '((128 . 8364)
			    (130 . 8218)
			    (130 . 8218)
			    (131 . 402)
			    (132 . 8222)
			    (133 . 8230)
			    (134 . 8224)
			    (135 . 8225)
			    (136 . 710)
			    (137 . 8240)
			    (138 . 352)
			    (139 . 8249)
			    (140 . 338)
			    (142 . 381)
			    (145 . 8216)
			    (146 . 8217)
			    (147 . 8220)
			    (148 . 8221)
			    (149 . 8226)
			    (150 . 8211)
			    (151 . 8212)
			    (152 . 732)
			    (153 . 8482)
			    (154 . 353)
			    (155 . 8250)
			    (156 . 339)
			    (158 . 382)
			    (159 . 376)
			    )) code))

(defun mdl-handle-alt (prompt)
  "Read next 3 keyboard events and interpret them as alt numpad
digits specifying a character number in decimal.  E.g., M-kp-0
M-kp-6 M-kp-4 yields the character 64 (@)."
  (let ((k1 (mdl-get-digit (read-event)))
	(k2 (mdl-get-digit (read-event)))
	(k3 (mdl-get-digit (read-event))))
    (vector (mdl-convert-Windows-1252-to-Unicode (+ (* k1 100) (* k2 10) k3)))))

; handling of translation keymaps was improved in version 23:
(if (>= emacs-major-version 23)
    (progn
      (defvar input-decode-map)
      (define-key input-decode-map [M-kp-0] 'mdl-handle-alt)
      (define-key input-decode-map [A-kp-0] 'mdl-handle-alt))
    (progn
      (define-key function-key-map [M-kp-0] 'mdl-handle-alt)
      (define-key function-key-map [A-kp-0] 'mdl-handle-alt)
      ; function-key-map is only used if no binding is found in the
      ; current keymap; unfortunately, isearch binds everything so we
      ; have to unbind our keys there for our translation to work:
      (define-key isearch-mode-map [M-kp-0] nil)
      (define-key isearch-mode-map [A-kp-0] nil)))
