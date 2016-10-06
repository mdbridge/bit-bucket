;;;
;;; GNU Emacs customizations -- last updated 9/28/2016:
;;;
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

(eval-when-compile 
  (if (< emacs-major-version 22)
      (defmacro with-no-warnings (&rest body) (cons 'progn body))))
(eval-when-compile 
  (load "~/elisp/browse-kill-ring"))


;; 
;; Microsoft Windows-specific customizations:
;; 

(if (eq window-system 'w32)
    (progn
      (cd "~")
      (set-frame-name "PC emacs")

      ; use better font on Windows:
      (defvar mdl-font 
	"-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")
      (set-frame-font mdl-font)
      (add-to-list 'default-frame-alist `(font . ,mdl-font))
      ))


;; 
;; Control nagging:
;; 

(setq inhibit-startup-message t)
(setq large-file-warning-threshold nil)  ; Don't ask about opening large files
(setq delete-old-versions t)  ; Don't ask about deleting excess number of backups
(put 'eval-expression 'disabled nil)     ; allow shortcut: ESC-:
(put 'narrow-to-region 'disabled nil)


; Don't ask about killing running subprocesses:
(remove-hook 'kill-buffer-query-functions 'process-kill-buffer-query-function)
(defadvice yes-or-no-p (around hack-exit (prompt))
   (if (string= prompt "Active processes exist; kill them and exit anyway? ")
       (setq ad-return-value t)
      ad-do-it))
(ad-activate 'yes-or-no-p)

; Make sure we don't exit without confirmation:
(defun safe-exit-from-emacs ()
  "Exit from Emacs upon user confirmation"
  (interactive)
  (if (yes-or-no-p "Do you want to exit ")
      (save-buffers-kill-emacs)))
;(global-set-key "\C-x\C-c" 'safe-exit-from-emacs)  ; <<<>>>


;; 
;; Preserve original behavior of many commands:
;; 

(defvar recenter-positions)
(setq recenter-positions '(middle))  ; make ^L only re-center

; scroll page up/down (e.g., C-v) moves point to buffer ends:
(defvar scroll-error-top-bottom)
(setq scroll-error-top-bottom t)

(defvar line-move-visual)
(setq line-move-visual nil)          ; Move by logical lines, not visual lines

; make ^n add newlines at the end of the buffer as necessary:
(setq next-line-add-newlines t)

; just selecting an active region doesn't copy it anywhere automatically:
; (turning this on copies to PRIMARY (and hence windows clipboard) but
; *not* to kill ring...)
(defvar select-active-regions)
(setq select-active-regions  nil)
; but selecting via mouse dragging copies to the kill ring (and ...)
; (double click to select always copies to kill ring)
(setq mouse-drag-copy-region t)

; with X, copy/cut to CLIPBOARD and PRIMARY
;         paste from the one above that changed since last time
;           looked; ties broken in favor of CLIPBOARD
; (my xterm pastes to both, prefers PRIMARY to CLIPBOARD)
; (Windows programs paste to both (via xwin -clipboard); get most
;  recent of the two changed?)
(defvar x-select-enable-clipboard)
(setq x-select-enable-clipboard t)
(defvar x-select-enable-primary)
(setq x-select-enable-primary   t)

; middle mouse yanks normally rather than only from PRIMARY:
(global-set-key [mouse-2] 'mouse-yank-at-click)

(defvar isearch-lax-whitespace)
(setq isearch-lax-whitespace nil) ; make one space still match exactly one space

(electric-indent-mode -1)
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

; don't stop blinking when receive no input
(setq blink-cursor-blinks 0)


;; 
;; Miscellaneous key rebinding:
;; 

(define-key esc-map "g" 'goto-line)
(global-set-key "\C-z"  'undo)

; Swap meaning of space and tab in mini-buffer:  (overridden by dragon.el)
;(define-key minibuffer-local-completion-map " " 'minibuffer-complete)
;(define-key minibuffer-local-completion-map "\t" 'minibuffer-complete-word)
;(define-key minibuffer-local-must-match-map " " 'minibuffer-complete)
;(define-key minibuffer-local-must-match-map "\t" 'minibuffer-complete-word)


;; 
;; Other global customization:
;; 

(setq focus-follows-mouse nil)  ; Make frame switching work properly

(tooltip-mode -1)  ; Graphical ToolTips are too slow with remote connections
(defvar x-gtk-use-system-tooltips)
(setq x-gtk-use-system-tooltips nil)

(setq truncate-partial-width-windows nil)  ; Wrap too long lines

; make it easier to have two windows on the same buffer with different points:
(defvar switch-to-buffer-preserve-window-point)
(setq switch-to-buffer-preserve-window-point 'already-displayed)

(defvar help-window-select)
(setq help-window-select t)  ; select help windows more often

; prefer windows-1252 to latin-1, utf-8 to both (when valid):
(if (>= emacs-major-version 22)
    (progn
      (prefer-coding-system 'windows-1252);
      (prefer-coding-system 'utf-8)
      ))

; Turn on font locking in every buffer (e.g., use of color to show
; distinctions):
(global-font-lock-mode t)   ; not needed for Emacs 22 onwards...

; allow recursive use of mini-buffer (allows toggling minibuffer):
(setq enable-recursive-minibuffers t)

; Add newline at end of each file if needed:
(setq require-final-newline t)
  ; workaround stupid Red Hat bug where /usr/share/emacs/site-lisp/default.el
  ; reset's require-final-newline to 'query *after* this file runs...
  (setq inhibit-default-init t)

; Uniquify buffer names by adding parent directory names.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

; Ignore more machine generated files on completion:
(setq completion-ignored-extensions
      (append completion-ignored-extensions
	      (quote
	       (".bak" ".lpt"  ".aux" ".otl" ".err" ".lib"
		".dvi" ".PS" ".press" ".2bin" ".class" ".o"))))

; Where to print (in color):
(defvar ps-printer-name)
(if (equal (getenv "HOST") "foil")
    (setq ps-printer-name "ENVY_4500")
  (setq ps-printer-name "pal01p137"))  ; on Left

; Make find file at point work with gnu.* filenames:  (had a "gnu" originally):
(defvar ffap-newsgroup-heads		; entirely inadequate
  '("alt" "comp" "misc" "news" "sci" "soc" "talk")
  "Used by `ffap-newsgroup-p' if gnus is not running.")
(defvar thing-at-point-newsgroup-heads
  '("alt" "comp" "misc" "news" "sci" "soc" "talk")
  "Used by `thing-at-point-newsgroup-p' if gnus is not running.")


;;
;; Customize the most often used major modes:
;;

(load "~/elisp/my-modes")


;; 
;; Load third-party packages:
;; 

(load "~/elisp/align-regexp")      ; align by regular expressions

; Make "yank again" allow interactively choosing from the kill ring
; when *not* following a yank:
(load "~/elisp/browse-kill-ring")
(browse-kill-ring-default-keybindings)


;;
;; Functions to support Dragon macros
;;

  ; code to smooth out the Dragon NaturallySpeaking/Gnuemacs interaction:
(load "~/elisp/dragon")

  ; functions for implementing my leap API:
(load "~/elisp/leap")

  ; shortcutting occurs:
(load "~/elisp/occurs") 

  ;  allow jumping to any onscreen line by its line number mod 100:
(load "~/elisp/line-numbers")

(load "~/elisp/fragments")         ; moving and deleting by fragments

  ; misc. elisp routines called by Vocola commands:
(load "~/elisp/voice-routines")


;; 
;; Experiments:
;; 

  ; experimental implementation of undo correction:
(load "~/elisp/undo-correction")
