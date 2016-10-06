;;;
;;; printable-command.el:
;;;
;;;
;;; Author: Mark Lillibridge
;;;
;;; Version: 1
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


;;
;; The definition of what characters make up a printable command prefix.
;;

(defconst mdl-prefix1 ?@
  "The first character of the prefix for printable commands.")

(defconst mdl-prefix2 ?!
  "The second character of the prefix for printable commands.")

(defconst mdl-prefix1-internal '@-prefix
  "An unused event not previously bound to anything that we transform
mdl-prefix1 into so we can distinguish it from the character it
usually produces.

This event will be visible when the user types only part of a
printable command.")


;;
;; Functions for creating key maps and bindings:
;;

(defun mdl-make-sparse-keymap (&optional default-binding)
  "Construct and return a new sparse-keymap list.

The optional argument default-binding supplies a default binding for all 
events."
  (cons 'keymap (if default-binding
		    (cons (cons t default-binding) nil))))

(defun mdl-becomes-events-binding (events &optional keep-last-event)
  "Construct and return the keymap binding that replaces the key sequence
that ran it with the event sequence events followed by the last event
of the key sequence that ran it if keep-last-event is true."
  `(lambda ()
     (interactive)
     (setq unread-command-events (append (quote ,events)
					 ,(if keep-last-event
					      '(list last-input-event)
					    nil)
					 unread-command-events))))

;;
;; Transform mdl-prefix1 into mdl-prefix1-internal.
;;
;; As a side effect, transform mdl-prefix2 into mdl-prefix2-internal.     
;;
;; This transformation occurs once on input before commands are looked up.
;;

(keyboard-translate mdl-prefix1 mdl-prefix1-internal)

(defconst mdl-prefix2-internal (if (equal mdl-prefix1 mdl-prefix2)
				   mdl-prefix1-internal
				 mdl-prefix2))

;;
;; Setup printable command prefix bindings and keymaps for printable
;; commands:
;;
;;   Any occurrence of mdl-prefix1-internal that does not start a
;; defined printable command is replaced by mdl-prefix1, and command
;; lookup restarts.
;;

(defconst mdl-printable-command-keymap
  (mdl-make-sparse-keymap (mdl-becomes-events-binding
			   (list mdl-prefix1 mdl-prefix2-internal) t))
  "The key map used to look up globally-bound printable commands based
on the first key typed after the printable command prefix.

\\{mdl-printable-command-keymap}")

(global-set-key (vector mdl-prefix1-internal)
		(mdl-make-sparse-keymap
		 (mdl-becomes-events-binding (list mdl-prefix1) t)))

(global-set-key (vector mdl-prefix1-internal mdl-prefix2-internal)
		mdl-printable-command-keymap)

;
; We need a separate keymap for isearch because it does not inherit
; the global keymap.
;
;   (Key sequences undefined in isearch mode, but defined globally,
; cause isearch mode to exit then execute the corresponding global
; command.)
;

(defconst mdl-isearch-printable-command-keymap
  (mdl-make-sparse-keymap (mdl-becomes-events-binding
			   (list mdl-prefix1 mdl-prefix2-internal) t))
  "The key map used to look up isearch-mode printable commands based
on the first key typed after the printable command prefix.

\\{mdl-isearch-printable-command-keymap}")

(define-key isearch-mode-map (vector mdl-prefix1-internal)
  (mdl-make-sparse-keymap
   (mdl-becomes-events-binding (list mdl-prefix1) t)))

(define-key isearch-mode-map
  (vector mdl-prefix1-internal mdl-prefix2-internal)
  mdl-isearch-printable-command-keymap)


;;
;; As an escape mechanism, we define the printable command mdl-prefix1
;; mdl-prefix2 mdl-prefix1 to be equivalent to a special version of
;; mdl-prefix1 that cannot be part of any printable command prefix.
;;

(define-key mdl-printable-command-keymap (vector mdl-prefix1-internal)
  (mdl-becomes-events-binding
   (list mdl-prefix1)))

(define-key mdl-isearch-printable-command-keymap (vector mdl-prefix1-internal)
  (mdl-becomes-events-binding
   (list mdl-prefix1)))


;;
;; Define printable commands for incremental search forwards and backwards:
;;

; Starting a search:
(define-key mdl-printable-command-keymap (vector ?+) 'isearch-forward)
(define-key mdl-printable-command-keymap (vector ?-) 'isearch-backward)

; Repeating a search:
(define-key mdl-isearch-printable-command-keymap [?+] 'isearch-repeat-forward)
(define-key mdl-isearch-printable-command-keymap [?-]
  'isearch-repeat-backward)

; Make these printable commands noops when editing the search term so
; that we can use "correct that" on these commands:
(define-key minibuffer-local-isearch-map
  [mdl-prefix1-internal mdl-prefix2-internal ?+] "")
(define-key minibuffer-local-isearch-map
  [mdl-prefix1-internal mdl-prefix2-internal ?-] "")
