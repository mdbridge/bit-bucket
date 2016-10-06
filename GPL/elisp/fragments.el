;;;
;;; fragments.el: code to allow moving and deleting by fragments. 
;;;
;;; A fragment is part of an emacs word.  This implementation attempts to 
;;; divide words formed using Studley caps (remove spaces between original 
;;; words and capitalize the second and later words) into their component 
;;; words.
;;;
;;; E.g., "IOExceptionPTRRequested" is divided into "IO", "Exception",
;;; "PTR", and "Requested".
;;;
;;; NOTE: as of Emacs version 22, the sub word minor mode (^c^w to
;;; toggle) allows esc f/b to move by fragments.
;;;
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

;; Version: 3


;
; Definition of what a fragment is:
;
;   Fragments start where emacs words start and at a Studley caps division 
; point.
;
;   Fragments end where emacs words end and at a Studley caps division point.
;
;
; A Studley caps division point occurs at the & character in the following 
; two regular expressions:
;
;   [a-z]&[A-Z]          [A-Z]&[A-Z][a-z]
;
;
; Note that one fragment can end at the same point the next fragment 
; begins, unlike emacs words.
;

(defun mdl-lowercase-p (char)
  (and (<= ?a char)
       (<= char ?z)))
  
(defun mdl-uppercase-p (char)
  (and (<= ?A char)
       (<= char ?Z)))


(defun mdl-studley-caps-division-p ()
  (or
   (and (mdl-lowercase-p (preceding-char))
	(mdl-uppercase-p (following-char)))
   (and (mdl-uppercase-p (preceding-char))
	(save-excursion
	  (forward-char 1)
	  (and (mdl-uppercase-p (preceding-char))
	       (mdl-lowercase-p (following-char)))))))


(defun mdl-fragment-start-p ()
  "Return t iff point is at the start of a fragment."
  (or
   (= (point) (save-excursion (forward-word 1) (backward-word 1) (point)))
   (mdl-studley-caps-division-p)))

(defun mdl-fragment-end-p ()
  "Return t iff point is at the end of a fragment."
  (or
   (= (point) (save-excursion (backward-word 1) (forward-word 1) (point)))
   (mdl-studley-caps-division-p)))


;
; code for right/left-a-fragment macros:
;

(defun mdl-forward-fragment (arg)
  "Like forward-word, but using fragments instead of words."
  (interactive "p")
  (while (> arg 0)
    (setq arg (- arg 1))
    (forward-char 1)
    (while (not (mdl-fragment-end-p))
      (forward-char 1))
    )
  )

(defun mdl-backward-fragment (arg)
  "Like backward-word, but using fragments instead of words."
  (interactive "p")
  (while (> arg 0)
    (setq arg (- arg 1))
    (backward-char 1)
    (while (not (mdl-fragment-start-p))
      (backward-char 1))
    )
  )

; bind to control c prefix:
(define-key mode-specific-map "f" 'mdl-forward-fragment)
(define-key mode-specific-map "b" 'mdl-backward-fragment)
