;;;
;;; leap.el: 
;;;
;;;
;;; Author: Mark Lillibridge
;;;
;;; (c) Copyright 2009-2011,2016 Hewlett Packard Enterprise Development LP
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

(defvar mdl-isearch-original-start nil)
(defvar mdl-isearch-adjusted-start -1)

(defun mdl-adjust-search-start (n)
  (setq mdl-isearch-adjusted-start (+ (point) n))
  (if (> mdl-isearch-adjusted-start (point-max))
      (setq mdl-isearch-adjusted-start (point-max)))
  (if (< mdl-isearch-adjusted-start (point-min))
      (setq mdl-isearch-adjusted-start (point-min)))
  (setq mdl-isearch-original-start (point))
  (goto-char mdl-isearch-adjusted-start))

(defun mdl-restore-search-start ()
  (if (and mdl-isearch-original-start
	   (= mdl-isearch-adjusted-start isearch-opoint))
      (progn
	(setq isearch-opoint mdl-isearch-original-start)
	(setq mdl-isearch-original-start nil))))
  




(defun mdl-isearch-leap-forward (regexp-p no-recursive-edit)
  "\
Do incremental search forward after advancing one character if not at 
the end of a buffer.
With a prefix argument, do a regular expression search instead.
See \\[isearch-forward] for more information."
  (interactive "P\np")
  (mdl-adjust-search-start 1)
;  (if (not (= (point) (point-max)))
;      (forward-char 1))
  (isearch-forward regexp-p no-recursive-edit))

(define-key mode-specific-map "s" 'mdl-isearch-leap-forward)
(define-key mode-specific-map "r" 'isearch-backward)


(defun mdl-isearch-exit ()
  "\
Exit search.  If successful, move to start of match.  Otherwise, go 
back to the search starting point then quit.

If searching forward, the search starting point is defined to be one 
character (if possible) before the point at which incremental search 
was invoked.  This affects where any mark is placed.

(This function is intended to be paired with isearch-backward and 
mdl-isearch-leap-forward to implement leaping.)
" 
  (interactive)

  ; fixup search starting point retroactively on the assumption that 
  ; any forward incremental search was started by
  ; mdl-isearch-leap-forward:
;  (if isearch-forward
;      (if (not (= isearch-opoint (point-min)))
;	  (setq isearch-opoint (- isearch-opoint 1))))
  (mdl-restore-search-start)

  (if isearch-success
      (goto-char (match-beginning 0))
    (goto-char isearch-opoint))
  (isearch-done)
  (if (not isearch-success)
      (signal 'quit nil))
)

(define-key isearch-mode-map [right] 'mdl-isearch-exit)
