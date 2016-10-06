;;;
;;; occurs.el: 
;;;
;;;   mdl-occur is same as occur except that if there is only one occurrence,
;;; jumps directly to that occurrence.  A case sensitive version is also 
;;; available.
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

;; Version: 1

(defun mdl-occur (regexp &optional nlines)
  "Show all lines following point containing a match for REGEXP.
Display each line with NLINES lines before and after,
 or -NLINES before if NLINES is negative.
NLINES defaults to list-matching-lines-default-context-lines.
Interactively it is the prefix arg.

The lines are shown in a buffer named *Occur*.
It serves as a menu to find any of the occurrences in this buffer.
\\[describe-mode] in that buffer will explain how.

Exception: if there is only one occurrence, jump directly to that occurrence.
"
  (interactive "sList lines matching regexp: \nP")

; put below in if you have *Occur*'s buffers with one occurrence hanging around
;  (if (get-buffer "*Occur*")
;    (kill-buffer "*Occur*"))

  (list-matching-lines regexp nlines)

  (if (get-buffer "*Occur*")
    (progn
      (if (with-current-buffer "*Occur*"
	    (equal "1 " (buffer-substring 1 3)))
	; only one occurrence found:
        (mdl-select-occurrence 2)

	; more than one occurrence found:
	(when (with-current-buffer "*Occur*"
		(> (count-lines (point-min) (point-max)) 18))
	  (switch-to-buffer "*Occur*")
	  (delete-other-windows))
      )
    )
  )
)

(defun mdl-occur-no-fold (regexp &optional nlines)
  "Show all lines following point containing a match for case-sensitive REGEXP.
Display each line with NLINES lines before and after,
 or -NLINES before if NLINES is negative.
NLINES defaults to list-matching-lines-default-context-lines.
Interactively it is the prefix arg.

The lines are shown in a buffer named *Occur*.
It serves as a menu to find any of the occurrences in this buffer.
\\[describe-mode] in that buffer will explain how.

Exception: if there is only one occurrence, jump directly to that occurrence.
"
  (interactive "sList lines matching case-sensitive regexp: \nP")

  (save-current-buffer
    (let ((case-fold-search  t))
      (mdl-occur regexp nlines))
    )
)


(defun mdl-select-occurrence (n)
  "Goto occurrence on line N of *Occur* buffer, killing that buffer."
  (interactive "NGoto occurrence on line number number: ")
  (set-buffer "*Occur*")
  (goto-char (point-min))
  (forward-line (- n 1))
  (occur-mode-goto-occurrence)
  (delete-windows-on "*Occur*")
  (kill-buffer "*Occur*")
)
