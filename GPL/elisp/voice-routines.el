;;; 
;;; Misc. elisp routines called by Vocola commands
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
  (require 'rmail)
  (load "~/elisp/line-numbers"))


;; 
;; Change how set mark interacts with transient mark mode: <<<>>>
;; 

(defun mdl-set-mark-command (arg)
"Set mark at where point is, or jump to mark.
With no prefix argument, set mark, push old mark position on local mark
ring, and push mark on global mark ring.
With argument, jump to mark, and pop a new position for mark off the ring
(does not affect global mark ring).

Unlike the normal set-mark-command, this version always leaves the
mark deactivated."
   (interactive "P")
   (set-mark-command arg)
   (deactivate-mark)
)
(global-set-key [?\C- ] 'mdl-set-mark-command)
;(global-set-key [?\C-@] 'mdl-set-mark-command)

(defun mdl-exchange-point-and-mark-nomark  ()
  "Like `exchange-point-and-mark' but without activating the mark."
  (interactive)
  (exchange-point-and-mark)
  (setq mark-active nil))
(global-set-key [?\C-x ?\C-x] 'mdl-exchange-point-and-mark-nomark)


;; 
;; hack for DNS 9.5 spacing bug: <<<>>>
;; 

(defun mdl-noop ()
  (interactive)
  nil
)

(define-key special-event-map [?\C-\S-a] 'mdl-noop)
(define-key special-event-map [f5]       'mdl-noop)


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


;; 
;; code for goto message command:
;; 

(defun mdl-rmail-show-message (arg)
  "Like rmail-show-message, but interactive."
  (interactive "NGoto message number: ")
  (rmail-show-message arg))


;; 
;; code for loop on index command:
;; 

(defun mdl-index-of (string character)
  "return index of first occurrence of character in string or -1 if it doesn't 
occur."
  (let ((index 0) (result -1))
    (while (< index (length string))
      (if (char-equal (elt string index) character)
	  (setq result index))
      (setq index (+ index 1))
      )
    result
    )
)

(defun mdl-loop-on-index (arg index)
  "Like loop on index <indexvariable> command, but prompts for index variable 
name and optional type."
  (interactive "p\nsIndex variable [type and] name: ")
  (let (justname)
    (setq justname (substring index (+ (mdl-index-of index 32) 1)))
    (insert "for (" index "=0; " justname "<")
    (save-excursion
      (insert "; " justname "++)")
      )
    )
  )


;; 
;; code for various indent commands (e.g., indent message):
;; 

(defun mdl-insert-string (start end string)
  "Insert string at the start of each line of current region."
  (interactive "r\nsEnter prefix string: ")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)

      (goto-char start)
      (while (< (point) (point-max))
	(beginning-of-line)
	(insert string)
	(forward-line 1)
	)
      )
    )
)


;; 
;; code for rename file and buffer & move file directory:
;; 

(load "~/elisp/moving_open_buffers")


;; 
;; code for fix spaces:
;; 

(defun mdl-fix-spaces ()
  "\
Attempt to remove extra spaces inserted by DNS on the current line:

  Remove extra single space after left delimiters and before
  right delimiters on current line; also remove extra single
  space before comma or dot.

Also adds back the single space removed by justification after
simily/frowny faces (e.g., ':-)  X' justifies to ':-) X'; ditto for
':-(  X').
"
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point-at-bol) (point-at-eol))

      (goto-char (point-min))
      (while (search-forward "( " nil t) 
	; don't remove space after ':-(':
	(if (not (eq (char-before (- (point) 2)) ?-))
	    (replace-match "(" nil t)))
      (goto-char (point-min))
      (while (search-forward " )" nil t) (replace-match ")" nil t))

      (goto-char (point-min))
      (while (search-forward "{ " nil t) (replace-match "{" nil t))
      (goto-char (point-min))
      (while (search-forward " }" nil t) (replace-match "}" nil t))

      (goto-char (point-min))
      (while (search-forward "< " nil t) (replace-match "<" nil t))
      (goto-char (point-min))
      (while (search-forward " >" nil t) (replace-match ">" nil t))

      (goto-char (point-min))
      (while (search-forward " ," nil t) (replace-match "," nil t))
      (goto-char (point-min))
      (while (search-forward " ." nil t) (replace-match "." nil t))

      (goto-char (point-min))
      (while (search-forward-regexp ":-\\([)(]\\) \\([^ ]\\)" nil t) 
	  (replace-match ":-\\1  \\2"))
      
      )
    )
  )


;; 
;; code for fix sentence:
;; 

(defun mdl-fix-sentence ()
  "Find end of previous sentence (must be on current line before
point) and fix spacing and capitalization."
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point-at-bol) (point))
      (if (search-backward-regexp "\\.[ ]+" nil t) 
	  (progn
	   (replace-match ".  " nil t)
	   (capitalize-word 1)
	   )
	(if (search-backward-regexp "\\." nil t) 
	  (progn
	   (replace-match ".  " nil t)
	   (capitalize-word 1)
	   )
	  )
	)
      )
    )
  )


;; 
;; code for export buffer command:
;; 

(defun mdl-temporary-pathname (filename)
  "Return the local pathname for ~/Tmp/<filename>, substituting 
foil:~/Tmp/<filename> from home PC and work:~/Tmp/<filename> from work PC."
  (if (not (eq window-system 'w32))
      (concat "~/Tmp/" filename)
    (if (string= (getenv "COMPUTERNAME") "MDL")
	(concat "p:\\Tmp\\" filename)
      (concat "w:\\Tmp\\" filename))))

(defun mdl-export-buffer-to-l1 (arg)
  "Write contents of current buffer to the file UNIX ~/Tmp/l1."
  (interactive "p")
  (let ((file (mdl-temporary-pathname "l1")))
    (write-region (point-min) (point-max) file nil nil nil nil)))


;; 
;; code for "print buffer":
;; 

(defun mdl-encodable-by (coding-system)
  "Returns true if current buffer is encodable by coding-system"
  ; requires Emacs version 22 or higher
  (eq nil (unencodable-char-position (point-min) (point-max) coding-system)))

(defun mdl-print-buffer ()
  (interactive)
  (if (eq window-system 'w32)
      (error "print buffer doesn't work from PC Emacs yet") ; <<<>>>
    (if (mdl-encodable-by 'latin-1)
	(let ((coding-system-for-write 'latin-1)
	      (file (mdl-temporary-pathname "buffer.Latin-1"))
	      (coding-system-require-warning t)
	      )
	  (write-region (point-min) (point-max) file nil nil nil nil)
;	  (shell-command (concat "echo -P\$PRINTER " file))
	  (shell-command (concat "enscript -P\$PRINTER " file))
	  )
      (let ((coding-system-for-write 'utf-8)
	    (file (mdl-temporary-pathname "buffer.UTF-8"))
	    (coding-system-require-warning t)
	    )
	(write-region (point-min) (point-max) file nil nil nil nil)
	(shell-command (concat "echo paps --font='Courier 10' --paper letter "
			       file " > "
			       (mdl-temporary-pathname "buffer.ps")
			       " && ")
		       "Fred")
	))))


;; 
;; code for reopen that in ..., copy buffer {filename|pathname}, etc.:
;; 

(defun mdl-copy-buffer-filename (arg)
  "Push the fully qualified filename of the current buffer onto the
kill ring.  With prefix argument, push just the filename and not the
directory it's in."
  (interactive "P")
  (let ((full-name (or buffer-file-name "")))
    (if (not arg)
	(kill-new full-name)
      (kill-new (file-name-nondirectory full-name)))))


(defun mdl-local-pathname-to-PC-pathname (&optional absolute)
  "Pushes onto the kill ring an absolute PC filename for accessing via
a PC the file locally accessible via pathname absolute.

If absolutely is omitted or nil, defaults to a pathname to the
file the current buffer is visiting (error if none).

Requires: this Emacs is running on a PC, foil, or a master work
machine."
  (interactive "slocal absolute pathname to convert: ")
  (if (not absolute)
      (setq absolute (buffer-file-name (current-buffer))))
  (if (eq window-system 'w32)
      (kill-new absolute)
    (let* ((hostname (getenv "HOST"))
	   (work-p   (not (equal hostname "foil")))
	   (drive    (if work-p "w:" "p:"))
	   (relative (replace-regexp-in-string "^/home/mdl" "" absolute))
	   (flipped  (replace-regexp-in-string "/" "\\\\" relative))
	   (final    (concat drive flipped)))
      (kill-new final)
      ;final
    )))


;; 
;; Code for start-word, pull-word:
;; 

(defun mdl-start-word (arg)
  "Like forward-word, but moves to start of next word."
  (interactive "p")
  (while (> arg 0)
    (setq arg (- arg 1))
    (let ((boundary1 (save-excursion (forward-word 1) (backward-word 1) (point)))
	  (boundary2 (save-excursion (forward-word 2) (backward-word 1) (point)))
	  )
      (if (< (point) boundary1)
	  (goto-char boundary1)
	(if (< (point) boundary2)
	  (goto-char boundary2)
	  (goto-char (point-max))
	  )
	)
      )
    )
  )
(define-key mode-specific-map "s" 'mdl-start-word)


;; 
;; Code for load commands, blue save ...:
;; 

(defun mdl-save-visiting-buffer ()
  "Try and save buffer if and only if it is visiting a file."
  (interactive)
  (if buffer-file-name
      (save-buffer)
    nil))


;; 
;; Code for [recent] buffer menu
;; 

(defun mdl-buffer-menu ()
  "Like buffer menu but sorted on file name."
  (interactive)
  (if (get-buffer "*Buffer List*")
      (kill-buffer "*Buffer List*"))
  (buffer-menu)
  (if (or (< emacs-major-version 24)
	  (< emacs-minor-version 3))
      (Buffer-menu-sort 2)
    (Buffer-menu-sort 3))
)

(defun mdl-recent-buffer-menu ()
  "Like buffer menu but sorted on most recent buffer."
  (interactive)
  (if (get-buffer "*Buffer List*")
      (kill-buffer "*Buffer List*"))
  (buffer-menu)
  (if (or (< emacs-major-version 24)
	  (< emacs-minor-version 3))
      (Buffer-menu-sort nil)
    )
)

;; 
;; Experimental: invert commands...
;; 

(defun mdl-window-top ()
  "Move point to start of current window."
  (interactive)
  (goto-char (window-start)))


;; 
;; Experiment: <<<>>>
;; 

; wrap only lines more than 80 chars long excluding any mail indent:
(defun mdl-wrap-long-lines (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((s start)
	    e)
	(while (< s (point-max))
	  (goto-char s)
	  (looking-at "\\([ >]*>[ ]?\\)?")
	  (forward-line 1)
	  (setq e (point))
	  (if (> (- e (match-end 0)) 81)  ; count includes \n at end...
	      (let ((fill-prefix (buffer-substring s (match-end 0)))
		    (fill-column 72))
		(fill-region s e nil t))) ; do not squeeze spaces
	  (setq s e)
	  )))))
