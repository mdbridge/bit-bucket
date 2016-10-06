;;;
;;; alist.el: 
;;;
;;;    Code to improve the Rmail rmail-output-file-alist feature.
;;;
;;;
;;; Author: Mark Lillibridge, Version: 1
;;;
;;; (c) Copyright 2016 Hewlett Packard Enterprise Development LP
;;;
;;; Copyright (C) 1985-2015 Free Software Foundation, Inc.
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

;; Replace rmail-output-to-rmail-file command (normally "o" in Rmail)
;; with one that applies the association list in
;; rmail-output-file-alist to the string result of calling
;; (mdl-get-alist-headers) with the current buffer restricted to the
;; current message instead of applying the association list to the
;; entire current message.

(add-hook 'rmail-mode-hook
  (function (lambda()
	      (define-key rmail-mode-map "o" 'mdl-rmail-output-to-rmail-file)
	      (define-key rmail-mode-map [menu-bar classify output]
		'("Output (Rmail)..." . mdl-rmail-output-to-rmail-file))
	      )))

; should be the same as rmail-output-to-rmail-file except calls
; mdl-rmail-output-read-rmail-file-name instead of
; rmail-output-read-rmail-file-name.
(defun mdl-rmail-output-to-rmail-file (file-name &optional count)
  "Append the current message to an Rmail file named FILE-NAME.
If the file does not exist, ask if it should be created.
If file is being visited, the message is appended to the Emacs
buffer visiting that file.
If the file exists and is not an Rmail file, the message is
appended in inbox format, the same way `rmail-output' does it.

The default file name comes from `rmail-default-rmail-file',
which is updated to the name you use in this command.

A prefix argument N says to output N consecutive messages
starting with the current one.  Deleted messages are skipped and don't count."
  (interactive
   (list (mdl-rmail-output-read-rmail-file-name)
	 (prefix-numeric-value current-prefix-arg)))
  (rmail-output-to-rmail-file file-name count))

(defun mdl-rmail-output-read-rmail-file-name ()
  "Read the file name to use for `rmail-output-to-rmail-file'.
Set `rmail-default-rmail-file' to this name as well as returning it."
  (let ((default-file
	  (let (answer
		tail
		(headers (save-excursion
			   (set-buffer rmail-buffer)
			   (mdl-get-alist-headers)))
		)
	    (setq tail rmail-output-file-alist)
	    ;; Suggest a file based on a pattern match.
	    (while (and tail (not answer))
	      (if (string-match (car (car tail)) headers)
		  (setq answer (eval (cdr (car tail)))))
	      (setq tail (cdr tail)))
	    ;; If no suggestions, use same file as last time.
	    (expand-file-name (or answer rmail-default-rmail-file)))))
    (let ((read-file
	   (expand-file-name
	    (read-file-name
	     (concat "Output message to Rmail file: (default "
		     (file-name-nondirectory default-file)
		     ") ")
	     (file-name-directory default-file)
	     (abbreviate-file-name default-file))
	    (file-name-directory default-file))))
      ;; If the user enters just a directory,
      ;; use the name within that directory chosen by the default.
      (setq rmail-default-rmail-file
	    (if (file-directory-p read-file)
		(expand-file-name (file-name-nondirectory default-file)
				  read-file)
	      read-file)))))


;; 

