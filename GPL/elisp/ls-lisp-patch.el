;;; 
;;; Bug fix for ls-lisp-insert-directory:
;;; 
;;; Fix bug where it fails on PC with p:/X/
;;; see <<<>>> lines
;;; 
;;; This bug exists in versions 22 and 23.
;;; 
;;; 
;;; (c) Copyright 2016 Hewlett Packard Enterprise Development LP
;;; 
;;; Copyright (C) 1992, 1994, 2000-2015 Free Software Foundation, Inc.
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

(require 'ls-lisp)

(defun ls-lisp-insert-directory
  (file switches time-index wildcard-regexp full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.  This is an internal function
optionally called by the `ls-lisp.el' version of `insert-directory'.
It is called recursively if the -R switch is used.
SWITCHES is a *list* of characters.  TIME-INDEX is the time index into
file-attributes according to SWITCHES.  WILDCARD-REGEXP is nil or an *Emacs
regexp*.  FULL-DIRECTORY-P means file is a directory and SWITCHES does
not contain `d', so that a full listing is expected."
  (if (or wildcard-regexp full-directory-p)
      (let* ((dir (file-name-as-directory file))
	     (default-directory dir)	; so that file-attributes works
	     (file-alist
	      (directory-files-and-attributes dir nil wildcard-regexp t
					      (if (memq ?n switches)
						  'integer
						'string)))
	     (now (current-time))
	     (sum 0)
	     (max-uid-len 0)
	     (max-gid-len 0)
	     (max-file-size 0)
	     ;; do all bindings here for speed
	     total-line files elt short file-size fil attr
	     fuid fgid uid-len gid-len)
	(cond ((memq ?A switches)
	       (setq file-alist
		     (ls-lisp-delete-matching "^\\.\\.?$" file-alist)))
	      ((not (memq ?a switches))
	       ;; if neither -A  nor -a, flush . files
	       (setq file-alist
		     (ls-lisp-delete-matching "^\\." file-alist))))
	(setq file-alist
	      (ls-lisp-handle-switches file-alist switches))
	(if (memq ?C switches)		; column (-C) format
	    (ls-lisp-column-format file-alist)
	  (setq total-line (cons (point) (car-safe file-alist)))
	  ;; Find the appropriate format for displaying uid, gid, and
	  ;; file size, by finding the longest strings among all the
	  ;; files we are about to display.
	  (dolist (elt file-alist)
	    (setq attr (cdr elt)
		  fuid (or (nth 2 attr) "") ; <<<>>>
		  uid-len (if (stringp fuid) (string-width fuid)
			    (length (format "%d" fuid)))
		  fgid (or (nth 3 attr) "") ; <<<>>>
		  gid-len (if (stringp fgid) (string-width fgid)
			    (length (format "%d" fgid)))
		  file-size (or (nth 7 attr) 0)) ; <<<>>>
	    (if (> uid-len max-uid-len)
		(setq max-uid-len uid-len))
	    (if (> gid-len max-gid-len)
		(setq max-gid-len gid-len))
	    (if (> file-size max-file-size)
		(setq max-file-size file-size)))
	  (setq ls-lisp-uid-d-fmt (format " %%-%dd" max-uid-len))
	  (setq ls-lisp-uid-s-fmt (format " %%-%ds" max-uid-len))
	  (setq ls-lisp-gid-d-fmt (format " %%-%dd" max-gid-len))
	  (setq ls-lisp-gid-s-fmt (format " %%-%ds" max-gid-len))
	  (setq ls-lisp-filesize-d-fmt
		(format " %%%dd"
			(if (memq ?s switches)
			    (length (format "%.0f"
					    (fceiling (/ max-file-size 1024.0))))
			  (length (format "%.0f" max-file-size)))))
	  (setq ls-lisp-filesize-f-fmt
		(format " %%%d.0f"
			(if (memq ?s switches)
			    (length (format "%.0f"
					    (fceiling (/ max-file-size 1024.0))))
			  (length (format "%.0f" max-file-size)))))
	  (setq files file-alist)
	  (while files			; long (-l) format
	    (setq elt (car files)
		  files (cdr files)
		  short (car elt)
		  attr (cdr elt)
		  file-size (nth 7 attr))
	    (and attr
		 (setq sum (+ file-size
			      ;; Even if neither SUM nor file's size
			      ;; overflow, their sum could.
			      (if (or (< sum (- 134217727 file-size))
				      (floatp sum)
				      (floatp file-size))
				  sum
				(float sum))))
		 (insert (ls-lisp-format short attr file-size
					 switches time-index now))))
	  ;; Insert total size of all files:
	  (save-excursion
	    (goto-char (car total-line))
	    (or (cdr total-line)
		;; Shell says ``No match'' if no files match
		;; the wildcard; let's say something similar.
		(insert "(No match)\n"))
	    (insert (format "total %.0f\n" (fceiling (/ sum 1024.0))))))
	(if (memq ?R switches)
	    ;; List the contents of all directories recursively.
	    ;; cadr of each element of `file-alist' is t for
	    ;; directory, string (name linked to) for symbolic
	    ;; link, or nil.
	    (while file-alist
	      (setq elt (car file-alist)
		    file-alist (cdr file-alist))
	      (when (and (eq (cadr elt) t) ; directory
			 ;; Under -F, we have already decorated all
			 ;; directories, including "." and "..", with
			 ;; a /, so allow for that as well.
			 (not (string-match "\\`\\.\\.?/?\\'" (car elt))))
		(setq elt (expand-file-name (car elt) dir))
		(insert "\n" elt ":\n")
		(ls-lisp-insert-directory
		 elt switches time-index wildcard-regexp full-directory-p)))))
    ;; If not full-directory-p, FILE *must not* end in /, as
    ;; file-attributes will not recognize a symlink to a directory,
    ;; so must make it a relative filename as ls does:
    (if (file-name-absolute-p file) (setq file (expand-file-name file)))
    (if (eq (aref file (1- (length file))) ?/)
	(setq file (substring file 0 -1)))
    (let ((fattr (file-attributes file 'string)))
      (if fattr
	  (insert (ls-lisp-format
		   (if (memq ?F switches)
		       (ls-lisp-classify-file file fattr)
		     file)
		   fattr (nth 7 fattr)
				  switches time-index (current-time)))
	(message "%s: doesn't exist or is inaccessible" file)
	(ding) (sit-for 2)))))		; to show user the message!
