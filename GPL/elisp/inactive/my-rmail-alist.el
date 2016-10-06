;;;
;;; Improved alist feature; would require modification to work with Rmail 23+
;;;
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

; originally invoked via:
;
;; set rmail-output-file-alist to output of generate_alist.rb:
;(load "~/elisp/alist-data")
;
;; use my improved alist matching code:
;(load "~/elisp/my-rmail-alist")


(eval-when-compile 
  (require 'rmail)
  (require 'rmailsum)
  (require 'rmailout))


; 
; replace use of rmail-output-read-rmail-file-name with
; mdl-rmail-output-read-rmail-file-name:
; 

(add-hook 'rmail-mode-hook
  (function (lambda()
	      (define-key rmail-mode-map         "o"
		'mdl-rmail-output-to-rmail-file)
	      (define-key rmail-mode-map         [menu-bar classify output]
		'("Output (Rmail)..." . mdl-rmail-output-to-rmail-file))
	      )))

(add-hook 'rmail-summary-mode-hook
  (function (lambda()
	      (define-key rmail-summary-mode-map "o"      
		'mdl-rmail-summary-output-to-rmail-file)
	      (define-key rmail-summary-mode-map [menu-bar classify output]
		'("Output (Rmail)..." . rmail-summary-output-to-rmail-file))
	      )))

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

(defun mdl-rmail-summary-output-to-rmail-file (&optional file-name n)
  "Append the current message to an Rmail file named FILE-NAME.
If the file does not exist, ask if it should be created.
If file is being visited, the message is appended to the Emacs
buffer visiting that file.

A prefix argument N says to output N consecutive messages
starting with the current one.  Deleted messages are skipped and don't count."
  (interactive
   (progn (require 'rmailout)
	  (list (mdl-rmail-output-read-rmail-file-name)
		(prefix-numeric-value current-prefix-arg))))
  (rmail-summary-output-to-rmail-file file-name n))

; 
; Same as rmail-output-read-rmail-file-name, but matches against the headers
; produced by (mdl-get-alist-headers) instead of matching against the actual 
; message's headers.
; 
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

; 
; routines for extracting headers from current message:
; 

(defun mdl-get-headers ()
  "return first 100 headers or so of current buffer as a list of strings"
  (save-excursion
    (goto-char (point-min))
    (forward-line 100)
    (let ((header-text (buffer-substring (point-min) (point))))
      ; the \< here skips splitting before indented lines:
      (split-string header-text "[\n]+\\<"))
  )
)

(defun mdl-starts-with (string target)
  "does string begin with target?"
  (string-equal target (substring string 0 (min (length target)
						(length string)))))

(defun mdl-get-header (headers field-name)
  (let ((answer ""))
    (while headers
      (if (mdl-starts-with (car headers) field-name)
	  (progn
	    (setq answer (car headers))
	    (setq headers nil))
	(setq headers (cdr headers))))
    answer))

; 
; routines for extracting email addresses from headers:
; 

(defun mdl-get-address (text)
  "Return first email address in string text or nil if none found"
  (if (string-match "[-a-zA-Z0-9_\.+=]+@[-a-zA-Z0-9_\.+=]+" text)
      (match-string 0 text)
    nil)
  )

(defun mdl-get-sender (headers)
  (mdl-get-address (mdl-get-header headers "From:")))

  ; returns first recipient if any...
(defun mdl-get-recipient (headers)
  (mdl-get-address (mdl-get-header headers "To:")))

  ; returns first CC if any...
(defun mdl-get-cc (headers)
  (mdl-get-address (mdl-get-header headers "CC:")))

; 
; 
; 

(defun mdl-not-me (email-address)
  "Return email-address unchanged unless it is my work email address,
in which case return nil."
  (if (null email-address)
      email-address
    (if (string-equal (downcase email-address) "mark.lillibridge@hp.com")
	  nil
	email-address)))

    
(defun mdl-get-key-address (headers)
  (or
   (mdl-not-me (mdl-get-sender headers))
   (mdl-not-me (mdl-get-recipient headers))
   (mdl-get-cc headers)))

(defun mdl-get-alist-headers ()
  (let* ((headers (mdl-get-headers))
	 (from (or (mdl-get-key-address headers)        "unknown"))
	 (to   (or (mdl-get-recipient headers)          "unknown"))
	 (sub  (or (mdl-get-header headers "Subject:")  "Subject: unknown")))
    (concat "From: <" from ">\nTo: <" to ">\n" sub "\n")))



;; 
;; Experiments:
;; 

(defun mdl-rmail-output-by-email-address (address)
  "\
Append the current message to the Rmail file where emails from
the given email address are outputed to by default.  If the file
does not exist, ask if it should be created.  If file is being
visited, the message is appended to the Emacs buffer visiting
that file.  If the file exists and is not an Rmail file, the
message is appended in inbox format, the same way `rmail-output'
does it.
"
  (interactive "sEnter email address: ")
  (let (answer
	(headers (concat "From: <" address ">\n"))
	(tail rmail-output-file-alist))
    (while (and tail (not answer))
      (if (string-match (car (car tail)) headers)
	  (setq answer (eval (cdr (car tail)))))
      (setq tail (cdr tail)))
    (rmail-output-to-rmail-file answer 1)))
