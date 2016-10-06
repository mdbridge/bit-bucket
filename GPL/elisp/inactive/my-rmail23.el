;;;
;;; Customize Rmail 23:
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

(eval-when-compile 
  (require 'rmail)
  (require 'rmailsum)
)


;; 
;; Who I am:
;; 

(setq
 rmail-user-mail-address-regexp    
   "mark.lillibridge@hp.com\\|mdl@alum.mit.edu\\|mdl@ts-rhel.\\.hpl\\.hp\\.com"

 ; this filters out CC'd names, not the To field unless it has >1 name:
 rmail-dont-reply-to-names
	    (concat (if rmail-default-dont-reply-to-names
			(concat rmail-default-dont-reply-to-names "\\|")
		        "")
		    "mark.lillibridge\\|"
		    (concat (regexp-quote (user-login-name)) ; <<<>>>
			    "\\>"))
)


;; 
;; Where to put email:
;; 

(setq
 rmail-file-name (expand-file-name "~/Rmail/RMAIL")
 rmail-secondary-file-directory    "~/Rmail/"
 rmail-secondary-file-regexp       ".*"
)


;; 
;; Misc. Rmail mode options:
;; 

(setq
 rmail-confirm-expunge       nil
 rmail-delete-after-output   t
)


;; 
;; Use word wrap when displaying messages:
;; 

(add-hook 'rmail-mode-hook
  (function (lambda()
	      (toggle-word-wrap 1)
	      (setq fringe-indicator-alist (cons '(continuation nil nil) 
						 fringe-indicator-alist))
	      )))


;;
;; Set larger list of headers to suppress normally in rmail (unless toggle):
;;

; copied from rmail.el of emacs 23.3.1:
(defvar original-rmail-ignored-headers
  (concat "^via:\\|^mail-from:\\|^origin:\\|^references:\\|^sender:"
	  "\\|^status:\\|^received:\\|^x400-originator:\\|^x400-recipients:"
	  "\\|^x400-received:\\|^x400-mts-identifier:\\|^x400-content-type:"
	  "\\|^\\(resent-\\|\\)message-id:\\|^summary-line:\\|^resent-date:"
	  "\\|^nntp-posting-host:\\|^path:\\|^x-char.*:\\|^x-face:\\|^face:"
	  "\\|^x-mailer:\\|^delivered-to:\\|^lines:"
	  "\\|^content-transfer-encoding:\\|^x-coding-system:"
	  "\\|^return-path:\\|^errors-to:\\|^return-receipt-to:"
	  "\\|^precedence:\\|^mime-version:"
	  "\\|^list-owner:\\|^list-help:\\|^list-post:\\|^list-subscribe:"
	  "\\|^list-id:\\|^list-unsubscribe:\\|^list-archive:"
	  "\\|^content-length:\\|^nntp-posting-date:\\|^user-agent"
	  "\\|^importance:\\|^envelope-to:\\|^delivery-date\\|^openpgp:"
	  "\\|^mbox-line:\\|^cancel-lock:"
	  "\\|^DomainKey-Signature:\\|^dkim-signature:"
	  "\\|^resent-face:\\|^resent-x.*:\\|^resent-organization:"
	  "\\|^resent-openpgp:"
	  "\\|^x-.*:"))

(defconst headers-to-ignore '(
			  ; more specific origin info:
			  "resent-from"
			  "resent-sender"
			  "accept-?language"
			  "organization"

			  ; more specific recipients info:
			  "disclose-recipients"
			  "alternate-recipients?"

			  ; information about source mailer:
			  "disposition-notification-to"

			  ; signature information:
			  "comment: domainkeys.*"

			  ; routing:
			  "[a-z-]*message-id"
			  "old-received"
			  "priority"

			  ; content information:
			  "content-identifier"
			  "content-id"
			  "content-return"
			  "content-type"
			  "conversion"
			  "conversion-with-loss"
			  "encoding"
			  "content-class"
			  "content-?language"
			  "content-disposition"
			  
			  ; spam information:
			  "authentication-results"
			  "Received-SPF"

			  ; rmail information:
			  "lines"
			    ; I added this one during Babyl conversion:
			  "xx-coding-system"  

			  ; mailing list information:
			  "mailing-list"
			  "thread-index"

			  ; threading:
			  "references"
			  "thread-topic"
			  ))

(setq rmail-ignored-headers
      (concat original-rmail-ignored-headers "\\|"
	      (mapconcat (function (lambda (x) (concat "^" x ":"))) 
			 headers-to-ignore "\\|")))


;; 
;; Automatically suggest correct folder for most email messages:
;; 

; set rmail-output-file-alist to output of generate_alist.rb:
(load "~/elisp/alist-data")


;(defun mdl-file-that ()
;  "Attempt to file current email message automatically; complain if don't know
;where to file"
;  (interactive)
;  (setq rmail-default-rmail-file "unknown")
;  (mdl-rmail-output-read-rmail-file-name))




;; 
;; 
;; 

;; code for export buffer command:

(defun mdl-export-mime-to-l1 ()
  "Write raw contents of current Rmail message to the file ~/Tmp/l1."
  (interactive)

  (if rmail-buffer
      (set-buffer rmail-buffer)
    (error "There is no Rmail buffer"))
  
  (let ((beg (rmail-msgbeg rmail-current-message))
	(end (rmail-msgend rmail-current-message))
	cur)
    ;; All access to the buffer's local variables is now finished...
    (save-excursion
      ;; ... so it is ok to go to a different buffer.
      (if (rmail-buffers-swapped-p) (set-buffer rmail-view-buffer))
      (setq cur (current-buffer))
      (save-restriction
	(widen)
	(write-region beg end "~/Tmp/l1" nil nil nil nil)
	)))
)


;; 
;; Require shift be used with Rmail [summary] letter commands:
;; 

(defun mdl-capture-misrecognition (misrecognition)
  (interactive "sDragon misrecognition: ")
  nil)

(defun mdl-require-shift (keymap)
  (when (not (lookup-key keymap "A"))  ; ensure idempotent
    (dolist (key (split-string "abcdefghijklmnopqrstuvwxyz" "" t)) 
      (let ((old-value (lookup-key keymap key)))
	(when old-value
	  (define-key keymap (upcase key) old-value))
	(define-key keymap key 'mdl-capture-misrecognition)))
    ))

; these are done in next patch to ensure no keymap edit conflicts:
;(mdl-require-shift rmail-mode-map)
;(mdl-require-shift rmail-summary-mode-map)

; note: we overwrite the Q summary command (wipe summary); I don't use it
; also break rmail-resent menu item (hardcoded to ^uf)



;; 
;; Patches/workarounds for Rmail 23.3 bugs:
;; 
;; Warning: rmail-mode-hook is run once per Rmail buffer...
;; 


; disable editing functionality due to bug #9840:

(defun mdl-edit-message ()
  (interactive)
  (error "Rmail 23.3 edit message command is badly broken"))

(add-hook 'rmail-mode-hook
  (function (lambda()
	      (define-key rmail-mode-map         "e"
		'mdl-edit-message)
	      (mdl-require-shift rmail-mode-map)	      
	      )))

(add-hook 'rmail-summary-mode-hook
  (function (lambda()
	      (define-key rmail-summary-mode-map "e"      
		'mdl-edit-message)
	      (mdl-require-shift rmail-summary-mode-map)
	      )))

;;

(add-hook 'rmail-mode-hook
  (function (lambda()


; my patch for bug #9831:

(require 'rmailsum)
(defun rmail-summary (&rest no-display)
  "Display a summary of all messages, one line per message.

If no-display is set, updates/creates the summary buffer, but does not
necessarily display it."
  (interactive)
  (rmail-new-summary "All" '(rmail-summary t) nil)
  (unless (or no-display (get-buffer-window rmail-buffer))
    (rmail-summary-beginning-of-message)))


; my patch for bug # 9974:

(defun rmail-get-new-mail (&optional file-name)
  "Move any new mail from this Rmail file's inbox files.
The buffer-local variable `rmail-inbox-list' specifies the list
of inbox files.  By default, this is nil, except for your primary
Rmail file `rmail-file-name'.  In this case, when you first visit
the Rmail file it is initialized using either
`rmail-primary-inbox-list', or the \"MAIL\" environment variable,
or the function `user-login-name' and the directory
`rmail-spool-directory' (whose value depends on the operating system).

The command `set-rmail-inbox-list' sets `rmail-inbox-list' to the
value you specify.

You can also specify the file to get new mail from just for one
instance of this command.  In this case, the file of new mail is
not changed or deleted.  Noninteractively, you can pass the inbox
file name as an argument.  Interactively, a prefix argument
causes us to read a file name and use that file as the inbox.

If the variable `rmail-preserve-inbox' is non-nil, new mail will
always be left in inbox files rather than deleted.

Before doing anything, this runs `rmail-before-get-new-mail-hook'.
Just before returning, it runs `rmail-after-get-new-mail-hook',
whether or not there is new mail.

If there is new mail, it runs `rmail-get-new-mail-hook', saves
the updated file, and shows the first unseen message (which might
not be a new one).  It returns non-nil if it got any new messages."
  (interactive
   (list (if current-prefix-arg
	     (read-file-name "Get new mail from file: "))))
  (run-hooks 'rmail-before-get-new-mail-hook)
  ;; If the disk file has been changed from under us,
  ;; revert to it before we get new mail.
  (or (verify-visited-file-modtime (current-buffer))
      (find-file (buffer-file-name)))
  (set-buffer rmail-buffer)
  (rmail-modify-format)
  (rmail-swap-buffers-maybe)
  (rmail-maybe-set-message-counters)
  (widen)
  ;; Get rid of all undo records for this buffer.
  (or (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
  (let ((all-files (if file-name (list file-name) rmail-inbox-list))
	(rmail-enable-multibyte (default-value 'enable-multibyte-characters))
	found)
    (unwind-protect
	(progn
	  ;; This loops if any members of the inbox list have the same
	  ;; basename (see "name conflict" below).
	  (while all-files
	    (let ((opoint (point))
		  ;; If buffer has not changed yet, and has not been
		  ;; saved yet, don't replace the old backup file now.
		  (make-backup-files (and make-backup-files
					  (buffer-modified-p)))
		  (buffer-read-only nil)
		  ;; Don't make undo records while getting mail.
		  (buffer-undo-list t)
		  delete-files success files file-last-names)
	      ;; Pull files off all-files onto files as long as there is
	      ;; no name conflict.  A conflict happens when two inbox
	      ;; file names have the same last component.
	      ;; The reason this careful handling is necessary seems
	      ;; to be that rmail-insert-inbox-text uses .newmail-BASENAME.
	      (while (and all-files
			  (not (member (file-name-nondirectory (car all-files))
				file-last-names)))
		(setq files (cons (car all-files) files)
		      file-last-names
		      (cons (file-name-nondirectory (car all-files)) files))
		(setq all-files (cdr all-files)))
	      ;; Put them back in their original order.
	      (setq files (nreverse files))
	      (goto-char (point-max))
	      ;; Make sure we are in valid mbox format (end with a
	      ;; blank line unless no messages):
	      ;;   (fixes damage caused by bug #????)
	      (unless (bobp)
		(while (not (looking-back "\n\n"))
		  (insert "\n")))
	      (setq found (or
			   (rmail-get-new-mail-1 file-name files delete-files)
			   found))))
	  ;; Move to the first new message unless we have other unseen
	  ;; messages before it.
	  (if found (rmail-show-message (rmail-first-unseen-message)))
	  (run-hooks 'rmail-after-get-new-mail-hook)
	  found)
      ;; Don't leave the buffer screwed up if we get a disk-full error.
      (rmail-show-message))))

(defun rmail-insert-inbox-text (files renamep)
  ;; Detect a locked file now, so that we avoid moving mail
  ;; out of the real inbox file.  (That could scare people.)
  (or (memq (file-locked-p buffer-file-name) '(nil t))
      (error "RMAIL file %s is locked"
	     (file-name-nondirectory buffer-file-name)))
  (let (file tofile delete-files movemail popmail got-password password)
    (while files
      ;; Handle remote mailbox names specially; don't expand as filenames
      ;; in case the userid contains a directory separator.
      (setq file (car files))
      (let ((url-data (rmail-parse-url file)))
	(setq file (nth 0 url-data))
	(setq popmail (nth 1 url-data))
	(setq password (nth 2 url-data))
	(setq got-password (nth 3 url-data)))

      (if popmail
	  (setq renamep t)
	(setq file (file-truename
		    (substitute-in-file-name (expand-file-name file)))))
      (setq tofile (expand-file-name
		    ;; Generate name to move to from inbox name,
		    ;; in case of multiple inboxes that need moving.
		    (concat ".newmail-"
			    (file-name-nondirectory
			     (if (memq system-type '(windows-nt cygwin ms-dos))
				 ;; cannot have colons in file name
				 (replace-regexp-in-string ":" "-" file)
			       file)))
		    ;; Use the directory of this rmail file
		    ;; because it's a nuisance to use the homedir
		    ;; if that is on a full disk and this rmail
		    ;; file isn't.
		    (file-name-directory
		     (expand-file-name buffer-file-name))))
      ;; Always use movemail to rename the file,
      ;; since there can be mailboxes in various directories.
      (when (not popmail)
	;; On some systems, /usr/spool/mail/foo is a directory
	;; and the actual inbox is /usr/spool/mail/foo/foo.
	(if (file-directory-p file)
	    (setq file (expand-file-name (user-login-name)
					 file))))
      (cond (popmail
	     (message "Getting mail from the remote server ..."))
	    ((and (file-exists-p tofile)
		  (/= 0 (nth 7 (file-attributes tofile))))
	     (message "Getting mail from %s..." tofile))
	    ((and (file-exists-p file)
		  (/= 0 (nth 7 (file-attributes file))))
	     (message "Getting mail from %s..." file)))
      ;; Set TOFILE if have not already done so, and
      ;; rename or copy the file FILE to TOFILE if and as appropriate.
      (cond ((not renamep)
	     (setq tofile file))
	    ((or (file-exists-p tofile) (and (not popmail)
					     (not (file-exists-p file))))
	     nil)
	    (t
	     (with-temp-buffer
	       (let ((errors (current-buffer)))
		 (buffer-disable-undo errors)
		 (let ((args
			(append
			 (list (or rmail-movemail-program "movemail") nil errors nil)
			 (if rmail-preserve-inbox
			     (list "-p")
			   nil)
			 (if (rmail-movemail-variant-p 'mailutils)
			     (append (list "--emacs") rmail-movemail-flags)
			   rmail-movemail-flags)
			 (list file tofile)
			 (if password (list password) nil))))
		   (apply 'call-process args))
		 (if (not (buffer-modified-p errors))
		     ;; No output => movemail won
		     nil
		   (set-buffer errors)
		   (subst-char-in-region (point-min) (point-max)
					 ?\n ?\s)
		   (goto-char (point-max))
		   (skip-chars-backward " \t")
		   (delete-region (point) (point-max))
		   (goto-char (point-min))
		   (if (looking-at "movemail: ")
		       (delete-region (point-min) (match-end 0)))
		   (beep t)
		   ;; If we just read the password, most likely it is
		   ;; wrong.  Otherwise, see if there is a specific
		   ;; reason to think that the problem is a wrong passwd.
		   (if (or got-password
			   (re-search-forward rmail-remote-password-error
					      nil t))
		       (rmail-set-remote-password nil))

		   ;; If using Mailutils, remove initial error code
		   ;; abbreviation
		   (when (rmail-movemail-variant-p 'mailutils)
		     (goto-char (point-min))
		     (when (looking-at "[A-Z][A-Z0-9_]*:")
		       (delete-region (point-min) (match-end 0))))

		   (message "movemail: %s"
			    (buffer-substring (point-min)
					      (point-max)))

		   (sit-for 3)
		   nil)))))

      ;; At this point, TOFILE contains the name to read:
      ;; Either the alternate name (if we renamed)
      ;; or the actual inbox (if not renaming).
      (if (file-exists-p tofile)
	  (let ((coding-system-for-read 'no-conversion)
		size)
	    (goto-char (point-max))
	    (setq size
		  ;; If new mail is in Babyl format, convert it to mbox.
		  (rmail-unrmail-new-mail-maybe
		   tofile
		   (nth 1 (insert-file-contents tofile))))
	    (goto-char (point-max))
	    ;; Paranoia: make sure read in mbox format data properly
	    ;; ends with a blank line unless it is of size 0:
	    (unless (zerop size)
	      (while (not (looking-back "\n\n"))
		(insert "\n")))
	    (if (not (and rmail-preserve-inbox (string= file tofile)))
		(setq delete-files (cons tofile delete-files)))))
      (message "")
      (setq files (cdr files)))
    delete-files))


; my patch for bug #9978:

(defun rmail-output-to-rmail-buffer (tembuf msg)
  "Copy message in TEMBUF into the current Rmail buffer.
Do what is necessary to make Rmail know about the new message. then
display message number MSG."
  (save-excursion
    (rmail-swap-buffers-maybe)
    (rmail-modify-format)
    ;; Turn on Auto Save mode, if it's off in this buffer but enabled
    ;; by default.
    (and (not buffer-auto-save-file-name)
	 auto-save-default
	 (auto-save-mode t))
    (rmail-maybe-set-message-counters)
    ;; Insert the new message after the last old message.
    (widen)
    (goto-char (point-max))
    ;; Make sure current Rmail buffer is in valid mbox format (end
    ;; with a blank line unless no messages):
    ;;   (fixes damage caused by bug #9974)
    (unless (bobp)
      (while (not (looking-back "\n\n"))
	(insert "\n")))
    ;; Insert the new message at the end.
    (narrow-to-region (point-max) (point-max))
    (insert-buffer-substring tembuf)
    (rmail-count-new-messages t)
    ;; FIXME should re-use existing windows.
    (if (rmail-summary-exists)
	(rmail-select-summary (rmail-update-summary)))
    (rmail-show-message-1 msg)))


; my patch for bug #10041:

;; The DONT-SHOW argument is new in 23.  Does not seem very important.
(defun rmail-expunge (&optional dont-show)
  "Erase deleted messages from Rmail file and summary buffer.
This always shows a message (so as not to leave the Rmail buffer
unswapped), and always updates any summary (so that it remains
consistent with the Rmail buffer).  If DONT-SHOW is non-nil, it
does not pop any summary buffer."
  (interactive)
  (when (rmail-expunge-confirmed)
    (rmail-modify-format)
    (let ((was-deleted (rmail-message-deleted-p rmail-current-message))
	  (was-swapped (rmail-buffers-swapped-p)))
      (rmail-only-expunge t)
      ;; We always update the summary buffer, so that the contents
      ;; remain consistent with the rmail buffer.
      ;; The only difference is, in the dont-show case, we use a
      ;; cut-down version of rmail-select-summary that does not pop
      ;; the summary buffer.  It's only used by rmail-quit, which is
      ;; just going to bury any summary immediately after.  If we made
      ;; rmail-quit bury the summary first, dont-show could be removed.
      ;; But the expunge might not be confirmed...
      (if (rmail-summary-exists)
	  (if dont-show
	      (let ((total rmail-total-messages))
		(with-current-buffer rmail-summary-buffer
		  (let ((rmail-total-messages total))
	    ; ignore errors (twice) from rmail-update-summary as
	    ; workaround for bug #9964; FIXME: remove once that bug is
	    ; fixed:
		    (ignore-errors (rmail-update-summary)))))
	    (rmail-select-summary (ignore-errors (rmail-update-summary)))))
      ;; We always show a message, because (rmail-only-expunge t)
      ;; leaves the rmail buffer unswapped.
      ;; If we expunged the current message, a new one is current now,
      ;; so show it.  If we weren't showing a message, show it.
      (if (or was-deleted (not was-swapped))
	  (rmail-show-message-1 rmail-current-message)
	;; We can just show the same message that was being shown before.
	(rmail-display-labels)
	(rmail-swap-buffers)
	(setq rmail-buffer-swapped t)))))


; Fix for not yet reported bug:

(defun rmail-summary-output (&optional file-name n)
  "Append this message to mail file FILE-NAME.
This works with both mbox format and Babyl format files,
outputting in the appropriate format for each.
The default file name comes from `rmail-default-file',
which is updated to the name you use in this command.

A prefix argument N says to output that many consecutive messages
from those in the summary, starting with the current one.
Deleted messages are skipped and don't count.
When called from Lisp code, N may be omitted and defaults to 1.

This command always outputs the complete message header,
even the header display is currently pruned."
  (interactive
   (progn (require 'rmailout)
	  (list (with-current-buffer rmail-buffer (rmail-output-read-file-name))
		(prefix-numeric-value current-prefix-arg))))
  (let ((i 0) prev-msg)
    (while
	(and (< i n)
	     (progn (rmail-summary-goto-msg)
		    (not (eq prev-msg
			     (setq prev-msg
				   (with-current-buffer rmail-buffer
				     rmail-current-message))))))
      (setq i (1+ i))
      (with-current-buffer rmail-buffer
	(let ((rmail-delete-after-output nil))
	  (rmail-output file-name 1)))
      (if rmail-delete-after-output
	  (rmail-summary-delete-forward nil)
	(if (< i n)
	    (rmail-summary-next-msg 1))))))

;


	      )))
