;;;
;;; Customize Rmail 24.5:
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
  (require 'rmailout)
)


;; 
;; Who I am:
;; 

(setq
 rmail-user-mail-address-regexp    
   "mark.lillibridge@hpe?.com\\|mdl@alum.mit.edu\\|mdl@ts-rhel.\\.hpl\\.hpe?\\.com"

 ; this filters out CC'd names, not the To field unless it has >1 name:
 mail-dont-reply-to-names
 	    (concat "mark.lillibridge\\|"
		    ; from mail-don't-reply-to function in mail-utils.el:
		    (if (and user-mail-address
			     (not (equal user-mail-address user-login-name)))
			;; Anchor the login name and email address so that we
			;; don't match substrings: if the login name is
			;; "foo", we shouldn't match "barfoo@baz.com".
			(concat "\\`"
				(regexp-quote user-mail-address)
				"\\'\\|")
		      "")
		    (concat "\\`" (regexp-quote user-login-name) "@"))
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

; copied from rmail.el of emacs 23.3.1 (same in 24.2):
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
			  "SpamDiagnosticOutput"
			  "SpamDiagnosticMetadata"

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


; 
; Limit search to first hundred lines of message:
; 
(add-hook 'rmail-mode-hook
  (function (lambda()

(require 'rmailout)  ; prevent loading this later from overwriting the patch...

(defun rmail-output-read-file-name ()
  "Read the file name to use for `rmail-output'.
Set `rmail-default-file' to this name as well as returning it.
This uses `rmail-output-file-alist', BUT LIMITED TO FIRST 100 LINES OF
A MESSAGE."
  (let* ((default-file
	   (or
	    (when rmail-output-file-alist
	      (or rmail-buffer (error "There is no Rmail buffer"))
	      (save-current-buffer
		(set-buffer rmail-buffer)
		(let ((beg (rmail-msgbeg rmail-current-message))
		      (end (rmail-msgend rmail-current-message)))
		  (if (rmail-buffers-swapped-p) (set-buffer rmail-view-buffer))
		  (save-excursion
		    (save-restriction
		      (widen)
		      (narrow-to-region beg end)
		      ; MDL START:
		      (goto-char (point-min))
		      (forward-line 100)
		      (narrow-to-region beg (point))
		      ; MDL END:
		      (let ((tail rmail-output-file-alist)
			    answer err)
			;; Suggest a file based on a pattern match.
			(while (and tail (not answer))
			  (goto-char (point-min))
			  (if (re-search-forward (caar tail) nil t)
			      (setq answer
				    (condition-case err
					(eval (cdar tail))
				      (error
				       (display-warning
					:error
					(format "Error evaluating \
`rmail-output-file-alist' element:\nregexp: %s\naction: %s\nerror: %S\n"
						(caar tail) (cdar tail) err))
				       nil))))
			  (setq tail (cdr tail)))
			answer))))))
	    ;; If no suggestion, use same file as last time.
	    rmail-default-file))
	 (read-file
	  (expand-file-name
	   (read-file-name
	    (concat "Output message to mail file (default "
		    (file-name-nondirectory default-file)
		    "): ")
	    (file-name-directory default-file)
	    (abbreviate-file-name default-file))
	   (file-name-directory default-file))))
    (setq rmail-default-file
	  (if (file-directory-p read-file)
	      (expand-file-name (file-name-nondirectory default-file)
				read-file)
	    (expand-file-name
	     (or read-file (file-name-nondirectory default-file))
	     (file-name-directory default-file))))))

))) ; end add-hook

;(defun mdl-file-that ()
;  "Attempt to file current email message automatically; complain if don't know
;where to file"
;  (interactive)
;  (setq rmail-default-rmail-file "unknown")
;  (mdl-rmail-output-read-rmail-file-name))



;; 
;; Automatically file some email messages:
;; 

(setq rmail-automatic-folder-directives
      '(("~/Rmail/p/companies" "from" "PetSmart@email-PetSmart.com")
	("~/Rmail/p/companies" "from" "landsend@email.landsend.com")
	("~/Rmail/p/companies" "from" "email@communications.flemingssteakhouse.com")
	("~/Rmail/p/companies" "from" "vmwareteam@connect.vmware.com")
	("~/Rmail/p/Twitter"   "from" "info@twitter.com")

	("~/Rmail/h/hp"   "from" "hpepac@hpe.com")
	("~/Rmail/h/TheMachine"   "subject" "changeset in repos/cluster/config")
	("~/Rmail/h/TheMachine"   "subject" "changeset in repos/hgadmin")

	("~/Rmail/b/cs"   "subject" "\\[TYPES/announce\\].*[sS]econd [cC]all")
	("~/Rmail/b/cs"   "subject" "\\[TYPES/announce\\].*2nd \\([cC]all\\|CfP\\)")
	("~/Rmail/b/cs"   "subject" "\\[TYPES/announce\\].*[pP]ost[ -]?doc")
	("~/Rmail/b/cs"   "subject" "\\[TYPES/announce\\].*PhD")
	("~/Rmail/b/cs"   "subject" "\\[TYPES/announce\\].*tenure")
	("~/Rmail/b/cs"   "subject" "\\[TYPES/announce\\].*\\b[jJ]obs?\\b")
	("~/Rmail/b/cs"   "subject" "\\[TYPES/announce\\].*[vV]acancy")
	("~/Rmail/b/cs"   "subject" "\\[TYPES/announce\\].*\\b\\(APLAS\\|CAV\\|Synasc\\|TAPAS\\|FTSCS\\|FOSAD\\|ICPE\\|IWC\\|JTRES\\|LCC\\)[^a-zA-Z]")
	("~/Rmail/b/cs"   "subject" "\\[TYPES/announce\\].*\\b\\(LOPSTR\\|PEPM\\|PPPJ\\|SPLASH\\|LICS\\|WADT\\|WLP\\|WoLLIC\\|EXPRESS\\)[^a-zA-Z]")
	("~/Rmail/b/cs"   "subject" "\\[TYPES/announce\\].*\\b\\(WFLP\\|ITP\\|HSB\\d+\\|IWC\\|FSEN\\|NSV\\|SOAP\\|HLDVT\\|PLRR\\)[^a-zA-Z]")
	("~/Rmail/b/cs"   "subject" "\\[TYPES/announce\\].*Computational Linguistics")
	))



;; 
;; code for unpack buffer, etc. commands:
;; 

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

;  ; Warning: rmail-mode-hook is run once per Rmail buffer so
;  ; mdl-require-shift needs to be idempotent:
;(add-hook 'rmail-mode-hook
;  (function (lambda()
;	      (mdl-require-shift rmail-mode-map)	      
;	      )))
;(add-hook 'rmail-summary-mode-hook
;  (function (lambda()
;	      (mdl-require-shift rmail-summary-mode-map)
;	      )))

; note: we overwrite the Q summary command (wipe summary); I don't use it
; also break rmail-resent menu item (hardcoded to ^uf)



;; 
;; Patches/workarounds for Rmail 24.5 bugs:
;; 
;; Warning: rmail-mode-hook is run once per Rmail buffer...
;; 


; disable editing functionality due to numerous unpatched editing bugs:
; e.g., #13327

(defun mdl-edit-message ()
  (interactive)
  (error "Rmail 24.5 edit message command is broken"))

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

; unpatched: bug #10080: Rmail shows an extra blank line at the end
; when displaying a non-MIME message

	      )))
