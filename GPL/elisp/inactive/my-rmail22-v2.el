;;;
;;; Customize Rmail 22:
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
  (require 'sendmail)
  (load "~/elisp/my-rmail-alist")
)


;; 
;; Configuring sendmail.el message sending facility:
;; 

(setq
 ; where put mail auto-saved files in Emacs 23:
 mail-default-directory      "~/mail/drafts"

 mail-default-reply-to       user-mail-address
 mail-self-blind             t

 mail-yank-prefix            ">  "

 ; mail-default-headers "Full-Name: Mark Lillibridge\n\
)


;; 
;; Who I am:
;; 

(setq
 rmail-user-mail-address-regexp    "mark.lillibridge@hp.com\\|mdl@alum.mit.edu\\|mdl@ts-rhel.\\.hpl\\.hp\\.com"

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
;; Where to pick up email:
;; 

;(setq rmail-primary-inbox-list '("/usr/spool/mail/mdl"))
;(setq rmail-primary-inbox-list '("~/mail/incoming"))
(setq rmail-primary-inbox-list '("~/mail/incoming-never"))


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
;; Decode RFC2047-encoded headers when displaying messages:
;; 

(autoload 'rfc2047-decode-string "rfc2047")
(autoload 'rfc2047-decode-region "rfc2047")

(setq rmail-message-filter
      (lambda ()
	(save-excursion
	  (when (search-forward "\n\n" nil t)
	    (rfc2047-decode-region
	     (point-min) (match-beginning 0)))))
      rmail-summary-line-decoder (function rfc2047-decode-string))


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

; use my improved alist matching code:
(load "~/elisp/my-rmail-alist")


(defun mdl-file-that ()
  "Attempt to file current email message automatically; complain if don't know
where to file"
  (interactive)
  (setq rmail-default-rmail-file "unknown")
  (mdl-rmail-output-read-rmail-file-name))


;; 
;; Allow save file from summary buffer to usefully save associated rmail-buffer:
;; 

(defun mdl-save-rmail-buffer ()
  "Save associated rmail buffer"
  (interactive)
  (with-current-buffer rmail-buffer
    (save-buffer)))

(add-hook 'rmail-summary-mode-hook
  (function (lambda()
	      (define-key rmail-summary-mode-map (kbd "C-x C-s")
		'mdl-save-rmail-buffer)
	      )))

;; 
;; Extensions for handling MIME messages:
;; 

(load "~/elisp/etach")


;;
;; Bug fix so X-Coding-System gives actual decoding used:
;;

;; Decode the region specified by FROM and TO by CODING.
;; If CODING is nil or an invalid coding system, decode by `undecided'.
(defun rmail-decode-region (from to coding)
  (if (or (not coding) (not (coding-system-p coding)))
      (setq coding 'undecided))
  ;; Use -dos decoding, to remove ^M characters left from base64 or
  ;; rogue qp-encoded text.
  (decode-coding-region from to
			(coding-system-change-eol-conversion coding 1))
  ;; Don't reveal the fact we used -dos decoding, as users generally
  ;; will not expect the RMAIL buffer to use DOS EOL format.
  (setq buffer-file-coding-system
	(setq last-coding-system-used
	      (coding-system-change-eol-conversion last-coding-system-used 0))))
