;;;
;;; Customize Rmail:
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
  (require 'rmailsum)
  (require 'sendmail)
  (require 'smtpmail)
  (load "~/elisp/my-rmail-alist")
  ;(require 'my-rmail-alist)
)


;; 
;; Who I am:
;; 

(setq user-full-name               "Mark Lillibridge")
(setq user-mail-address            "mark.lillibridge@hp.com")

(setq
 mail-default-reply-to             user-mail-address

 ; this filters out CC'd names, not the To field unless it has >1 name:
 rmail-dont-reply-to-names
	    (concat (if rmail-default-dont-reply-to-names
			(concat rmail-default-dont-reply-to-names "\\|")
		        "")
		    "mark.lillibridge\\|"
		    (concat (regexp-quote (user-login-name))
			    "\\>"))

 ; mail-default-headers "Full-Name: Mark Lillibridge\n\
)


;;
;; Send email via STMP server on pobox-pa:
;;

(setq smtpmail-default-smtp-server "smtp-pa.hpl.hp.com")
(setq smtpmail-local-domain        nil)
(setq send-mail-function           'smtpmail-send-it)

(load-library "smtpmail")


;; 
;; Where to pick up email:
;; 

;(setq rmail-primary-inbox-list '("/usr/spool/mail/mdl"))
(setq rmail-primary-inbox-list '("~/mail/incoming"))


;; 
;; Where to put email:
;; 

(setq
 rmail-file-name (expand-file-name "~/Rmail/RMAIL")
 rmail-secondary-file-directory    "~/Rmail/"
 rmail-secondary-file-regexp       ".*"
)

; where put mail auto-saved files in Emacs 23:
(setq mail-default-directory "~/mail")


;; 
;; Misc. Rmail mode options:
;; 

(setq
 mail-self-blind             t

 mail-yank-prefix            ">  "

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

(defconst headers-to-ignore '(
			  ; more specific origin info:
			  "mail-from"
			  "origin"
			  "x400-originator"
			  "return-receipt-to"
			  "resent-from"
			  "resent-sender"
			  "x-env-sender"
			  "x-originating-ip"
			  "errors-to"
			  "\\(x-\\)?accept-?language"
			  "organization"
			  "\\(x-\\)?\\(x-\\)?sender"
			  "x-source[a-z-]*"

			  ; more specific recipients info:
			  "x400-recipients"
			  "x-envelope-to"
			  "x-vms-to"
			  "x-vms-cc"
			  "disclose-recipients"
			  "alternate-recipients?"
			  "x-apparently-to"

			  ; information about source mailer:
			  "x-mailer"
			  "x-mailreader"
			  "user-agent"
			  "x-mailman[a-z-]*"
			  "x-facebook[a-z-]*"
			  "x-yahoo[a-z-]*"
			  "x-[a-z]*-version"
			  "disposition-notification-to"

			  ; signature information:
			  "domainkey-signature"
			  "dkim-signature"
			  "comment: domainkeys.*"

			  ; routing:
			  "[a-z-]*message-id"
			  "x400-mts-identifier"
			  "x-msg-ref"
			  "via"
			  "\\(x-\\)?received"
			  "x-loop"
			  "old-received"
			  "x400-received"
			  "priority"
			  "x-gateway"
			  "precedence"
			  "x?-beenthere"
			  "x-[a-z-]*priority"
			  "x-zzc-via"
			  "status"
			  "x-originalarrivaltime"

			  ; content information:
			  "content-identifier"
			  "content-return"
			  "mime-version"
			  "content-type"
			  "x400-content-type"
			  "content-length"
			  "content-transfer-encoding"
			  "conversion"
			  "conversion-with-loss"
			  "encoding"
			  "x-popmail-charset"
			  "x-coding-system"
			  "x-mimeole"
			  "x-ms-[a-z-]*"
			  "content-class"
			  "x-mime-autoconverted"
			  "\\(x-\\)?content-?language"
			  "content-disposition"
			  "x-language-detected"
			  
			  ; spam information:
			  "x-[a-z-]*spam[a-z-]*"
			  "x-[a-z-]*scan[a-z-]*"
			  "x-[a-z-]*virus[a-z-]*"
			  "x-[a-z-]*filter[a-z-]*"
			  "x-brightmail-tracker"
			  "x-auditid"
			  "authentication-results"
			  "x-provags-id"
			  "x-antiabuse"
			  "x-spam-status"

			  ; rmail information:
			  "summary-line"
			  "lines"

			  ; mailing list information:
			  "\\(x-\\)?mailing-list"
			  "list-help"
			  "list-post"
			  "list-subscribe"
			  "list-archive"
			  "list-unsubscribe"
			  "thread-index"
			  "list-id"
			  "x-egroups-.*"

			  ; threading:
			  "References"
			  ))

(setq rmail-ignored-headers
      (mapconcat (function (lambda (x) (concat "^" x ":"))) 
		 headers-to-ignore "\\|"))



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
