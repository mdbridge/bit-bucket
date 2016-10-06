;;; 
;;; How to send and receive email on the local machine:
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
  (require 'smtpmail)
)


;; 
;; Who I am:
;; 

(setq user-full-name    "Mark Lillibridge")
(setq user-mail-address "mdl@alum.mit.edu")


;;
;; Send email as mdl@alum.mit.edu via MIT SMTP server:
;;

(setq send-mail-function       'smtpmail-send-it)

(require 'starttls)

(defvar smtpmail-stream-type)
(defvar smtpmail-auth-credentials)
(defvar smtpmail-starttls-credentials)
(setq
 smtpmail-smtp-server          "outgoing-alum.mit.edu"
 smtpmail-smtp-service         587
 smtpmail-stream-type          'starttls

 ; no longer used in Emacs 24 and later; contents of ~/.authinfo used instead:
 smtpmail-starttls-credentials '(("outgoing-alum.mit.edu" 587 nil nil))
 ; no longer used in Emacs 24 and later; contents of ~/.authinfo used instead:
 smtpmail-auth-credentials     (expand-file-name "~/.authinfo")

 ;smtpmail-debug-info           t
 ;smtpmail-debug-verb           t
)

(load-library "smtpmail")


;; 
;; Where to pick up email:
;; 

(setq rmail-primary-inbox-list '("/var/mail/mdl" "~/mail/incoming"))
