;;; 
;;; Customize mail
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
  (require 'message)
  (if (= emacs-major-version 24)
      (require 'mairix))
)


; How to send and receive email on the local machine:
(load "~/elisp/mail-setup")  


;; 
;; Configuring message.el message sending facility:
;; 

(setq mail-user-agent 'message-user-agent)  ; needed pre-version 23

(defvar message-yank-empty-prefix)
(setq
 message-directory                 "~/mail/"  ; => put drafts in ~/mail/drafts

 message-yank-prefix               ">  "
 message-yank-cited-prefix         ">  "
 message-yank-empty-prefix         ">  "
 
 message-max-buffers               3

 ; don't ever attempt to split messages that are too long:
 message-send-mail-partially-limit nil

 message-beginning-of-line         nil
)

(setq message-default-headers
      (concat
       (format "Reply-to: %s\n" user-mail-address)
       (format "BCC: %s\n"      user-mail-address)
       ;"Full-Name: Mark Lillibridge\n"
       ))

; make attach not ask about, description, or disposition.
(defun mdl-mml-attach-file (file)
  "Attach a file to the outgoing MIME message.
The file is not inserted or encoded until you send the message with
`\\[message-send-and-exit]' or `\\[message-send]'.

FILE is the name of the file to attach."
  (interactive (list (mml-minibuffer-read-file "Attach file: ")))
   (mml-attach-file file))
(add-hook 'message-mode-hook
  (function (lambda()
	      (define-key message-mode-map "\C-c\C-a" 'mdl-mml-attach-file)
	      )))



;; 
;; Customize Rmail:
;; 

(if (and
     (not (eq window-system 'w32))
     (= emacs-major-version 24)
     (or 
      (= emacs-minor-version 3)
      (= emacs-minor-version 5)
      ))
    (if (= emacs-minor-version 5)
	(load "~/elisp/my-rmail24.5")
      (if (= emacs-minor-version 3)
	  (load "~/elisp/my-rmail24.3")))
  (progn
    (defun rmail (&optional file-name-arg)
      (interactive)
      (error "Rmail not available here"))

    (defun rmail-input (filename)
      (interactive "FRun rmail on RMAIL file: ")
      (error "Rmail not available here"))
    ))



;; 
;; Enable mairix:
;; 

(if (and
     (not (eq window-system 'w32))
     (= emacs-major-version 24))
    (require 'mairix)) ; clobbers rmail autoload due to bug #13294

(defvar mairix-file-path)
(defvar mairix-search-file)
(setq mairix-file-path   "~/mail")
(setq mairix-search-file "mairix_output")
