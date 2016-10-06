;;;
;;; Test code:
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
)

(load "~/elisp/my-rmail23.el")

;; 
;; Where to pick up email:
;; 

(setq rmail-primary-inbox-list '("~/scratch/mail/incoming")) ; <<<>>>


;; 
;; Where to put email:
;; 

(setq
; rmail-file-name (expand-file-name "~/Rmail/RMAIL")
 rmail-file-name (expand-file-name "~/Rmail2/RMAIL") ; <<<>>>
; rmail-secondary-file-directory    "~/Rmail/"
 rmail-secondary-file-directory    "~/Rmail2/" ; <<<>>>
 rmail-secondary-file-regexp       ".*"
)
