;;;
;;; windows.el: rebind window keys so they act the same in emacs as in 
;;;             normal windows applications.
;;;
;;;
;;; Author: Mark Lillibridge
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

;; Version: 1


(global-set-key [home] 'beginning-of-line)
(global-set-key [end]  'end-of-line)

;
; make sure to set Reflection so backspace sends a backspace, not a delete!
;
(global-set-key [delete] 'delete-char)

;
; we do not use the following rebinding so that backspace translates to DEL
; and minor modes that rebind DEL continue to work:
;(global-set-key [backspace] 'backward-delete-char-untabify)
