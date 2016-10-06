;;; 
;;; Elisp routines useful for debugging
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

(defun mdl-lisp-output (thingtoprint)
  "Print out thingtoprint in the buffer *mdl lisp output*, surrounded
by a separator."
  (with-current-buffer (get-buffer-create "*mdl lisp output*")
    (goto-char (point-max))
    (insert "----------------------------------------------------------\n")
    (princ thingtoprint (current-buffer))
    (insert "\n----------------------------------------------------------\n")))

(defun mdl-lisp-output-buffer (bufname)
  "Print out the entire contents of bufname in the buffer *mdl lisp output*,
surrounded by a separator."
  (with-current-buffer bufname
    (mdl-lisp-output (buffer-substring 0 (point-max)))))

