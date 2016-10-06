;;; 
;;; Experimental implementation of undo correction
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

(defun mdl-start-correction ()
  "Called at the start of corrections, never in the middle or end."
  ; insert fake undo record into undo list:
  (setq buffer-undo-list (cons (cons "" (point))
			       buffer-undo-list)))

(defun forward-char-mark (&optional arg)
  "Ensure mark is active; move point right ARG characters (left if ARG negative).
On reaching end of buffer, stop and signal error.

[MDL] Calls mdl-start-correction if sets mark."
  (interactive "p")
  (if (not mark-active)
      (progn
       (mdl-start-correction)
       (set-mark-command nil)))
  (forward-char arg))

(defun undo-correction ()
  "Attempt to undo until start of last correction."
  (interactive)
  (deactivate-mark)
  (let ((count 0))
    (catch 'exit 
      (dolist (record buffer-undo-list)
	(if (null record)
	    (setq count (1+ count)))
	(if (and (consp record)
		 (equal (car record) ""))
	    (throw 'exit nil))
	(if (and (consp record)
		 (equal (car record) t))
	    (error "correction not found"))
	)
      )
    (undo count)
    (message "undo-ing %d times... %s" count buffer-undo-list)
    ))

(global-set-key "\C-cu" 'undo-correction)


;(defun handle-shift-selection ()
;  "Activate/deactivate mark depending on invocation thru shift translation.
;This function is called by `call-interactively' when a command
;with a `^' character in its `interactive' spec is invoked, before
;running the command itself.
;
;If `shift-select-mode' is enabled and the command was invoked
;through shift translation, set the mark and activate the region
;temporarily, unless it was already set in this way.  See
;`this-command-keys-shift-translated' for the meaning of shift
;translation.
;
;Otherwise, if the region has been activated temporarily,
;deactivate it, and restore the variable `transient-mark-mode' to
;its earlier value."
;  (cond ((and shift-select-mode this-command-keys-shift-translated)
;         (unless (and mark-active
;                      (eq (car-safe transient-mark-mode) 'only))
;           (setq transient-mark-mode
;                 (cons 'only
;                       (unless (eq transient-mark-mode 'lambda)
;                         transient-mark-mode)))
;           (push-mark nil nil t)))
;        ((eq (car-safe transient-mark-mode) 'only)
;         (setq transient-mark-mode (cdr transient-mark-mode))
;         (deactivate-mark))))
