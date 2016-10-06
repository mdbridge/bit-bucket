(defun undo-marker ()
  (interactive)
  (let ((count 0))
    (catch 'exit 
      (dolist (record buffer-undo-list)
	(if (null record)
	    (setq count (1+ count)))
	(if (and (consp record)
		 (markerp (car record)))
	    (throw 'exit nil))
	)
      )
    (message "undo-ing %d times... %s" count buffer-undo-list)
    ;(undo count)
    ))

(global-set-key "\C-cu" 'undo-marker)







(setq correction-marker 0)


(defun mdl-forward-char-mark (&optional arg)
  "Ensure mark is active; move point right ARG characters (left if ARG negative).
On reaching end of buffer, stop and signal error."
  (interactive "p")
  (or mark-active (setq buffer-undo-list (cons (cons correction-marker -1) 
					       buffer-undo-list)))
  (or mark-active (set-mark-command nil))
  (forward-char arg))



(global-set-key [S-right]   'mdl-forward-char-mark)


======================


(defun mdl-forward-char-mark (&optional arg)
  "Ensure mark is active; move point right ARG characters (left if ARG negative).
On reaching end of buffer, stop and signal error."
  (interactive "p")
  (or mark-active (setq buffer-undo-list (cons (cons "" (point))
					       buffer-undo-list)))
  (or mark-active (set-mark-command nil))
  (forward-char arg))

(global-set-key [S-right]   'mdl-forward-char-mark)


(defun undo-marker ()
  (interactive)
  (let ((count 0))
    (catch 'exit 
      (dolist (record buffer-undo-list)
	(if (null record)
	    (setq count (1+ count)))
	(if (and (consp record)
		 (equal (car record) ""))
	    (throw 'exit nil))
	)
      )
    (message "undo-ing %d times... %s" count buffer-undo-list)
    ;(undo count)
    ))

(global-set-key "\C-cu" 'undo-marker)
