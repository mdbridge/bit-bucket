(load-file "~/Dragon/utterance-mode.el")

(defun mdl-utterance-restore-point (&optional arg no-cleaning)
  "restores the previously remembered region. if no-cleaning is non-nil
no backtracking of remember-region takes place. with a prefix argument do this multiple times."
  (interactive "pnumber of times: ")
  (if arg
      (while (> arg 1)
	(setq utterance-remember-region (cdr (assoc 'previous (car utterance-remember-region))))
	(setq arg (- arg 1))))
  (let ((region-element (cdr (assoc 'region (car utterance-remember-region)))))
    (goto-char (or (nth 1 region-element) (point)))
))



