(defun tiger-hook ()
  ; remove "tiger", the abbrev trigger:
  (delete-backward-char 5)
  ; match symbol body:
  (let ((my-end (point)))
    (re-search-backward 
     "\\(\\`\\|[^[:word:][:blank:]]\\)[[:blank:]]*\\([[:word:][:blank:]]*\\)\\="
     )
    (goto-char my-end))
  (message "<%s>" (match-string 2))
  (let* ((symbol-body (match-string 2))
	 (total-length (+ 5 (length symbol-body)))
	 (body-capitalized (capitalize symbol-body))
	 (symbol (replace-regexp-in-string "[[:blank:]]" ""
					   body-capitalized t t))
	 (blank-count (- total-length (length symbol)))
	 i
	 )
    (replace-match symbol t t nil 2)
    (dotimes (i blank-count nil)
      (insert "​"))  ; zero width space (unicode #0x200b)
  ))
(define-abbrev global-abbrev-table "tiger" 42 'tiger-hook)


