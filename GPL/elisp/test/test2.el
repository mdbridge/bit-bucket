(setq ace-jump-mode-move-keys
      (nconc (loop for i from ?0 to ?9 collect i)
	     (loop for i from ?a to ?z collect i)
	     (loop for i from ?A to ?Z collect i)))


(if (< x y)
    (setq x y)
  (progn
    (let (z)
      (setq z x))))


(dolist (key (split-string "" "" t))
  (message "" key
	   (lookup-key Rmail-mode-map key)))

(dolist (s (split-string "abc" "" t)) (message "%s" s))



(let ((map rmail-mode-map))
  (dolist (key (split-string "abcdefghijklmnopqrstuvwxyz" "" t)) 
    (let ((old-value (lookup-key map key)))
    (when old-value
      (define-key map (upcase key) old-value))))
)


(let ((map rmail-summary-mode-map))
  (dolist (key (split-string "abcdefghijklmnopqrstuvwxyz" "" t)) 
    (let ((old-value (lookup-key map key)))
    (when old-value
      (define-key map (upcase key) old-value))))
)

(let ((map rmail-summary-mode-map))
  (dolist (key (split-string "abcdefghijklmnopqrstuvwxyz" "" t)) 
    (let ((old-value (lookup-key map key)))
      (when old-value
	(define-key map (upcase key) old-value))
      (define-key map key 'mdl-edit-message)))
)

(let ((map Rmail-summary-mode-map))
  (dolist (key (split-string "" ""t))
    (let ((old-value (lookup-keymap key)))
      (when old-value
	(define-key map (upcase key) old-value))
      (define-key map key'mdl-edit-message))))
