;; 
;; Code for scratch abort:
;; 

(defun mdl-update-modified-flag ()
  "Update the buffer modified flag."
  (interactive)
  (let* ((buffer (current-buffer))
         (basefile
          (or (buffer-file-name buffer)
              (error "Buffer %s has no associated file" buffer)))
         (tempfile (make-temp-file "buffer-content-")))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (write-region (point-min) (point-max) tempfile nil 'silent)))
    (if (= (call-process "diff" nil nil nil basefile tempfile) 0)
        (progn
          (set-buffer-modified-p nil)
          (message "Buffer matches file"))
      (message "Buffer doesn't match file"))
    (delete-file tempfile)))


