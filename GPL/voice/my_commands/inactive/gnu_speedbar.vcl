## 
## Experiment: speedbar
## 

add speed bar = Elisp("(require 'speedbar)")
                Elisp('(speedbar-change-initial-expansion-list "buffers")')
    	        Do(speedbar) Wait(200)  Do(speedbar-get-focus);


# "other frame" can toggle between the speed bar and the normal frame


speed buffer <r> = Do(mdl-speedbar-pick) $1{enter};




====================  cut here my-modes.el ====================
;;
;; Experiment: speedbar
;;

(eval-when-compile 
  (require 'speedbar))

(setq speedbar-frame-parameters '((minibuffer          . nil)
                                       (width          . 30)          ; was 20
                                       (border-width   . 0)
                                       (menu-bar-lines . 0)
                                       (tool-bar-lines . 0)
                                       (unsplittable   . t)
                                       (left-fringe    . 0)
                                       ))


====================  cut here voice-routines.el ====================
;;
;; code for speedbar <r>:
;;

(defun mdl-speedbar-pick (n)
  "Pick using speed bar line number N."
  (interactive "Nspeed buffer line number to pick: ")
  (let* ( (properties (mdl-speedbar-pick-properties n))
	  (fn (nth 0 properties))
	  (buffer-text (nth 2 properties))
	)
    ; currently can only handle buffer mode:
    (if (eq fn 'speedbar-buffer-click)
	(switch-to-buffer buffer-text)
      (message " Unable to find any buffer information"))
  )
)


(defun mdl-speedbar-pick-properties (n)
  "Get speed bar properties for pick using speed bar line number N."
  (interactive "Nspeed buffer line number to pick: ")
  (with-current-buffer speedbar-buffer
    (mdl-goto-onscreen-line-mod100 n)
    ; below extracted from speedbar-edit-line:
    (beginning-of-line)
    (re-search-forward "[]>?}] [^ ]"
		       (save-excursion (end-of-line) (point))
		       t)
    (forward-char -1)
    (mdl-speedbar-get-properties)
  )
)

  ; modified from speedbar-do-function-pointer:
(defun mdl-speedbar-get-properties ()
  "Look under the cursor and examine the text properties.
From this extract the file/tag name, token, indentation level and 
a function."
  (let* ((fn  (get-text-property (point) 'speedbar-function))
	 (tok (get-text-property (point) 'speedbar-token))
	 ;; The 1-,+ is safe because scaning starts AFTER the point
	 ;; specified.  This lets the search include the character the
	 ;; cursor is on.
	 (tp (previous-single-property-change
	      (1+ (point)) 'speedbar-function))
	 (np (next-single-property-change
	      (point) 'speedbar-function))
	 (txt (buffer-substring-no-properties (or tp (point-min))
					      (or np (point-max))))
	 (dent (save-excursion (beginning-of-line)
			       (string-to-number
				(if (looking-at "[0-9]+")
				    (buffer-substring-no-properties
				    (match-beginning 0) (match-end 0))
				  "0")))))
    ;(message "%S:%S:%S:%s" fn tok txt dent)
    `(,fn ,tok ,txt ,dent)
  )
)
