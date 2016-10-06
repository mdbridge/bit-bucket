(defun mdl-become-events (events)
  (setq unread-command-events (append events unread-command-events)))

(defun mdl-pushback (event)
  (setq unread-command-events (cons event 
				    (cons last-input-event 
					  unread-command-events))))

(defun mdl-curry (function-name value)
  `(lambda () (interactive) (,function-name ,value)))

(defun mdl-make-sparse-keymap-with-default (default)
  (cons 'keymap
	(cons (cons t default) nil)))

(defun mdl-make-pushback-map (event)
  (mdl-make-sparse-keymap-with-default (mdl-curry 'mdl-pushback event)))


(keyboard-translate ?@ 'f9)

(global-set-key [f9] (mdl-make-pushback-map ?@))
(global-set-key [f9 f9] (mdl-curry 'mdl-become-events ''(?@ ?@)))
(global-set-key [f9 ?!] (mdl-make-sparse-keymap-with-default 'undefined))

(define-key isearch-mode-map [f9] (mdl-make-pushback-map ?@))
(define-key isearch-mode-map [f9 f9] (mdl-curry 'mdl-become-events ''(?@ ?@)))
(define-key isearch-mode-map [f9 ?!] 
  (mdl-make-sparse-keymap-with-default 'undefined))


(global-set-key [f9 ?! ?+] 'isearch-forward)
(global-set-key [f9 ?! ?-] 'isearch-backward)

(define-key isearch-mode-map [f9 ?! ?+] 'isearch-repeat-forward)
(define-key isearch-mode-map [f9 ?! ?-] 'isearch-repeat-backward)

(define-key minibuffer-local-isearch-map [f9 ?! ?+] "")
(define-key minibuffer-local-isearch-map [f9 ?! ?-] "")


(global-set-key [f9 ?! ?g] 'goto-line)
(global-set-key [f9 ?! ?m] "mdl")


      (setq nlinum-format-function
	    (lambda (line width)
	      (let ((str (format "%7d" line)))
		(put-text-property 0 width 'face 'linum str)
		(mdl-prop str))))

(defun mdl-goto-onscreen-line-mod100 (n)
  "\
Attempt to go to the nearest on-screen line to point such that
its line number is equal to N mod 100.  N should be between 0 and
99 inclusive.  Leaves a mark where we jumped from.  Also sets
register '*' to where we jumped from.  Ties are broken in an
unspecified manner.

If no such line exists, goes instead to the nearest line to point
such that its line number is equal to N mod 100, considering
potential lines before the buffer's first line (with negative
line numbers) and after the buffer's last line as possible
targets for this purpose."
  (interactive "NLast two digits of line number: ")
  (message "to: %d" n)
  (if (or (< n 0) (> n 99))
      (error "Expected argument between 0 and 99 inclusive."))
  (push-mark)
  (point-to-register ?*)  ; save starting point for fetch...  <<<>>>
  (let* ((current-line (mdl-current-line-number))
	 (option-a     (+ (- current-line (mod current-line 100)) n))
	 (option-b     (if (< option-a current-line)
			   (+ option-a 100)
			 (- option-a 100)))
	 ; here option-a = option-b = n (mod 100),
	 ; current-line between option-a and option-b inclusive
	 (a-closest    (< (abs (- option-a current-line))
		          (abs (- option-b current-line))))
	 (closest      (if a-closest option-a option-b))
	 (farthest     (if a-closest option-b option-a)))
	 (message "%s %s %s" current-line closest farthest))


  (let* ((current-line (mdl-current-line-number))
	 (option-a     (+ (- current-line (mod current-line 100)) n))
	 (option-b     (if (< option-a current-line)
			   (+ option-a 100)
			 (- option-a 100)))
	 ; here option-a = option-b = n (mod 100),
	 ; current-line between option-a and option-b inclusive
	 (a-closest    (< (abs (- option-a current-line))
		          (abs (- option-b current-line))))
	 (closest      (if a-closest option-a option-b))
	 (farthest     (if a-closest option-b option-a)))
    (or (mdl-goto-line closest)
	(mdl-goto-line farthest)
	(with-no-warnings (goto-line closest)))))


(defun mdl-goto-line (n)
  "\
Like goto-line but also returns true iff desired line exists and is
visible."
  (with-no-warnings (goto-line n))
  (message "%s %s"  n
    (and (< 0 n)
       (pos-visible-in-window-p)
       (or (/= (line-end-position) (point-max)) ; attempt to avoid count-lines
	   (= n (mdl-current-line-number)))))    
  (message "%s %s"  
       (pos-visible-in-window-p)
       (or (/= (line-end-position) (point-max)) ; attempt to avoid count-lines
	   (= n (mdl-current-line-number))))

  (and (< 0 n)
       (pos-visible-in-window-p)
       (or (/= (line-end-position) (point-max)) ; attempt to avoid count-lines
	   (= n (mdl-current-line-number)))))    


(global-set-key [f8] 'mdl-noop)



(load "~/elisp/nlinum")

(defun nlinum--face-height (face)
  (aref (font-info (face-font face)) 2))

(defun nlinum--face-width (face)        ;New info only in Emacs>=25.
  (let ((fi (font-info (face-font face))))
    (when (> (length fi) 11)
      (let ((width (aref fi 11)))
        (if (<= width 0)
            (aref fi 10)
          width)))))

(setq nlinum--width 7)

    (let ((width (if (display-graphic-p)
                     (ceiling
                      (let ((width (nlinum--face-width 'linum)))
                        (if width
                            (/ (* nlinum--width 1.0 width)
                               (frame-char-width))
                          (/ (* nlinum--width 1.0
                                (nlinum--face-height 'linum))
                             (frame-char-height)))))
                   nlinum--width)))
	width)



(defadvice tabulated-list-print (after mdl-buffer-menu-advice)
  (message "%s" major-mode)
  (if (eq major-mode 'Buffer-menu-mode)
      (buffer-menu-custom-font-lock)))

(ad-activate 'tabulated-list-print)



(defadvice tabulated-list-print (after mdl-buffer-menu-advice)
  (if (eq major-mode 'Buffer-menu-mode)
      (buffer-menu-custom-font-lock)))

(ad-activate 'tabulated-list-print)

(setq switch-to-buffer-preserve-window-point 'already-displayed)
