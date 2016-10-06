;;;
;;; line-numbers.el: allow jumping to any onscreen line by its line number 
;;;                  mod 100
;;;
;;; Version: 14
;;;
;;; This version requires Gnuemacs.
;;;
;;;
;;; Authors: Mark Lillibridge, John MacCormick
;;;
;;; (c) Copyright 2016 Hewlett Packard Enterprise Development LP
;;;
;;; Copyright (C) 2007, 2008  Markus Triska  [for linum code]
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


(eval-when-compile 
  (if (< emacs-major-version 22)
      (defmacro with-no-warnings (&rest body) (cons 'progn body))))
(eval-when-compile 
  (if (>= emacs-major-version 22)
      (load "~/elisp/linum-22"))
  (if (>= emacs-major-version 24)
      (load "~/elisp/nlinum"))
  (load "~/elisp/wb-line-number-patched")
  (require 'info))


;;;
;;; Arrange for each line's line-number to be displayed to the left of that 
;;; line:
;;;

(defvar mdl-frame-extra 0)

(if (>= emacs-major-version 24)
    ;
    ; For Gnu Emacs 24-, nlinum mode is superior:
    ;
    (progn
      (load "~/elisp/nlinum")

      (setq-default nlinum--width 7)
      (setq mdl-frame-extra 7)
      (set-face-foreground 'linum "blue")

      ; ensure header line with links is reachable by keystrokes:
      (setq Info-use-header-line nil)

      ;
      ; Change to put all but last two digits of line numbers in shadow:
      ;
      (defface linum-shadow
	'((t :inherit (shadow default)))
	"Face for displaying line numbers in the display margin."
	:group 'linum)
      (defun mdl-prop (str)
	(let ((lead   (substring str 0 -2))
	      (target (substring str -2)))
	  (concat (propertize lead   'face 'linum-shadow)
		  (propertize target 'face 'linum)))
	)

      (setq nlinum-format-function
	    (lambda (line width)
	      (let ((str (format "%7d" line)))
		(when (< (length str) width)
		  ;; Left pad to try and right-align the line-numbers.
		  (setq str (concat (make-string (- width (length str)) ?\ ) str)))
		(put-text-property 0 width 'face 'linum str)
		(mdl-prop str))))

      ; bug fix for 1.6 (it calculates left margin wrong):
      (defun nlinum--setup-window ()
	(set-window-margins nil (if nlinum-mode nlinum--width)
			    (cdr (window-margins))))

      (with-no-warnings
	(global-nlinum-mode 1))
      )
)


(if (and (>= emacs-major-version 22) (< emacs-major-version 24))
    ;
    ; For Gnu Emacs 22-23, linum mode is superior:
    ;
    (progn
      ; linum not included with Gnuemacs until version 23:
      (if (= emacs-major-version 22)
	  (load "~/elisp/linum-22"))

      (require 'linum)
      (setq linum-format "%7d")
      (setq mdl-frame-extra 7)
      (set-face-foreground 'linum "blue")

      ; ensure header line with links is reachable by keystrokes:
      (setq Info-use-header-line nil)

      ; bug fix for line numbers given intangible text; patch submitted...
      ; ditto numbering restrictions correctly...
      (defun linum-update-window (win)
	"Update line numbers for the portion visible in window WIN."
	(goto-char (window-start win))
	(let ((line (save-restriction
		      (widen)
		      (line-number-at-pos)))
	      (limit (window-end win t))
	      (fmt (cond ((stringp linum-format) linum-format)
			 ((eq linum-format 'dynamic)
			  (let ((w (length (number-to-string
					    (count-lines (point-min) (point-max))))))
			    (concat "%" (number-to-string w) "d")))))
	      (width 0))
	  (run-hooks 'linum-before-numbering-hook)
	  ;; Create an overlay (or reuse an existing one) for each
	  ;; line visible in this window, if necessary.
	  (while (and (not (eobp)) (<= (point) limit))
	    (let* ((str (if fmt
			    (propertize (format fmt line) 'face 'linum)
			  (funcall linum-format line)))
		   (visited (catch 'visited
			      (dolist (o (overlays-in (point) (point)))
				(when (equal-including-properties
				       (overlay-get o 'linum-str) str)
				  (unless (memq o linum-overlays)
				    (push o linum-overlays))
				  (setq linum-available (delq o linum-available))
				  (throw 'visited t))))))
	      (setq width (max width (length str)))
	      (unless visited
		(let ((ov (if (null linum-available)
			      (make-overlay (point) (point))
			    (move-overlay (pop linum-available) (point) (point)))))
		  (push ov linum-overlays)
		  (overlay-put ov 'before-string
			       (propertize " " 'display `((margin left-margin) ,str)))
		  (overlay-put ov 'linum-str str))))
	    (let ((inhibit-point-motion-hooks t))
	      (forward-line))
	    (setq line (1+ line)))
	  (set-window-margins win width)))

      (with-no-warnings (global-linum-mode 1))  ; defined via marco in linum
      ; this does not properly number the last line if empty
      ; can patch later (http://www.emacswiki.org/emacs/LineNumbers) if needed
    )
)

(if (< emacs-major-version 22)
    (progn
      ;
      ; For earlier versions of Gnuemacs, I find wb-line-number mode works best:
      ;
      ;(load "~/elisp/wb-line-number")
      (load "~/elisp/wb-line-number-patched") ;; workaround for v-m bug

      (set-scroll-bar-mode nil)	; no scroll bar, even in X-window system
      ;(setq truncate-partial-width-windows nil) ; use continuous line
      (setq mdl-frame-extra 10)
      (set-face-foreground 'wb-line-number-face "blue")

      (add-hook 'after-make-frame-functions
		(function (lambda (frame)
			    (select-frame frame)
			    (wb-line-number-enable)
			    )))
      (wb-line-number-enable)

      ; disable immovable header lines that confuse wb-line-number:
      (setq Buffer-menu-use-header-line nil)
      (setq Info-use-header-line nil)
    )
)

; make 80-columns-wide fit when using line-numbers:
; (On X windows, the .Xdefaults file overrides this for first frame)
(add-to-list 'default-frame-alist `(width . ,(+ 80 mdl-frame-extra)))



;;;
;;; Code to jump to the start of the nearest [on screen] line to point
;;; such that its line number is equal to N mod 100.
;;;

(defun mdl-current-line-number ()
  "\
Return current line number of line containing point ignoring
narrowing."
  (1+ (count-lines 1 (line-beginning-position))))


(defun mdl-goto-line (n)
  "\
Like goto-line but also returns true iff desired line exists and is
visible."
  (with-no-warnings (goto-line n))
  (and (< 0 n)
       (pos-visible-in-window-p)
       (or (/= (line-end-position) (point-max)) ; attempt to avoid count-lines
	   (= n (mdl-current-line-number)))))    


;; 
;; This routine is appropriate for windows with fewer than 50 lines;
;; for larger windows, it may pick a closer off-screen line instead of
;; a further away on-screen line.
;; 
(defun mdl-goto-line-mod100 (n)
  "\
Go to the nearest line to point such that it's line number is
equal to N mod 100.  N should be between 0 and 99 inclusive.
Leaves a mark where we jumped from.  Also sets register '*' to
where we jumped from.  Ties are broken in an unspecified manner.

Considers potential lines before the buffer's first line (with
negative line numbers) and after the buffer's last line as targets."
  (interactive "NLast two digits of line number: ")
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
	 (closest      (if a-closest option-a option-b)))
    (with-no-warnings (goto-line closest))))


;; 
;; This routine is appropriate for windows with fewer than 101 lines.
;; 
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
    (or (mdl-goto-line closest)
	(mdl-goto-line farthest)
	(with-no-warnings (goto-line closest)))))



;(define-key esc-map "G" 'mdl-goto-line-mod100)
(define-key esc-map "G" 'mdl-goto-onscreen-line-mod100)
