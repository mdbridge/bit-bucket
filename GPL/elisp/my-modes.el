;;; 
;;; Customize the most often used modes
;;; 
;;; 
;;; (c) Copyright 2016 Hewlett Packard Enterprise Development LP
;;;
;;; Copyright (C) 1985-1987, 1993-1999, 2001-2015 Free Software
;;; Foundation, Inc.       [for grep code]
;;;
;;; Copyright (C) 1993-1994, 1997, 2001-2015 Free Software Foundation,
;;; Inc.                   [for dired-x code]
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

;;
;; Make the default major mode be text:
;;
(with-no-warnings (setq default-major-mode 'text-mode))  ; obsolete since 23.2
(if (>= emacs-major-version 22)
    (setq-default major-mode 'text-mode))
(setq initial-major-mode 'text-mode)  ; was meta-x lisp-interaction-mode...


;; 
;; Customize mail:
;; 
(load "~/elisp/my-mail")


;; 
;; Programming language modes:
;; 

  ; make shell script files executable:
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


  ; fix C indentation style:
(eval-when-compile 
  (require 'cc-vars))
(setq c-default-style  '((java-mode . "java")
                         (awk-mode  . "awk")
                         (other     . "k&r"))
      c-basic-offset   4
)   

  ; don't use hard tabs...
(add-hook 'c-mode-common-hook
      '(lambda ()
	 ; this variable is automatically buffer local:
	 (setq indent-tabs-mode nil)
	 ))

  ; automatically guess C indentation level (doesn't always work):
;(add-hook 'c-mode-common-hook
;      '(lambda ()
;	 (load "~/elisp/inactive/guess-offset") ; 
;	 ))

  ; always use C++ mode for .h files:
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


  ; python-mode sometimes tries to set its syntax table to treat underscores as
  ; word-constituent. don't let it!
(add-hook 'python-mode-hook (lambda () (modify-syntax-entry ?_ "_")))


; add Ruby mode if not yet built in:
(if (< emacs-major-version 23)  
    (progn
      ; newer version shadows this in version 23 emacs:
      (autoload 'ruby-mode "~/elisp/ruby-mode.elc" "ruby mode" t nil)
      (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
      ))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(defvar ruby-encoding-magic-comment-style) 
(setq ruby-encoding-magic-comment-style 'emacs)


; C# mode:
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
(defvar csharp-mode-map)
(defvar csharp-want-imenu)
(defun my-csharp-mode-fn ()
  "my function that runs when csharp-mode is initialized for a buffer."
  ; stop stupid auto "}" insertion...
  (define-key csharp-mode-map (kbd "{") 'c-electric-brace)
  (setq csharp-want-imenu nil) ; don't need this and it causes bugs
  )
(add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)


; elisp mode:
(add-hook 'emacs-lisp-mode-hook       'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)


; Vocola mode:
(load "~/elisp/vocola-mode")


; HAML mode:
(load "~/elisp/haml-mode")
  ; don't use hard tabs...
(add-hook 'haml-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)))



;;
;; Customize text mode:
;;

(add-hook 'text-mode-hook
      '(lambda ()
	 (setq fill-column 72)
	 (auto-fill-mode 1)
	 ))

; prevent problems with voice corrections by inhibiting where auto fill runs:
(require 'newcomment)  ; need to ensure comment-beginning loaded
(defun mdl-do-auto-fill ()
  "Like do-auto-fill but only fills when (automatic) fill prefix
is empty, there is no current indentation, and where we are not
in a comment -- aka, when no text could be inserted at line start"
  (let ((my-fill-prefix fill-prefix)
	(in-comment-p (and comment-start 
			   (save-excursion (comment-beginning)))))
      ;; Choose a fill-prefix automatically.
      (when (and adaptive-fill-mode
		 (or (null my-fill-prefix) (string= my-fill-prefix "")))
	(let ((prefix
	       (fill-context-prefix
		(save-excursion (backward-paragraph 1) (point))
		(save-excursion (forward-paragraph 1) (point)))))
	  (and prefix (not (equal prefix ""))
	       ;; Use auto-indentation rather than a guessed empty prefix.
	       (not (and fill-indent-according-to-mode
			 (string-match "\\`[ \t]*\\'" prefix)))
	       (setq my-fill-prefix prefix))))
      (if (and (not in-comment-p)
	       (or (null my-fill-prefix) (string= my-fill-prefix ""))
	       (eq (current-indentation) 0))
	  (do-auto-fill)
	nil)))
(if (>= emacs-major-version 22)  ; not worth fixing to work with version 21
    (setq normal-auto-fill-function 'mdl-do-auto-fill))

(if (>= emacs-major-version 22)
    (add-hook 'message-mode-hook 
	      #'(lambda () 
		  (defun message-do-auto-fill ()
		    "Like `mdl-do-auto-fill', but don't fill in message header."
		    (unless (message-point-in-header-p)
		      (mdl-do-auto-fill))))))


;
; Make a single long line paragraph starting with an adaptive prefix
; of all whitespace fill to
;
;  XXXXX
;XXXXXXX
;XXXXXXX
;
; rather than the Emacs default of:
;
;  XXXXX
;  XXXXX
;  XXXXX
;
(defadvice fill-context-prefix (around fix-fill 
				      (from to &optional first-line-regexp))
  (let ((one-line-p 
	 (save-excursion
	   (goto-char from)
	   (if (eolp) (forward-line 1))
	   ;; Move to the second line unless there is just one.
	   (move-to-left-margin)
	   (forward-line 1)
	   (if (< (point) to)
	       nil
	     t))))
    (if one-line-p
	; the standard adaptive-fill-regexp value modified to require at
	; least one non-whitespace character:
	(let ((adaptive-fill-regexp "[ \t]*\\([-!|#%;>*·•‣⁃◦]+[ \t]*\\)+"))
	  ad-do-it)
      ad-do-it)))
(ad-activate 'fill-context-prefix)



;; 
;; Latex mode:
;; 

; fix stupid latex filling in V22,23 <<<>>>:
(add-hook 'latex-mode-hook
	  (lambda ()
	    (set (make-local-variable 'fill-indent-according-to-mode) nil)))



;; 
;; Grep mode:
;; 

(eval-when-compile 
  (if (>= emacs-major-version 22)
      (require 'grep)))


(if (and (>= emacs-major-version 22)
	 (< emacs-major-version 24))
    (progn

;
; Patch for bug#6114: 23.1; grep-read-files does incorrect wildcard match
; (fixed in 24)

; use advice to avoid need to preload grep mode:
(defadvice grep-read-files (around grep-read-files-around activate)
  "Workaround for bug #6114."
  (setq ad-return-value
	; below is complete replacement for this function based on v22:
	(let* ((bn (or (buffer-file-name) (buffer-name)))
	       (fn (and bn
			(stringp bn)
			(file-name-nondirectory bn)))
	       (default
		 (or (and fn
			  (let ((aliases (remove (assoc "all" grep-files-aliases)
						 grep-files-aliases))
				alias)
			    (while aliases
			      (setq alias (car aliases)
				    aliases (cdr aliases))
			      (if (string-match (mapconcat
						 'wildcard-to-regexp
						 (split-string (cdr alias) nil t)
						 "\\|")
						fn)
				  (setq aliases nil)
				(setq alias nil)))
			    (cdr alias)))
		     (and fn
			  (let ((ext (file-name-extension fn)))
			    (and ext (concat "*." ext))))
		     (car grep-files-history)
		     (car (car grep-files-aliases))))
	       (files (read-string
		       (concat "Search for \"" regexp
			       "\" in files"
			       (if default (concat " (default " default ")"))
			       ": ")
		       nil 'grep-files-history default)))
	  (and files
	       (or (cdr (assoc files grep-files-aliases))
		   files)))
	))

)) ; end (if (< emacs-major-version 23)


(setq grep-files-aliases
  '(
    ; make lgrep have reasonable pattern defaults:
    ("c++" .  "*.h *.cpp *.c")
    ("v" .    "*.vcl *.vch")

    ; below this point copied from version 24:
    ("all" . "* .*")
    ("el" . "*.el")
    ("ch" . "*.[ch]")
    ("c" . "*.c")
    ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++")
    ("cchh" . "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
    ("hh" . "*.hxx *.hpp *.[Hh] *.HH *.h++")
    ("h" . "*.h")
    ("l" . "[Cc]hange[Ll]og*")
    ("m" . "[Mm]akefile*")
    ("tex" . "*.tex")
    ("texi" . "*.texi")
    ("asm" . "*.[sS]")
    ))


(defun mdl-lgrep (regexp &optional files dir confirm)
  "[MDL] Like lgrep but does not prompt for DIR."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-command
				   nil nil 'grep-history)))
      ((not grep-template)
       (error "grep.el: No `grep-template' available"))
      (t (let* ((regexp (grep-read-regexp))
		(files (grep-read-files regexp))
		;(dir (read-directory-name "In directory: "
		;                           nil default-directory t))
		(dir default-directory)
		(confirm (equal current-prefix-arg '(4))))
	   (list regexp files dir confirm))))))
  (if (< emacs-major-version 23) 
      (lgrep regexp files dir)
    (lgrep regexp files dir confirm))
)

(defun mdl-rgrep (regexp &optional files dir confirm)
  "[MDL] Like rgrep but does not prompt for DIR."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
				   nil nil 'grep-find-history)))
      ((not grep-find-template)
       (error "grep.el: No `grep-find-template' available"))
      (t (let* ((regexp (grep-read-regexp))
		(files (grep-read-files regexp))
		;(dir (read-directory-name "Base directory: "
		;     			   nil default-directory t))
		(dir default-directory)
		(confirm (equal current-prefix-arg '(4))))
	   (list regexp files dir confirm))))))
  (if (< emacs-major-version 23) 
      (rgrep regexp files dir)
    (rgrep regexp files dir confirm))
)



;; 
;; Dired mode:
;; 

(put 'dired-find-alternate-file 'disabled nil) ; enable 'a' command

(if (and (>= emacs-major-version 22)
	 (< emacs-major-version 24))
    (load "ls-lisp-patch.el"))


; from dired-x.el, disabled refs to dired-omit-mode:
(eval-when-compile 
  (require 'dired)
  (require 'dired-aux))
(defun dired-jump (&optional other-window file-name)
  "Jump to dired buffer corresponding to current buffer.
If in a file, dired the current directory and move to file's line.
If in Dired already, pop up a level and goto old directory's line.
In case the proper dired file line cannot be found, refresh the dired
buffer and try again.
When OTHER-WINDOW is non-nil, jump to dired buffer in other window.
Interactively with prefix argument, read FILE-NAME and
move to its line in dired."
  (interactive
   (list nil (and current-prefix-arg
		  (read-file-name "Jump to dired file: "))))
  (let* ((file (or file-name buffer-file-name))
         (dir (if file (file-name-directory file) default-directory)))
    (if (and (eq major-mode 'dired-mode) (null file-name))
        (progn
          (setq dir (dired-current-directory))
          (dired-up-directory other-window)
          (unless (dired-goto-file dir)
              ;; refresh and try again
            (dired-insert-subdir (file-name-directory dir))
            (dired-goto-file dir)))
      (if other-window
          (dired-other-window dir)
        (dired dir))
      (if file
          (or (dired-goto-file file)
              ;; refresh and try again
              (progn
                (dired-insert-subdir (file-name-directory file))
                (dired-goto-file file))
              ;; Toggle omitting, if it is on, and try again.
	      ;(when dired-omit-mode  ; <<<>>>
              ;  (dired-omit-mode)
	      (dired-goto-file file)
	      ;)
	      )))))


;; 
;; E browse mode:
;; 

; make {ctrl+l} work as normal in E browse mode:
(add-hook 'ebrowse-tree-mode-hook
      '(lambda ()
	 (define-key ebrowse-tree-mode-map "\C-l" 'recenter-top-bottom)
	 ))


;; 
;; Experiment: Buffer menu mode:
;; 

; width of name field in buffer-menu mode (default is 19):
(setq Buffer-menu-name-width 30)

(defvar buffer-menu-buffer-font-lock-keywords) 
(setq buffer-menu-buffer-font-lock-keywords
      '(
       ;(".*Dired.*"             . font-lock-comment-face)      ; Dired
        ("^....[*].*"            . font-lock-string-face)       ; "*" named buffers
        ("^..[*].*"              . font-lock-warning-face)      ; Modified
        ("^.[%].*"               . font-lock-keyword-face)      ; Read only

        ("^....[*]shell.*"       . font-lock-preprocessor-face)  ; shell buff
        (".*[*]scratch[*].*"     . font-lock-function-name-face) ; scratch buffer
        ))

(defun buffer-menu-custom-font-lock  ()
  (let ((font-lock-unfontify-region-function
	 #'(lambda (start end)
	     (remove-text-properties start end '(font-lock-face nil)))))
    (font-lock-unfontify-buffer)
    (set (make-local-variable 'font-lock-defaults)
	 '(buffer-menu-buffer-font-lock-keywords t))
    (font-lock-fontify-buffer)))

(if (>= emacs-major-version 22)
    (add-hook 'buffer-menu-mode-hook 'buffer-menu-custom-font-lock))

(if (>= emacs-major-version 23)
    (progn
      ; buffer-menu-mode-hook doesn't get called after filling the
      ; buffer anymore in 24.3+ so need to use advice:
      (defadvice tabulated-list-print (after mdl-buffer-menu-advice)
	(if (eq major-mode 'Buffer-menu-mode)
	    (buffer-menu-custom-font-lock)))

      (ad-activate 'tabulated-list-print)
      ))


;; 
;; Ace jump mode:
;; 

(require 'cl)   ; <<<>>>

(eval-when-compile 
  (load "~/elisp/ace-jump-mode"))
(load "~/elisp/ace-jump-mode")

  ; only search current window:
(defvar ace-jump-mode-scope)
(setq ace-jump-mode-scope 'window)

  ; use 0-9 as valid move keys:
(defvar ace-jump-mode-move-keys)
(setq ace-jump-mode-move-keys
      (nconc (loop for i from ?0 to ?9 collect i)
	     (loop for i from ?a to ?z collect i)
	     (loop for i from ?A to ?Z collect i)))


(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(defun mdl-Ace-jump (regex)
  "Start Ace jump targeting given regex; does not signal an error in normal usage.

You can control whether to be case sensitive via
`ace-jump-mode-case-fold'.
"
  (condition-case e
      (ace-jump-do regex)
    (error (cond ((string= (error-message-string e) "[AceJump] No one found")
		  (ding)
		  (message "%s" "AceJump: No occurrence found!"))
		 (t 
		  (signal (car e) (cdr e)))))))


;; 
;; Align mode:
;; 

(defvar align-to-tab-stop)
(setq align-to-tab-stop 'nil)
