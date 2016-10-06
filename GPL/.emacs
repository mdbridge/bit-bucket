;;; 
;;; Initialization code for Gnuemacs
;;; 

  ; this must be literally in the .emacs file, with the username hardcoded:
(setq inhibit-startup-echo-area-message "mdl")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((encoding . latin-1) (encoding . binary) (encoding . utf-8) (encoding . windows-1252)))))



(setq load-path (append load-path '("~/elisp")))

(load "~/elisp/my-emacs")
