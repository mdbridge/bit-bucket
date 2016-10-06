;;;
;;; leap.el: Gnuemacs functions for use in implementing the Leap v2.0 API
;;;          of leap.vch; see leap4_gnuemacs.vch for their use.
;;;
;;; Author: Mark Lillibridge
;;;
;;; (c) Copyright 2009-2011,2016 Hewlett Packard Enterprise Development LP
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

;; Version: 3.2


(defun leap-isearch-atomic-left-exit ()
  "\
Exit search.  If successful, move to start (left edge) of match.  
Otherwise, go back to the original search starting point then quit.
" 
  (interactive)
  (if (not isearch-success)
      (isearch-cancel)
    (if isearch-forward
	(goto-char isearch-other-end))
    ; next two lines are isearch-exit w/o
    ; search-nonincremental-instead functionality:
    (isearch-done)
    (isearch-clean-overlays)))

(defun leap-isearch-atomic-right-exit ()
  "\
Exit search.  If successful, move to end (right edge) of match.  
Otherwise, go back to the original search starting point then quit.
" 
  (interactive)
  (if (not isearch-success)
      (isearch-cancel)
    (if (not isearch-forward)
	(goto-char isearch-other-end))
    ; next two lines are isearch-exit w/o
    ; search-nonincremental-instead functionality:
    (isearch-done)
    (isearch-clean-overlays)))


(defun leap-isearch-advance ()
  "\
When called in the middle of an incremental search when the search string 
is empty, restarts the current search one character further in the direction
we are currently searching.  (This moves the search anchor point.)

When advancing the character further isn't possible due to being at an edge 
of the current buffer, instead rings the bell and makes the current search fail.
"
  (interactive)

  ; exit isearch unless called when have an empty search string:
  (if (/= 0 (length isearch-string))
      (isearch-cancel))

  (if (if isearch-forward (eobp) (bobp))
      ;; If there's nowhere to advance to, fail
      (progn
	(setq isearch-success nil)
	(setq isearch-barrier (point)) ; For subsequent \| if regexp.
	(ding))
    (forward-char (if isearch-forward 1 -1))
    (setq isearch-barrier (point)) ; For subsequent \| if regexp.
    (isearch-search))

  (isearch-push-state)
  (isearch-update))



;; 
;; These bindings are intended to be used by voice commands and are thus 
;; intentionally obscure so as to not get in the way of users using the 
;; keyboard.  Moreover, they are intended to be transmittable by a normal
;; terminal (e.g., ^( isn't an ASCII character but ^_ is.) so they
;; can be used with emacs in terminal mode (-nw).
;; 
;; ^_ is a hard to type form of undo, which is almost never used in 
;; incremental search.
;; 

(define-prefix-command 'isearch-mode-map-leap)
(define-key isearch-mode-map [?\C-_]    'isearch-mode-map-leap)

(define-key isearch-mode-map [?\C-_ ?a] 'leap-isearch-advance)

(define-key isearch-mode-map [?\C-_ ?l] 'leap-isearch-atomic-left-exit)
(define-key isearch-mode-map [?\C-_ ?r] 'leap-isearch-atomic-right-exit)
