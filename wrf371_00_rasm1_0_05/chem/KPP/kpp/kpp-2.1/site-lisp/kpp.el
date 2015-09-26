;; kpp.el --- kpp mode for GNU Emacs 21
;; (c) Rolf Sander <sander@mpch-mainz.mpg.de>
;; Time-stamp: <2005-02-15 15:18:42 sander>
 
;; to activate it copy kpp.el to a place where emacs can find it and then
;; add "(require 'kpp)" to your .emacs startup file

;; known problem:
;; ":" inside comments between reaction products confuses font-lock
 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; start kpp-mode automatically when loading a *.eqn, *.spc, or *.kpp file
(setq auto-mode-alist
  (cons '("\\.eqn\\'" . kpp-mode) auto-mode-alist))
(setq auto-mode-alist
  (cons '("\\.spc\\'" . kpp-mode) auto-mode-alist))
(setq auto-mode-alist
  (cons '("\\.kpp\\'" . kpp-mode) auto-mode-alist))

(setq kpp-font-lock-keywords
 (list
  '("^\\([^=\n]*=[^:\n]*\\):[^;\n]*;" 1 font-lock-constant-face) ; reaction
  ;; alternatively, use another color for rate constant:
  ;; '("^\\([^=\n]*=[^:\n]*\\):\\([^;\n]*\\);"
  ;;     (1 font-lock-constant-face) (2 font-lock-keyword-face))
  '("<[A-z0-9_#]+>" 0 font-lock-variable-name-face t) ; equation tag
  '("{[^}\n]*}"     0 font-lock-comment-face t)       ; comment
  '("!.*"           0 font-lock-comment-face t)       ; f90 comment
  '("{@[^}]+}"      0 font-lock-doc-face t)           ; alternative LaTeX text
  '("{$[^}]+}"      0 font-lock-string-face t)        ; alternative LaTeX text
  '("{&[^}]+}"      0 font-lock-builtin-face t)       ; BibTeX reference
  '("{%[A-z0-9#]+}" 0 font-lock-type-face t)          ; marker
  ;; KPP sections (Tab. 3 in thesis), commands (Tab. 13 in thesis), and
  ;; fragments (Tab. 17 in thesis)
  (cons (concat 
         "\\(#ATOMS\\|#CHECKALL\\|#CHECK\\|#DEFFIX\\|#DEFRAD"
         "\\|#DEFVAR\\|#DOUBLE\\|#DRIVER\\|#DUMMYINDEX"
         "\\|#ENDINLINE\\|#EQNTAGS\\|#EQUATIONS\\|#FUNCTION"
         "\\|#HESSIAN\\|#INCLUDE\\|#INITIALIZE"
         "\\|#INITVALUES\\|#INLINE\\|#INTEGRATOR\\|#INTFILE"
         "\\|#JACOBIAN\\|#LANGUAGE\\|#LOOKATALL"
         "\\|#LOOKAT\\|#LUMP\\|#MEX\\|#MODEL\\|#MONITOR"
         "\\|#REORDER\\|#RUN\\|#SETFIX\\|#SETRAD\\|#SETVAR"
         "\\|#SPARSEDATA\\|#STOCHASTIC\\|#STOICMAT\\|#TRANSPORTALL"
         "\\|#TRANSPORT\\|#USE\\|#USES\\|#WRITE_ATM"
         "\\|#WRITE_MAT\\|#WRITE_OPT\\|#WRITE_SPC"
         "\\|#XGRID\\|#YGRID\\|#ZGRID\\)"
         ) 'font-lock-keyword-face)
  '("^//.*"         0 font-lock-comment-face t) ; comment
  )
)

; comment a region (adopted from wave-comment-region)

(defvar kpp-comment-region "// "
  "*String inserted by \\[kpp-comment-region] at start of each line in region.")

(defun kpp-comment-region (beg-region end-region arg)
  "Comments every line in the region.
Puts kpp-comment-region at the beginning of every line in the region. 
BEG-REGION and END-REGION are args which specify the region boundaries. 
With non-nil ARG, uncomments the region."
  (interactive "*r\nP")
  (let ((end-region-mark (make-marker)) (save-point (point-marker)))
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (if (not arg)			;comment the region
	(progn (insert kpp-comment-region)
	       (while (and  (= (forward-line 1) 0)
			    (< (point) end-region-mark))
		 (insert kpp-comment-region)))
      (let ((com (regexp-quote kpp-comment-region))) ;uncomment the region
	(if (looking-at com)
	    (delete-region (point) (match-end 0)))
	(while (and  (= (forward-line 1) 0)
		     (< (point) end-region-mark))
	  (if (looking-at com)
	      (delete-region (point) (match-end 0))))))
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)))

(defvar kpp-mode-map () 
  "Keymap used in kpp mode.")

(if kpp-mode-map
    ()
  (setq kpp-mode-map (make-sparse-keymap))
  (define-key kpp-mode-map "\C-c;"    'kpp-comment-region)
  ;; TAB inserts 8 spaces, not the TAB character
  (define-key kpp-mode-map (kbd "TAB")
    '(lambda () (interactive) (insert "        ")))
)

(defun kpp-mode ()
  "Major mode for editing kpp code.
Turning on kpp mode calls the value of the variable `kpp-mode-hook'
with no args, if that value is non-nil.

Command Table:
\\{kpp-mode-map}"
  (interactive)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((kpp-font-lock-keywords) t t))
  (make-local-variable 'comment-start)
  (setq comment-start "{")
  (make-local-variable 'comment-end)
  (setq comment-end "}")
  (use-local-map kpp-mode-map)
  (setq mode-name "kpp")
  (setq major-mode 'kpp-mode)
  (turn-on-font-lock)
  (set-syntax-table (copy-syntax-table))
  (modify-syntax-entry ?_ "w") ; the underscore can be part of a word
  (auto-fill-mode 0)           ; no automatic line breaks
  (run-hooks 'kpp-mode-hook)
)

(provide 'kpp)

;; kpp.el ends here
