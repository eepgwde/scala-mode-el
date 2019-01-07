;;; -*-Emacs-Lisp-*-
;;; xscala-mode-feature-electric.el - electric editing commands for xscala files

;; Copyright (C) 2009 by Hemant Kumar (gethemant at gmail to com)
;; Modified by Anders Bach Nielsen <andersbach.nielsen at epfl dot ch> to fit into the xscala mode
;; Based on ruby-electric by Dee Zsombor <dee dot zsombor at gmail dot com>.
;; Keywords: xscala languages oop

;;; License

;; SCALA LICENSE
;;  
;; Copyright (c) 2002-2011 EPFL, Lausanne, unless otherwise specified.
;; All rights reserved.
;;  
;; This software was developed by the Programming Methods Laboratory of the
;; Swiss Federal Institute of Technology (EPFL), Lausanne, Switzerland.
;;  
;; Permission to use, copy, modify, and distribute this software in source
;; or binary form for any purpose with or without fee is hereby granted,
;; provided that the following conditions are met:
;;  
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;  
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;  
;;    3. Neither the name of the EPFL nor the names of its contributors
;;       may be used to endorse or promote products derived from this
;;       software without specific prior written permission.
;;  
;;  
;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'xscala-mode-feature-electric)

;; Customization
 
(defgroup xscala-mode-feature:electric nil
  "Minor mode providing electric editing commands for xscala files"
  :group 'xscala)


(defcustom xscala-mode-feature:electric-expand-delimiters-list '(all)
  "*List of contexts where matching delimiter should be
inserted. The word 'all' will do all insertions."
  :type '(set :extra-offset 8
              (const :tag "Everything" all )
              (const :tag "Curly brace" ?\{ )
              (const :tag "Square brace" ?\[ )
              (const :tag "Round brace" ?\( )
              (const :tag "Quote" ?\' )
              (const :tag "Double quote" ?\" )
              (const :tag "Back quote" ?\` )
              (const :tag "Vertical bar" ?\| ))
  :group 'xscala-mode-feature:electric)


(defcustom xscala-mode-feature:electric-newline-before-closing-bracket nil
  "*Controls whether a newline should be inserted before the
closing bracket or not."
  :type 'boolean 
  :group 'xscala-mode-feature:electric)


(defcustom xscala-mode-feature:electric-on-per-default nil
  "*Controls whether xscala electric mode should be on per default or not."
  :type 'boolean
  :group 'xscala-mode-feature:electric)

;; Variables

(defvar xscala-mode-feature-electric-matching-delimeter-alist
  '((?\[ . ?\])
    (?\( . ?\))
    (?\' . ?\')
    (?\` . ?\`)
    (?\" . ?\")))


(defvar xscala-mode-feature-electric-mode xscala-mode-feature:electric-on-per-default
  "nil disables xscala electric mode, non-nil enables.")

(defvar xscala-mode-feature-electric-mode-map (make-sparse-keymap)
  "Keymap for xscala electric minor mode.")

;;; Mode setup

(make-variable-buffer-local 'xscala-mode-feature-electric-mode)

(defun xscala-mode-feature-electric-mode (&optional arg)
  ""
  (interactive "P")
  (setq xscala-mode-feature-electric-mode
        (if (null arg)
            ;; Toggle mode
            (not xscala-mode-feature-electric-mode)
          ;; Enable/Disable according to arg
          (> (prefix-numeric-value arg) 0)))
  )

;; Alias for some backwards compat
(defalias 'xscala-electric-mode 'xscala-mode-feature-electric-mode)


;; Functions
(defun xscala-mode-feature-electric-active-p ()
  xscala-mode-feature-electric-mode)

(defun xscala-mode-feature-electric-code-at-point-p()
  (and xscala-mode-feature-electric-mode
       (let* ((properties (text-properties-at (point))))
         (and (null (memq 'font-lock-string-face properties))
              (null (memq 'font-lock-comment-face properties))))))

(defun xscala-mode-feature-electric-string-at-point-p()
  (and xscala-mode-feature-electric-mode
       (consp (memq 'font-lock-string-face (text-properties-at (point))))))

(defun xscala-mode-feature-electric-is-last-command-char-expandable-punct-p()
  (or (memq 'all xscala-mode-feature:electric-expand-delimiters-list)
      (memq last-command-char xscala-mode-feature:electric-expand-delimiters-list)))

(defun xscala-mode-feature-electric-curlies(arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if (xscala-mode-feature-electric-is-last-command-char-expandable-punct-p)
      (cond ((xscala-mode-feature-electric-code-at-point-p)
             (insert " ")
             (save-excursion
               (if xscala-mode-feature:electric-newline-before-closing-bracket
                   (newline))
               (insert "}")))
            ((xscala-mode-feature-electric-string-at-point-p)
             (save-excursion
               (backward-char 1)
               (when (char-equal ?\# (preceding-char))
                 (forward-char 1)
                 (insert "}")))))))

(defun xscala-mode-feature-electric-matching-char(arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (and (xscala-mode-feature-electric-is-last-command-char-expandable-punct-p)
       (xscala-mode-feature-electric-code-at-point-p)
       (save-excursion
         (insert (cdr (assoc last-command-char
                             xscala-mode-feature-electric-matching-delimeter-alist))))))

(defun xscala-mode-feature-electric-install ()
  (or (assoc 'xscala-mode-feature-electric-mode minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(xscala-mode-feature-electric-mode " electric") minor-mode-alist)))
  
  (or (assoc 'xscala-mode-feature-electric-mode minor-mode-map-alist)
      (setq minor-mode-map-alist
	    (cons (cons 'xscala-mode-feature-electric-mode xscala-mode-feature-electric-mode-map)
		  minor-mode-map-alist)))

  (define-key xscala-mode-feature-electric-mode-map "{"  'xscala-mode-feature-electric-curlies)
  (define-key xscala-mode-feature-electric-mode-map "("	 'xscala-mode-feature-electric-matching-char)
  (define-key xscala-mode-feature-electric-mode-map "["	 'xscala-mode-feature-electric-matching-char)
  (define-key xscala-mode-feature-electric-mode-map "\"" 'xscala-mode-feature-electric-matching-char)
  
  t)
