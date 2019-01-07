;;; -*-Emacs-Lisp-*-
;;; xscala-mode-navigation.el - 

;; Copyright (C) 2009-2011 Scala Dev Team at EPFL
;; Authors: See AUTHORS file
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

(provide 'xscala-mode-navigation)

(require 'xscala-mode-constants)

(defun xscala-when-looking-at* (regexp &optional thunk)
  (let ((saved-match-data (match-data)))
    (if (looking-at regexp)
        (progn (goto-char (match-end 0))
               (set-match-data saved-match-data)
               (or (not thunk) (funcall thunk)))
      (set-match-data saved-match-data)
      nil)))

(defmacro xscala-when-looking-at (regexp &rest body)
  (if body
      `(xscala-when-looking-at* ,regexp (lambda () ,@body))
    `(xscala-when-looking-at* ,regexp)))

(defun xscala-forward-spaces (&optional limit)
  (if limit
      (save-restriction
        (narrow-to-region (point) limit)
        (forward-comment 100000))
    (forward-comment 100000)))

(defun xscala-backward-spaces ()
  (forward-comment -100000))

(defun xscala-looking-at-backward (re)
  (save-excursion
    (when (= 0 (skip-syntax-backward "w_")) (backward-char))
    (looking-at re)))

(defmacro xscala-point-after (&rest body)
  `(save-excursion
     ,@body
     (point)))


(defmacro xscala-move-if (&rest body)
  (let ((pt-sym (make-symbol "point"))
	(res-sym (make-symbol "result")))
    `(let ((,pt-sym (point))
	   (,res-sym ,(cons 'progn body)))
       (unless ,res-sym (goto-char ,pt-sym))
       ,res-sym)))

(defun xscala-forward-ident ()
  ;; Move forward over an identifier.
  (xscala-forward-spaces)
  (if (looking-at xscala-ident-re)
      (goto-char (match-end 0))
    (forward-char))
  t)

(defun xscala-backward-ident ()
  ;; Move backward over an identifier.
  (xscala-backward-spaces)
  (if (xscala-looking-at-backward xscala-ident-re)
      (goto-char (match-beginning 0))
    (backward-char))
  t)

(defun xscala-forward-qual-ident ()
  ;; Move forward over a qualifier identifier.
  (xscala-forward-spaces)
  (if (looking-at xscala-qual-ident-re)
      (goto-char (match-end 0))
    (forward-char))
  t)

(defun xscala-backward-qual-ident ()
  ;; Move backward over a qualifier identifier.
  (xscala-backward-spaces)
  (if (xscala-looking-at-backward xscala-qual-ident-re)
      (goto-char (match-beginning 0))
    (backward-char))
  t)

(defun xscala-forward-simple-type ()
  ;; Move forward over a simple type (as defined by the grammar).
  ;; Works only when point is at the beginning of a simple type
  ;; (modulo initial spaces/comments).
  (cond ((eobp) nil)
        ((= (char-after) ?\()
         ;; Parenthesized type
         (forward-sexp)
         t)
        (t
         ;; Type designator
         (xscala-forward-qual-ident)
         (xscala-forward-spaces)
         (cond ((eobp) nil)
               ((= (char-after) ?\[)
                ;; Type arguments
                (forward-sexp))
               ((= (char-after) ?\#)
                ;; Type selection
                (forward-char)
                (xscala-forward-ident)))
         t)))

(defun xscala-forward-type1 ()
  ;; Move forward over a type1 (as defined by the grammar).
  ;; Works only when point is at the beginning of a type (modulo
  ;; initial spaces/comments).
  (xscala-forward-spaces)
  (xscala-when-looking-at "\\<class\\>"
                         (forward-word 1) (xscala-forward-spaces))
  (xscala-forward-simple-type)
  (while (xscala-when-looking-at "\\s *\\<with\\>\\s *")
    (if (and (not (eobp)) (= (char-after) ?\{))
        (forward-sexp)                       ;skip refinement
      (xscala-forward-simple-type)))
  t)

(defun xscala-forward-type ()
  ;; Move forward over a type.
  (cond ((eobp) nil)
        ((= (char-after) ?\()
         ;; Function type (several arguments)
         (forward-sexp)
         (xscala-when-looking-at "\\s *=>\\s *" (xscala-forward-type))
         t)
        (t
         ;; Type1 or function type with one argument
         (xscala-forward-type1)
         (xscala-when-looking-at "\\s *=>\\s *" (xscala-forward-type))
         t)))

(defun xscala-forward-type-param ()
  ;; Move over a type parameter
  ;; variance
  (xscala-when-looking-at "\\s *[-+]\\s *")
  (xscala-forward-ident)
  ;; bounds
  (while (xscala-when-looking-at "\\s *[<>][:%]\\s *")
    (xscala-forward-type))
  t)

(defun xscala-forward-literal ()
  ;; Move forward over an integer, float, character or string literal.
  (xscala-forward-spaces)
  (xscala-when-looking-at xscala-literal-re)
  t)
