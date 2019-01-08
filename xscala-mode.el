;;; -*-Emacs-Lisp-*-
;;; xscala-mode.el - minor mode for editing Scala code.

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

(provide 'xscala-mode)

(require 'cl)

(require 'xscala-mode-constants)
(require 'xscala-mode-variables)
(require 'xscala-mode-lib)
(require 'xscala-mode-navigation)
(require 'xscala-mode-indent)
(require 'xscala-mode-fontlock)
(require 'xscala-mode-ui)
(require 'xscala-mode-feature)

;;; Customization and Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup xscala nil
  "Mode for editing Scala code."
  :group 'languages)

(defcustom xscala-mode:api-url "http://www.xscala-lang.org/docu/files/api/index.html"
  "URL to the online Scala documentation"
  :type 'string
  :group 'xscala)

(defconst xscala-mode-version "0.5.99.5")
(defconst xscala-mode-svn-revision "$Revision: 21917 $")
(defconst xscala-bug-e-mail "xscala@listes.epfl.ch")
(defconst xscala-web-url "http://xscala-lang.org/")


;;; Helper functions/macroes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xscala-mode:browse-web-site ()
  "Browse the Scala home-page"
  (interactive)
  (require 'browse-url)
  (browse-url xscala-web-url))


(defun xscala-mode:browse-api ()
  "Browse the Scala API"
  (interactive)
  (require 'browse-url)
  (browse-url xscala-mode:api-url))


(defun xscala-mode:report-bug ()
  "Report a bug to the author of the Scala mode via e-mail.
The package used to edit and send the e-mail is the one selected
through `mail-user-agent'."
  (interactive)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report
     xscala-bug-e-mail
     (concat "Emacs Scala mode v" xscala-mode-version)
     '(xscala-indent-step))))

(defvar xscala-mode-abbrev-table nil
  "Abbrev table in use in `xscala-mode' buffers.")
(define-abbrev-table 'xscala-mode-abbrev-table nil)


(defvar xscala-mode-syntax-table nil
  "Syntax table used in `xscala-mode' buffers.")
(when (not xscala-mode-syntax-table)
  (setq xscala-mode-syntax-table (make-syntax-table))
  ;; strings and character literals
  (modify-syntax-entry ?\" "\"" xscala-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" xscala-mode-syntax-table)

  ;; different kinds of "parenthesis"
  (modify-syntax-entry ?\( "()" xscala-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" xscala-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" xscala-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" xscala-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" xscala-mode-syntax-table)
  (modify-syntax-entry ?\} "){" xscala-mode-syntax-table)

  ;; special characters
  (modify-syntax-entry ?\_ "_" xscala-mode-syntax-table)
  
  (dolist (char xscala-all-special-chars)
    (modify-syntax-entry char "." xscala-mode-syntax-table))

  (modify-syntax-entry ?\. "." xscala-mode-syntax-table)
  
  ;; comments
  ;; the `n' means that comments can be nested
  (modify-syntax-entry ?\/  ". 124nb" xscala-mode-syntax-table)
  (modify-syntax-entry ?\*  ". 23n"   xscala-mode-syntax-table)
  (modify-syntax-entry ?\n  "> bn" xscala-mode-syntax-table)
  (modify-syntax-entry ?\r  "> bn" xscala-mode-syntax-table))


;;; Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-minor-mode xscala-mode ()
  "Minor mode for editing Scala code.
When started, run `xscala-mode-hook'.
\\{xscala-mode-map}"
  :init-value nil
  :lighter " XScala"
  :group 'scala
  ;; Turn off if we change major mode.
  (if xscala-mode
      (progn
        ;; Turn off this mode if we change major modes.
        (add-hook 'change-major-mode-hook
                  (lambda () (xscala-mode -1))
                  nil t)
        ;; (set (make-local-variable 'line-move-ignore-invisible) t)
        ;; Cause use of ellipses for invisible text.
        ;; (add-to-invisibility-spec '(outline . t)))
        ;; (setq line-move-ignore-invisible nil)
        ;; Cause use of ellipses for invisible text.
        ;; (remove-from-invisibility-spec '(outline . t))
        ;; When turning off outline mode, get rid of any outline hiding.
        ))

;;	comment-indent-function       'xscala-comment-indent-function

  (use-local-map xscala-mode-map)
  
  ;; (make-local-variable 'font-lock-defaults)
  (make-local-variable 'outline-regexp)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-end-skip)
  (make-local-variable 'comment-column)
  (make-local-variable 'xscala-std-options)
  (make-local-variable 'xscala-spark-options)
  ;;(make-local-variable 'comment-indent-function)

  (setq paragraph-separate            (concat "^\\s *$\\|" page-delimiter))
	(setq paragraph-start               (concat "^\\s *$\\|" page-delimiter))
	(setq paragraph-ignore-fill-prefix  t)
	(setq require-final-newline         t)
	(setq comment-start-skip            "/\\*+ *\\|//+ *")
	(setq comment-end-skip              " *\\*+/\\| *")

  (if (not scala-edit-mark-re) 
    (setq scala-edit-mark-re (concat "^" scala-edit-mark)) )

  (make-local-variable 'scala-edit-mark)
  (make-local-variable 'scala-edit-mark-re)

  (make-local-variable 'indent-line-function)
  ;;

  (xscala-mode-feature-install)
  (setq xscala-interpreter xscala-std-interpreter)
  (if xscala-mode-hook
      (run-hooks 'xscala-mode-hook)))

