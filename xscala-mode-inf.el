;;; -*-Emacs-Lisp-*-
;;; xscala-mode-inf.el - Interaction with a Scala interpreter.

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

(provide 'xscala-mode-inf)


(require 'xscala-mode-variables)
(require 'comint)

(define-derived-mode xscala-mode-inf comint-mode "Inferior Scala"
  "Minor mode for interacting with a Scala interpreter. It provides a means to change the buffer
that commands are sent to by using the function `xscala-toggle'. A use-case would be to start the sbt console go to the sbt console, put it into \"console\" mode and invoke M-x xscala-toggle. Scala code will then be sent to that interpreter.

\\{inferior-xscala-mode-map\\}"
  (define-key xscala-mode-inf-map [(meta return)] 'comint-accumulate)

  ;; Comint configuration
  (make-local-variable 'comint-input-sender)
  (setq comint-input-sender 'xscala-input-sender))

(defun xscala-toggle (&optional x0)
  "This sets the `ensime-inf-buffer-name' value to the string given or the name of the current buffer."
  (interactive)
  (or x0 (setq x0 (buffer-name)))
  (setq ensime-inf-buffer-name x0)
  (message "ensime-inf-buffer-name: \"%s\"" ensime-inf-buffer-name) )

(defun xscala-starter ()
  "Run an inferior instance of `xscala-interpreter' inside Emacs. https://www.masteringemacs.org/article/comint-writing-command-interpreter "
  (interactive)
  (let* ((xscala-program xscala-interpreter)
         (buffer (comint-check-proc "xscala")))
    ;; pop to the "*xscala*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'xscala-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*xscala*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "xscala" buffer
             xscala-program nil xscala-interpreter-options)
      (xscala-mode)
      (xscala-toggle))))

(defun xscala-input-sender (proc string)
  (comint-send-string proc string)
  ;; (comint-send-string proc "\nemacs:end\n")) ;; Heineman's contrib (06/03/2007)
  (comint-send-string proc "\n"))

(defvar xscala-tmp-file nil)

;;;###autoload
(defun xscala-eval-region (start end)
  "Send current region to Scala interpreter."
  (interactive "r")
  (ensime-inf-assert-running)
  (comint-send-region ensime-inf-buffer-name start end)
  (comint-send-string ensime-inf-buffer-name "\n")
  (sleep-for 0.2)
)

(defun xscala-eval-paste-region (start end)
  "Send current region to Scala interpreter as a paste."
  (interactive)
  (ensime-inf-assert-running)
  (let ((src0 (current-buffer)))
    (comint-send-string ensime-inf-buffer-name ":paste\n")
    (comint-send-region ensime-inf-buffer-name start end)
    (switch-to-buffer ensime-inf-buffer-name)
    (comint-send-string nil "\n")
    (sleep-for 0.2)
    (comint-send-eof)
    (sleep-for 0.2)
    (pop-to-buffer-same-window src0))
  )

;;; Send a paragraph and step forward.
;;; This doesn't work at all well
;;;###autoload
(defun xscala-eval-step ()
  (interactive)
  (save-excursion
    (let ((beg (point)))
      (forward-paragraph)
      (xscala-eval-region beg (point)) ))
  (forward-paragraph))

(defun xscala-eval-paste-mark ()
  (interactive)
  (save-excursion 
  (let ((beg (point)))
    (re-search-forward scala-edit-mark-re)
    (beginning-of-line)
    (xscala-eval-paste-region beg (point)) )) )

(defun xscala-eval-mark ()
  (interactive)
  (save-excursion 
  (let ((beg (point)))
    (re-search-forward scala-edit-mark-re)
    (beginning-of-line)
    (xscala-eval-region beg (point)) )) )

(defun xscala-eval-paste-mark-step ()
  (interactive)
  (xscala-eval-paste-mark)
  (re-search-forward scala-edit-mark-re)
  (next-line) )

(defun xscala-eval-mark-step ()
  (interactive)
  (xscala-eval-mark)
  (re-search-forward scala-edit-mark-re)
  (next-line) )

(defun xscala-mark-backward ()
  (interactive)
  (next-line -1)
  (re-search-backward scala-edit-mark-re)
  (next-line))

(defun xscala-mark-forward ()
  (interactive)
  (next-line)
  (re-search-forward scala-edit-mark-re))

(defun xscala-mark-insert ()
  (interactive)
  (next-line)
  (re-search-forward scala-edit-mark-re))

;;;###autoload
(defun xscala-eval-buffer ()
  "Send whole buffer to Scala interpreter."
  (interactive)
  (xscala-eval-region (point-min) (point-max)))

(defvar xscala-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last xscala-load-file.
Used for determining the default in the next one.")

;;;###autoload
(defun xscala-load-file (file-name)
  "Load a file in the Scala interpreter."
  (interactive (comint-get-source "Load Scala file: " xscala-prev-l/c-dir/file
				  '(xscala-mode) t))
  (ensime-inf-assert-running)
  (comint-check-source file-name)
  (setq xscala-prev-l/c-dir/file (cons (file-name-directory file-name)
                                      (file-name-nondirectory file-name)))
  (ensime-inf-send-string ":load %s" file-name))


