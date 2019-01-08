;;; -*-Emacs-Lisp-*-
;;; xscala-mode-auto.el - Autoloads file for the xscala mode

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

;; We now depend on font-locking features only in emacs 21.x and newer
(unless (<= 21 emacs-major-version)
  (error
   (format "The Scala mode require Emacs version 21.x (and not your Emacs version %s.%s)"  emacs-major-version  emacs-minor-version)))

;; TODO insert check for correct version of speedbar


;; Attach .xscala files to the xscala-mode
(add-to-list 'auto-mode-alist '("\\.xscala\\'" . xscala-mode))
(add-to-list 'auto-mode-alist '("\\.sc\\'" . xscala-mode))

(modify-coding-system-alist 'file "\\.xscala$"     'utf-8)
(modify-coding-system-alist 'file "\\.sc$"        'utf-8)

(add-to-list 'auto-mode-alist '("\\.sbt\\'" . xscala-mode))

;; Autoload from xscala-mode.el
(autoload (quote xscala-mode) "xscala-mode" "\
Major mode for editing Scala code.

When started, run `xscala-mode-hook'.

\\{xscala-mode-map}" t nil)


;; Autoload from xscala-mode-inf.el
(autoload (quote ensime-inf-running-p-1) "xscala-mode-inf" nil t nil)

(autoload (quote xscala-run-xscala) "xscala-mode-inf" "Run a Scala interpreter in an Emacs buffer" t nil)

(autoload (quote xscala-switch-to-interpreter) "xscala-mode-inf" "Switch to buffer containing the interpreter" t nil)

(autoload (quote xscala-eval-region) "xscala-mode-inf" "Send current region to Scala interpreter." t nil)
(autoload (quote xscala-eval-definition) "xscala-mode-inf" "Send current definition to Scala interpreter." t nil)

(autoload (quote xscala-eval-buffer) "xscala-mode-inf" "Send whole buffer to Scala interpreter." t nil)

(autoload (quote xscala-load-file) "xscala-mode-inf" "Load a file in the Scala interpreter." t nil)

(autoload (quote xscala-quit-interpreter) "xscala-mode-inf" "Quit Scala interpreter." t nil)


(provide 'xscala-mode-auto)
