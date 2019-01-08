;;; -*-Emacs-Lisp-*-
;;; xscala-mode-feature.el - 

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

(provide 'xscala-mode-variables)

;; Feature specific variables that need to be shared!

; define xscala-mode-hook
(defvar xscala-mode-hook nil
  "Hook to run after installing xscala mode")

(defvar xscala-interpreter "xscala"
  "The interpreter that `run-xscala' should run. This should
 be a program in your PATH or the full pathname of the xscala interpreter.")

(defvar xscala-args ""
  "The arguments for the `run-xscala' should run.")

(defcustom xscala-std-interpreter "xscala"
  "The interpreter that `run-xscala' should run. This should
 be a program in your PATH or the full pathname of the xscala interpreter."
  :type 'string
  :group 'xscala-mode-inf)

(defcustom xscala-spark-interpreter "spark-shell"
  "The interpreter that `run-xscala' should run to use a Spark cluster. This should
 be a program in your PATH or the full pathname of the Spark interpreter."
  :type 'string
  :group 'xscala-mode-inf)

(defcustom xscala-std-options nil
  "*List of Scala interpreter options."
  :type '(repeat string)
  :group 'xscala-mode-inf)

(defcustom xscala-spark-options nil
  "*List of Spark Scala interpreter options."
  :type '(repeat string)
  :group 'xscala-mode-inf)

(defgroup xscala-mode-inf nil
  "Mode to interact with a Scala interpreter."
  :group 'xscala
  :tag "Inferior Scala")

(defcustom scala-edit-mark "// #mark"
  "String to insert."
  :type 'string
  :group 'xscala-mode-inf)

(defcustom scala-edit-mark-re nil
  "Regular expression for a line to mark the end of a block to send to the interpreter. Derived from `scala-edit-mark' by prefixing with ^."
  :type 'string
  :group 'xscala-mode-inf)

