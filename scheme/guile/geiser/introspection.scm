;; introspection.scm -- name says it all

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Feb 08, 2009 18:44

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Comentary:

;; Procedures introspecting on scheme objects and their properties.

;;; Code:

(define-module (geiser introspection)
  #:export (proc-args completions)
  #:use-module (system vm program)
  #:use-module (ice-9 session)
  #:use-module (srfi srfi-1))

(define (proc-args proc)
  (let ((proc (and (symbol? proc)
                   (module-bound? (current-module) proc)
                   (eval proc (current-module)))))
    (cond ((not proc) #f)
          ((program? proc) (program-args proc))
          ((procedure? proc) (procedure-args proc))
          ((macro? proc) (macro-args proc))
          (else #f))))

(define (program-args program)
  (let* ((arity (program-arity program))
         (arg-no (first arity))
         (opt (> (second arity) 0))
         (args (map first (take (program-bindings program) arg-no))))
    (format-args (if opt (drop-right args 1) args)
                 (and opt (last args))
                 (program-module program))))

(define (procedure-args proc)
  (let* ((arity (procedure-property proc 'arity))
         (req (first arity))
         (opt (third arity))
         (env (procedure-environment proc)))
    (format-args (map (lambda (n)
                        (string->symbol (format "arg~A" (+ 1 n))))
                      (iota req))
                 (and opt 'rest)
                 (and (not (null? env)) env))))

(define (macro-args macro)
  ;; check if return value is a procedure
  (let ((prog (macro-transformer macro)))
    (if prog
        (program-args prog)
        (format-args '(...) #f #f))))

(define (format-args args opt module)
  (list (cons 'required args)
        (cons 'optional (or opt '()))
        (cons 'module (if module (module-name module) '()))))

(define (completions prefix)
  (sort! (map symbol->string
              (apropos-internal (string-append "^" prefix)))
         string<?))

;;; introspection.scm ends here
