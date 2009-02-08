;; eval.scm -- evaluation procedures

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Fri Feb 06, 2009 22:54

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

;; Module defining evaluation procedures called from the Emacs side.

;;; Code:

(define-module (geiser eval)
  #:export (eval-in))

(define (eval-in form module-name)
  "Evals FORM in the module designated by MODULE-NAME.
If MODULE-NAME is #f or resolution fails, the current module is used instead.
The result is a list of the form ((RESULT . <form-value>))
if no evaluation error happens, or ((ERROR <error-key> <error-arg>...))
in case of errors."
  (let ((module (or (and module-name (resolve-module module-name))
                    (current-module))))
    (catch #t
      (lambda () (list (cons 'result (eval form module))))
      (lambda (key . args) (list (cons 'error (cons key args)))))))

;;; eval.scm ends here
