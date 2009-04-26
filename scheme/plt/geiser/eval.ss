;; eval.ss -- evaluation

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Apr 26, 2009 00:44

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

;; Evaluation functions

;;; Code:

#lang scheme

(provide eval-in compile-in
         load-file compile-file)

(require scheme/enter)

(define last-result (void))
(define nowhere (open-output-nowhere))

(define (ensure-spec spec)
  (cond ((symbol? spec) spec)
        ((not (string? spec)) #f)
        ((not (file-exists? spec)) #f)
        ((absolute-path? spec) `(file ,spec))
        (else spec)))

(define (load-module spec . port)
  (parameterize ((current-error-port (if (null? port) nowhere (car port))))
    (eval #`(enter! #,spec)))
  (enter! #f))

(define (ensure-namespace mod-spec)
  (letrec ((spec (ensure-spec mod-spec))
           (handler (lambda (e)
                      (load-module spec)
                      (module->namespace spec))))
    (if spec
        (with-handlers ((exn:fail:contract? handler))
          (module->namespace spec))
        (current-namespace))))

(define (exn-key e)
  (vector-ref (struct->vector e) 0))

(define (set-last-error e)
  (set! last-result `((error (key . ,(exn-key e))
                             (subr)
                             (msg . ,(exn-message e))))))
(define (set-last-result v)
  (set! last-result `((result  ,v))))

(define (eval-in form spec)
  (set-last-result (void))
  (with-handlers ((exn? set-last-error))
    (set-last-result (eval form (ensure-namespace spec))))
  last-result)

(define compile-in eval-in)

(define (load-file file)
  (with-handlers ((exn? set-last-error))
    (set-last-result
     (string-append (with-output-to-string
                      (lambda ()
                        (load-module (ensure-spec file) (current-output-port))))
                    "done.")))
  last-result)

(define compile-file load-file)

;;; eval.ss ends here
