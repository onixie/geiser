;; autodoc.ss -- support for autodoc

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun May 03, 2009 14:45

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

;;; Code:

#lang scheme

(provide autodoc update-module-cache)

(require geiser/utils geiser/locations)

(define (autodoc form)
  (cond ((null? form) #f)
        ((symbol? form) (describe-application (list form)))
        ((not (pair? form)) #f)
        ((not (list? form)) (autodoc (pair->list form)))
        ((define-head? form) => autodoc)
        (else (autodoc/list form))))

(define (autodoc/list form)
  (let ((lst (last form)))
    (cond ((and (symbol? lst) (describe-application (list lst))))
          ((and (pair? lst) (not (memq (car lst) '(quote))) (autodoc lst)))
          (else (describe-application form)))))

(define (define-head? form)
  (define defforms '(-define
                     define define-values
                     define-method define-class define-generic define-struct
                     define-syntax define-syntaxes -define-syntax))
  (and (= 2 (length form))
       (memq (car form) defforms)
       (car form)))

(define (describe-application form)
  (let* ((fun (car form))
         (loc (symbol-location* fun))
         (name (car loc))
         (path (cdr loc))
         (sgn (and path (signature path name fun))))
    (and sgn
         (list (cons 'signature (cons fun sgn))
               (cons 'position 0)
               (cons 'module (module-path-name->name path))))))

(define signatures (make-hash))

(define (signature path name local-name)
  (let ((path (if (path? path) (path->string path) path)))
    (hash-ref! (hash-ref! signatures
                          path
                          (lambda () (parse-signatures path)))
               name
               (lambda () (infer-signature local-name)))))

(define (parse-signatures path)
  (let ((result (make-hasheq)))
    (with-handlers ((exn? (lambda (e) result)))
      (with-input-from-file path
        (lambda ()
          (parameterize ((read-accept-reader #t))
            (let loop ((stx (read-syntax path)))
              (cond ((eof-object? stx) void)
                    ((syntax->datum stx) =>
                     (lambda (datum)
                       (parse-datum! datum result)
                       (loop (read-syntax path))))
                    (else void)))))))
    result))

(define (parse-datum! datum store)
  (match datum
    ((list 'module name lang forms ...)
     (for-each (lambda (f) (parse-datum! f store)) forms))
    ((list 'define (list name formals ...) body ...)
     (add-signature! name formals store))
    ((list 'define name (list 'lambda formals body ...))
     (add-signature! name formals store))
    (_ void)))

(define (add-signature! name formals store)
  (hash-set! store name (parse-formals formals)))

(define (parse-formals formals)
  (let loop ((formals formals) (req '()) (opt '()) (keys '()))
    (cond ((null? formals) (make-signature req opt keys #f))
          ((symbol? formals) (make-signature req opt keys formals))
          ((pair? (car formals)) (loop (cdr formals)
                                       req
                                       (cons (car formals) opt)
                                       keys))
          ((keyword? (car formals)) (let* ((kname (keyword->symbol (car formals)))
                                           (arg-id (cadr formals))
                                           (name (if (pair? arg-id)
                                                     (list kname (cadr arg-id))
                                                     kname)))
                                      (loop (cddr formals)
                                            req
                                            opt
                                            (cons name keys))))
          (else (loop (cdr formals) (cons (car formals) req) opt keys)))))

(define (make-signature req opt keys rest)
  `(,@(reverse req)
    ,@(if (null? opt) opt
          (cons '#:opt (reverse opt)))
    ,@(if (null? keys) keys
          (cons '#:key (reverse keys)))
    ,@(if rest (list '#:rest rest) '())))

(define (update-module-cache path . form)
  (when (and (string? path)
             (or (null? form)
                 (and (list? (car form))
                      (not (null? (car form)))
                      (memq (caar form) '(define)))))
    (hash-remove! signatures path)))

(define (infer-signature name)
  (let ((value (namespace-variable-value name (lambda () #f))))
    (and (procedure? value)
         (arity->signature (procedure-arity value)))))

(define (arity->signature arity)
  (cond ((number? arity)
         (make-signature (gen-arg-names 1 arity) '() '() #f))
        ((arity-at-least? arity)
         (make-signature (gen-arg-names 1 (arity-at-least-value arity))
                         '() '() 'rest))
        (else
         (let ((arg (map (lambda (a)
                           (if (number? a) a (list (arity-at-least-value a) '...)))
                         arity)))
           (make-signature (list arg) '() '() #f)))))

(define (gen-arg-names fst count)
  (let* ((letts (list->vector '(#\x #\y #\z #\u #\v #\w #\t)))
         (len (vector-length letts))
         (lett (lambda (n) (vector-ref letts (modulo n len)))))
    (reverse (map (lambda (n) (string->symbol (format "~a" (lett (- n 1)))))
                  (build-list (max count 1) (lambda (n) (+ n fst)))))))

;;; autodoc.ss ends here
