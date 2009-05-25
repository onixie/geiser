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

(provide autodoc update-module-cache get-help)

(require geiser/utils geiser/modules geiser/locations scheme/help)

(define (get-help symbol mod)
  (with-handlers ((exn? (lambda (e)
                          (eval `(help ,symbol #:from ,mod)))))
    (eval `(help ,symbol))))

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
         (sgn (and path (find-signature path name fun))))
    (and sgn
         (list (cons 'signature (format-signature fun sgn))
               (cons 'position (find-position sgn form))
               (cons 'module (module-path-name->name path))))))

(define signatures (make-hash))

(define-struct signature (required optional keys rest))

(define (find-signature path name local-name)
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
    ((list 'define (list (list name formals ...) other ...) body ...)
     (add-signature! name formals store))
    ((list 'define name (list 'lambda formals body ...))
     (add-signature! name formals store))
    ((list 'define (list name formals ...) body ...)
     (add-signature! name formals store))
    ((list 'define-for-syntax (list name formals ...) body ...)
     (add-signature! name formals store))
    ((list 'define-for-syntax name (list 'lambda formals body ...))
     (add-signature! name formals store))
    ((list 'define-syntax-rule (list name formals ...) body ...)
     (add-signature! name formals store))
    (_ void)))

(define (add-signature! name formals store)
  (hash-set! store name (parse-formals formals)))

(define (parse-formals formals)
  (let loop ((formals formals) (req '()) (opt '()) (keys '()))
    (cond ((null? formals)
           (make-signature (reverse req) (reverse opt) (reverse keys) #f))
          ((symbol? formals)
           (make-signature (reverse req) (reverse opt) (reverse keys) formals))
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

(define (infer-signature name)
  (define syntax-tag (cons 1 0))
  (define error-tag (cons 1 1))
  (define generic-signature (make-signature '(...) '() '() #f))
  (let ((value (with-handlers ((exn:fail:syntax? (lambda (_) syntax-tag))
                               (exn:fail:contract:variable? (lambda (_) error-tag)))
                 (namespace-variable-value name))))
    (cond ((procedure? value)
           (arity->signature (procedure-arity value)))
          ((eq? value syntax-tag) generic-signature)
          ((eq? value error-tag) #f)
          (else 'variable))))

(define (arity->signature arity)
  (define (args fst count)
    (let* ((letts (list->vector '(#\x #\y #\z #\u #\v #\w #\r #\s)))
           (len (vector-length letts))
           (lett (lambda (n) (vector-ref letts (modulo n len)))))
      (map (lambda (n) (string->symbol (format "~a" (lett n))))
           (build-list count (lambda (n) (+ n fst))))))
  (cond ((number? arity)
         (make-signature (args 0 arity) '() '() #f))
        ((arity-at-least? arity)
         (make-signature (args 0 (arity-at-least-value arity)) '() '() 'rest))
        (else
         (let* ((arg-nos (map (lambda (a)
                                (if (number? a) a (arity-at-least-value a)))
                              arity))
                (min-val (apply min arg-nos))
                (max-val (apply max arg-nos))
                (opt-no (- max-val min-val)))
           (make-signature (args 0 min-val) (args min-val opt-no) '() #f)))))

(define (format-signature fun sign)
 (cond ((symbol? sign) (cons fun sign))
       ((signature? sign)
        (let ((req (signature-required sign))
              (opt (signature-optional sign))
              (keys (signature-keys sign))
              (rest (signature-rest sign)))
          `(,fun
            ,@req
            ,@(if (null? opt) opt (cons '#:opt opt))
            ,@(if (null? keys) keys (cons '#:key keys))
            ,@(if rest (list '#:rest rest) '()))))
       (else #f)))

(define (find-position sign form)
  (if (signature? sign)
      (let* ((lf (length form))
             (lf-1 (- lf 1)))
        (if (= 1 lf) 0
            (let ((req (length (signature-required sign)))
                  (opt (length (signature-optional sign)))
                  (keys (map (lambda (k) (symbol->keyword (if (list? k) (car k) k)))
                             (signature-keys sign)))
                  (rest (signature-rest sign)))
              (cond ((<= lf (+ 1 req)) lf-1)
                    ((<= lf (+ 1 req opt)) (if (> opt 0) lf lf-1))
                    ((or (memq (last form) keys)
                         (memq (car (take-right form 2)) keys)) =>
                         (lambda (sl)
                           (+ 2 req
                              (if (> opt 0) (+ 1 opt) 0)
                              (- (length keys) (length sl)))))
                    (else (+ 1 req
                             (if (> opt 0) (+ 1 opt) 0)
                             (if (null? keys) 0 (+ 1 (length keys)))
                             (if rest 2 0)))))))
      0))

(define (update-module-cache path . form)
  (when (and (string? path)
             (or (null? form)
                 (and (list? (car form))
                      (not (null? (car form)))
                      (memq (caar form)
                            '(define-syntax-rule
                               define-syntax define set! define-struct)))))
    (hash-remove! signatures path)))

;;; autodoc.ss ends here