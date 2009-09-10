;;; autodoc.ss -- suport for autodoc echo

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sun May 03, 2009 14:45

#lang scheme

(provide autodoc update-module-cache get-help)

(require geiser/utils geiser/modules geiser/locations scheme/help)

(define (get-help symbol mod)
  (with-handlers ((exn? (lambda (e)
                          (eval `(help ,symbol #:from ,mod)))))
    (eval `(help ,symbol))))

(define (autodoc ids)
  (if (not (list? ids))
      '()
      (map (lambda (id) (or (autodoc* id) (list id))) ids)))

(define (autodoc* id)
  (and
   (symbol? id)
   (let* ((loc (symbol-location* id))
          (name (car loc))
          (path (cdr loc))
          (sgn (and path (find-signature path name id))))
     (and sgn
          `(,id
            (name . ,name)
            (args ,@(format-signature sgn))
            (module . ,(module-path-name->name path)))))))

(define (format-signature sign)
  (if (signature? sign)
      `((required ,@(signature-required sign))
        (optional ,@(signature-optional sign)
                  ,@(let ((rest (signature-rest sign)))
                      (if rest (list "...") '())))
        (key ,@(signature-keys sign)))
      '()))

(define signatures (make-hash))

(define-struct signature (required optional keys rest))

(define (find-signature path name local-name)
  (let ((path (if (path? path) (path->string path) path)))
    (hash-ref! (hash-ref! signatures path (lambda () (parse-signatures path)))
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
