;;; modules.ss -- module metadata

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Wed May 06, 2009 02:35

#lang scheme

(provide load-module
         module-spec->namespace
         namespace->module-path-name
         module-path-name->name
         module-spec->path-name
         module-list
         module-exports)

(require srfi/13 scheme/enter syntax/modresolve syntax/modcode)

(define (ensure-module-spec spec)
  (cond ((symbol? spec) spec)
        ((not (string? spec)) #f)
        (else `(file ,spec))))

(define (module-spec->namespace spec lang)
  (let* ((spec (ensure-module-spec spec))
         (try-lang (lambda (e)
                     (if (symbol? lang)
                         (begin
                           (load-module lang #f (current-namespace))
                           (module->namespace lang))
                         (current-namespace))))
         (filesystem-handler (lambda (e)
                               (with-handlers ((exn? try-lang))
                                 (module->namespace `',spec)))))
    (if spec
        (with-handlers ((exn:fail:filesystem? filesystem-handler)
                        (exn? try-lang))
          (module->namespace spec))
        (current-namespace))))

(define nowhere (open-output-nowhere))

(define (load-module spec (port #f) (ns #f))
  (parameterize ((current-error-port (or port nowhere)))
    (eval #`(enter! #,(ensure-module-spec spec)))
    (when (namespace? ns)
      (current-namespace ns))))

(define (namespace->module-path-name ns)
  (let ((rmp (variable-reference->resolved-module-path
              (eval '(#%variable-reference) ns))))
    (and (resolved-module-path? rmp)
         (resolved-module-path-name rmp))))

(define (module-spec->path-name spec)
  (with-handlers ((exn? (lambda (_) #f)))
    (let ((ns (module-spec->namespace (ensure-module-spec spec))))
      (namespace->module-path-name ns))))

(define (module-path-name->name path)
  (cond ((path? path)
         (let* ((path (path->string path))
                (cpaths (map (compose path->string path->directory-path)
                             (current-library-collection-paths)))
                (prefix-len (lambda (p)
                              (let ((pl (string-length p)))
                                (if (= pl (string-prefix-length p path))
                                    pl
                                    0))))
                (lens (map prefix-len cpaths))
                (real-path (substring path (apply max lens))))
           (if (absolute-path? real-path)
               (call-with-values (lambda () (split-path path))
                 (lambda (_ basename __) (path->string basename)))
               (regexp-replace "\\.[^./]*$" real-path ""))))
        ((eq? path '#%kernel) "(kernel)")
        ((string? path) path)
        ((symbol? path) (symbol->string path))
        (else "")))


(define (skippable-dir? path)
  (call-with-values (lambda () (split-path path))
    (lambda (_ basename __)
      (member (path->string basename) '(".svn" "compiled")))))

(define path->symbol (compose string->symbol path->string))

(define (path->entry path)
  (and (bytes=? (or (filename-extension path) #"") #"ss")
       (let ((path (path->string path)))
         (substring path 0 (- (string-length path) 3)))))

(define (visit-module-path path kind acc)
  (case kind
    ((file) (let ((entry (path->entry path)))
              (if entry (cons entry acc) acc)))
    ((dir) (cond ((skippable-dir? path) (values acc #f))
                 ((file-exists? (build-path path "main.ss"))
                  (cons (path->string path) acc))
                 (else acc)))
    (else acc)))

(define (find-modules path acc)
  (if (directory-exists? path)
      (parameterize ((current-directory path))
        (fold-files visit-module-path acc))
      acc))

(define module-cache #f)

(define (module-list)
  (when (not module-cache)
    (set! module-cache
          (sort (foldl find-modules '() (current-library-collection-paths))
                string<?)))
  module-cache)

(define (module-exports mod)
  (define (extract-ids ls)
    (append-map (lambda (idls)
                  (map car (cdr idls)))
                ls))
  (define (classify-ids ids ns)
    (let loop ((ids ids) (procs '()) (vars '()))
      (cond ((null? ids) `((procs ,@(reverse procs)) (vars ,@(reverse vars))))
            ((procedure? (namespace-variable-value (car ids) #t (lambda () #f) ns))
             (loop (cdr ids) (cons (car ids) procs) vars))
            (else (loop (cdr ids) procs (cons (car ids) vars))))))
  (let-values (((reg syn)
                (module-compiled-exports
                 (get-module-code (resolve-module-path mod #f)))))
    (let ((syn (extract-ids syn))
          (reg (extract-ids reg)))
      `((syntax ,@syn) ,@(classify-ids reg (module-spec->namespace mod))))))

;;; modules.ss ends here
