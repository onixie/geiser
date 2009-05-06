;; modules.ss -- module metadata

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Wed May 06, 2009 02:35

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

(provide load-module
         module-spec->namespace
         namespace->module-path-name
         module-path-name->name
         module-spec->path-name)

(require srfi/13 scheme/enter)

(define nowhere (open-output-nowhere))

(define (ensure-module-spec spec)
  (cond ((symbol? spec) spec)
        ((not (string? spec)) #f)
        ((not (file-exists? spec)) #f)
        ((absolute-path? spec) `(file ,spec))
        (else spec)))

(define (module-spec->namespace mod-spec)
  (letrec ((spec (ensure-module-spec mod-spec))
           (handler (lambda (e)
                      (load-module spec)
                      (enter! #f)
                      (module->namespace spec))))
    (if spec
        (with-handlers ((exn:fail:contract? handler))
          (module->namespace spec))
        (current-namespace))))

(define (load-module spec . port)
  (parameterize ((current-error-port (if (null? port) nowhere (car port))))
    (eval #`(enter! #,(ensure-module-spec spec)))))

(define (namespace->module-path-name ns)
  (let ((rmp (variable-reference->resolved-module-path
              (eval '(#%variable-reference) ns))))
    (and (resolved-module-path? rmp)
         (resolved-module-path-name rmp))))

(define (module-spec->path-name spec)
  (with-handlers ((exn? (lambda (_) #f)))
    (let ((ns (module->namespace (ensure-module-spec spec))))
      (namespace->module-path-name ns))))

(define (module-path-name->name path)
  (cond ((path? path)
         (let* ((path (path->string path))
                (cpaths (map (compose path->string path->directory-path)
                             (current-library-collection-paths)))
                (prefix-len (lambda (p)
                              (let ((pl (string-length p)))
                                (if (= pl (string-prefix-length p path)) pl 0))))
                (lens (map prefix-len cpaths))
                (real-path (substring path (apply max lens))))
           (if (absolute-path? real-path)
               (call-with-values (lambda () (split-path path))
                 (lambda (_ basename __) (path->string basename)))
               (regexp-replace "\\.[^./]*$" real-path ""))))
        ((eq? path '#%kernel) "(kernel)")
        ((string? path) path)
        ((symbol? path) (symbol->string path))
        (else "<top>")))

;;; modules.ss ends here
