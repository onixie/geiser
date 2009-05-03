;; locations.ss -- locating symbols

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Apr 26, 2009 19:43

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

;; Finding symbol location

;;; Code:

#lang scheme

(provide symbol-location
         symbol-module-name
         symbol-module-path-name)

(require geiser/utils)

(define (%symbol-location sym)
  (let* ((id (namespace-symbol->identifier sym))
         (binding (and id (identifier-binding id))))
    (if (list? binding)
        (cons
         (cadr binding)
         (resolved-module-path-name
          (module-path-index-resolve (car binding))))
        (cons sym #f))))

(define (symbol-location sym)
  (let* ((loc (%symbol-location sym))
         (name (car loc))
         (path (cdr loc)))
    (list (cons 'name name)
          (cons 'file (if (path? path) (path->string path) '())))))

(define symbol-module-path-name (compose cdr %symbol-location))

(define symbol-module-name
  (compose module-path-name->name symbol-module-path-name))

;;; locations.ss ends here
