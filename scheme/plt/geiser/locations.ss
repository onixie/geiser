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

(provide symbol-location)

(define (%symbol-location sym)
  (let ([binding (identifier-binding sym)])
    (and (list? binding)
         (resolved-module-path-name
          (module-path-index-resolve (car binding))))))

(define (symbol-location sym)
  (let ((file (%symbol-location (namespace-symbol->identifier sym))))
    (list (cons 'file (if (path? file) (path->string file) '())))))

;;; locations.ss ends here
