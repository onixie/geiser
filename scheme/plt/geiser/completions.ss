;; completions.ss -- completion support

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Apr 26, 2009 19:02

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

;; Finding completions of given prefixes.

;;; Code:

#lang scheme

(provide symbol-completions
         module-completions)

(require srfi/13 geiser/utils geiser/modules)

(define (filter-prefix prefix lst sort?)
  (filter (lambda (s) (string-prefix? prefix s))
          (if sort? (sort lst string<?) lst)))

(define (symbol-completions prefix)
  (filter-prefix prefix
                 (map symbol->string (namespace-mapped-symbols))
                 #t))

(define (module-completions prefix)
  (filter-prefix prefix (module-list) #f))

;;; completions.ss ends here
