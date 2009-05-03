;; utils.ss -- generic utilities

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun May 03, 2009 03:09

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

;; Utility procedures

;;; Code:

#lang scheme

(provide module-path-name->name)

(require srfi/13)

(define (module-path-name->name path)
  (if (path? path)
      (let* ((path (path->string path))
             (cpaths (map path->string (current-library-collection-paths)))
             (prefix-len (lambda (p)
                           (let ((pl (string-length p)))
                             (if (= pl (string-prefix-length p path)) pl 0))))
             (lens (map prefix-len cpaths))
             (real-path (substring path (apply max lens))))
        (if (absolute-path? real-path)
            (call-with-values (lambda () (split-path path))
              (lambda (_ basename __) basename))
            (regexp-replace "\\.[^./]*$" real-path "")))
      "<top>"))


;;; utils.ss ends here
