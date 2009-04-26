;; geiser.ss -- top level entry point

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Apr 25, 2009 22:36

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

;; Top level REPL definitions for Geiser.

;;; Code:

(module geiser scheme
  (provide geiser/eval
           geiser/compile
           geiser/load-file
           geiser/compile-file
           geiser/completions
           geiser/symbol-location
           geiser/autodoc
           geiser/make-repl-reader)

  (compile-enforce-module-constants #f)
  (require (lib "geiser/eval")
           (lib "geiser/completions")
           (lib "geiser/locations"))

  (define geiser/eval eval-in)
  (define geiser/compile compile-in)
  (define geiser/load-file load-file)
  (define geiser/compile-file compile-file)
  (define (geiser/autodoc . x) #f)
  (define geiser/completions completions)
  (define geiser/symbol-location symbol-location)

  (define prompt (make-parameter "mzscheme@(geiser)"))
  (define (geiser/make-repl-reader builtin-reader)
    (lambda ()
      (display (prompt))
      (builtin-reader))))

(require 'geiser)

(current-prompt-read
 (let ([old (current-prompt-read)])
   (lambda ()
     (current-prompt-read
      ((dynamic-require ''geiser 'geiser/make-repl-reader) old)))))


;;; geiser.ss ends here
