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

(require version/utils)
(unless (version<=? "4.1.5.5" (version))
  (error 'geiser
         "Mzscheme version 4.1.5.5 or better required (found ~a)"
         (version)))

(module geiser scheme
  (provide geiser:eval
           geiser:compile
           geiser:load-file
           geiser:compile-file
           geiser:macroexpand
           geiser:completions
           geiser:module-completions
           geiser:symbol-location
           geiser:module-location
           geiser:autodoc)

  (compile-enforce-module-constants #f)
  (require geiser/eval
           geiser/modules
           geiser/completions
           geiser/locations
           geiser/autodoc)

  (define geiser:eval eval-in)
  (define geiser:compile compile-in)
  (define geiser:load-file load-file)
  (define geiser:compile-file compile-file)
  (define geiser:autodoc autodoc)
  (define geiser:completions symbol-completions)
  (define geiser:module-completions module-completions)
  (define geiser:symbol-location symbol-location)
  (define geiser:module-location module-location)
  (define geiser:macroexpand macroexpand)

  (current-prompt-read (compose (make-repl-reader (current-prompt-read))
                                current-namespace)))

(require 'geiser)

;;; geiser.ss ends here
