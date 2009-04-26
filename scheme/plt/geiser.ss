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
           geiser/autodoc
           geiser/format-error
           geiser/set-result!
           geiser/make-repl-reader)

  (compile-enforce-module-constants #f)
  (require scheme/enter scheme/string scheme/port)

  (define (ensure-module spec)
    (cond ((symbol? spec) spec)
          ((not (string? spec)) #f)
          ((not (file-exists? spec)) #f)
          ((absolute-path? spec) `(file ,spec))
          (else spec)))

  (define (exn-key e)
    (vector-ref (struct->vector e) 0))

  (define last-result (void))

  (define (geiser/format-error e)
    (set! last-result `((error (key . ,(exn-key e))
                               (subr)
                               (msg . ,(exn-message e))))))
  (define (geiser/set-result! v)
    (set! last-result `((result  ,v))))

  (define nowhere (open-output-nowhere))

  (define (geiser/eval form spec)
    (geiser/set-result! (void))
    (with-handlers ((exn? geiser/format-error))
      (parameterize ((current-error-port nowhere))
        (eval #`(enter! #,(ensure-module spec))))
      ((dynamic-require ''geiser 'geiser/set-result!) (eval form)))
    (enter! #f)
    last-result)

  (define geiser/compile geiser/eval)
  (define (geiser/autodoc . x) #f)

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
