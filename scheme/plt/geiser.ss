;;; geiser.ss -- entry point

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sat Apr 25, 2009 22:36

;;; Code:

(require version/utils)
(unless (version<=? "4.2" (version))
  (error 'geiser
         "Mzscheme version 4.2 or better required (found ~a)"
         (version)))

(require geiser/user)

;;; geiser.ss ends here
