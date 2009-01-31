;; Tests for fancy-pants-function-replacement, a pale shadow of perl's Test::Resub.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; version 2 of the License.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
(require 'fancy-pants-function-replacement)
(require 'el-expectations)

(defun food () "nuts")
(defun beer () "IPA")

(expectations

  (desc "before any replacements (sanity check)")
  (expect "nuts" (food))

  (desc "within a replacement, the function changes")
  (expect "fruits"
    (with-temporary-function-replacement
     (food (lambda () "fruits"))
     (food)))
  
  (desc "after a replacement, the function goes back")
  (expect "nuts" (food))

  (desc "replacement function's signature differs from the original function's")
  (expect "pizza and beer"
    (with-temporary-function-replacement
     (food (lambda (basefood) (concat basefood " and beer")))
     (food "pizza")))

  (desc "replacement spec can take a symbol as well as a lambda")
  ; so you don't have to define your huge crappy test functions inline
  (expect "IPA"
    (with-temporary-function-replacement
     ; this interface seems a little strange... I'd want (food 'beer), I think.
     ; however, for consistency, it should be (food beer) or ('food 'beer).
     ; not sure what to do here...
     (food beer)
     (food)))

  (desc "error out on unbound function")
  (expect (error)
    (with-temporary-function-replacement
     (symbol-not-bound-to-a-function (lambda () "whee"))
     (symbol-not-bound-to-a-function)))
                                     
)
