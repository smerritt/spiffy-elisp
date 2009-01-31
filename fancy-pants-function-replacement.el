;; fancy-pants-function-replacement, a pale shadow of perl's Test::Resub.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; version 2 of the License.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
(provide 'fancy-pants-function-replacement)

(defmacro with-temporary-function-replacement (spec &rest body)
  (let ((retval-var (make-symbol "retval"))
        (funcsym-var (make-symbol "funcsym"))
        (funcbody-var (make-symbol "funcbody"))
        (original-function-var (make-symbol "original-function")))
  `(let*
       ((,funcsym-var (car ',spec))
        (,funcbody-var (cdr ',spec))
        (,original-function-var (symbol-function ,funcsym-var)))
     (ad-safe-fset
      ,funcsym-var
      (if (symbolp ,funcbody-var)
          (symbol-function ,funcbody-var)
        (car ,funcbody-var)))
     (setq ,retval-var (funcall (lambda () ,@body)))
     (ad-safe-fset ,funcsym-var ,original-function-var)
     ,retval-var)))
  
  

