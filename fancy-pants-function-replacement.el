(provide 'fancy-pants-function-replacement)


(defmacro with-temporary-function-replacement (spec &rest body)
  (let ((retval-var (make-symbol "retval")))
  `(let
       ; XXX clean up these leaky local variables (after it works!)
       ((funcsym (car ',spec))       ; XXX duplication: "(car ',spec)"
        (funcbody (cdr ',spec))
        (original-function (symbol-function (car ',spec))))
;;     (debug)
     (ad-safe-fset funcsym (car funcbody))  ;;; XXX this car is weird
     (setq ,retval-var (funcall (lambda () ,@body)))
     (ad-safe-fset funcsym original-function)
     ;; put it back here
     ,retval-var)))
  
  

