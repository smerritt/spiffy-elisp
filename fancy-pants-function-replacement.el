(provide 'fancy-pants-function-replacement)


(defmacro with-temporary-function-replacement (spec &rest body)
  (let ((retval-var (make-symbol "retval")))
  `(let
       ; XXX clean up these leaky local variables (after it works!)
       ((funcsym (car ',spec))       ; XXX duplication: "(car ',spec)"
        (funcbody (cdr ',spec))
        (original-function (symbol-function (car ',spec))))
     (ad-safe-fset 
      funcsym 
      (if (symbolp funcbody)
          (symbol-function funcbody)
        (car funcbody)))
     (setq ,retval-var (funcall (lambda () ,@body)))
     (ad-safe-fset funcsym original-function)
     ,retval-var)))
  
  

