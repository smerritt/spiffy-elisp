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
  
  

