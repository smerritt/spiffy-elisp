(require 'fancy-pants-function-replacement)
(require 'el-expectations)


(defun food () "nuts")

(expectations

  (desc "before any replacements")
  (expect "nuts" (food))          ; sanity check

  (desc "with a replacement")
  (expect "fruits"
    (with-temporary-function-replacement
     (food (lambda () "fruits"))
     (food)))
  
  (desc "after a replacement")
  (expect "nuts" (food))

  (desc "error out on unbound function")
  (expect (error)
    (with-temporary-function-replacement
     (symbol-not-bound-to-a-function (lambda () "whee"))
     (symbol-not-bound-to-a-function)))
                                     
)
