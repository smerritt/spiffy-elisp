(provide 'spiffy)

(defun spiffy-cwd ()
  ; pwd returns "Directory /where/you/are/"; this gets rid of the baloney
  (substring (pwd) 10))

(defmacro spiffy-run-in-directory (dir &rest body)
  "Execute code in a particular current working directory"
  (let ((retval-var (make-symbol "retval"))
        (original-dir-var (make-symbol "original-dir")))
    `(let ((,original-dir-var (spiffy-cwd)))
       (cd ,dir)
       (setq ,retval-var (funcall (lambda () ,@body)))
       (cd ,original-dir-var)
       ,retval-var)))
  
(defun spiffy-spec-binary-to-run-for (filename)
  (let ((merb-root (spiffy-merb-root-dir-for filename)))
    (if merb-root
        (concat (file-name-as-directory merb-root) "bin/spec")
      "spec")))    ; whatever the system's spec binary is

(defun spiffy-make-shell-command (&rest parts)
  (mapconcat 
   (lambda (str) 
     (if (string-match "[\t ]" str)
         (concat "\"" str "\"")
       str))
   parts
   " "))

(defun spiffy-parent-directory (filename)
  (file-name-as-directory (expand-file-name (concat(file-name-as-directory filename) ".."))))

(defun spiffy-is-merb-root-dir (dir)
  (file-exists-p (concat (file-name-as-directory dir) "bin/merb")))

(defun spiffy-merb-root-dir-for (filename)
  (let ((as-dir (file-name-as-directory filename)))
    (if (string= (file-truename as-dir) (file-truename (spiffy-parent-directory as-dir)))
        nil    ; base case
      (if (spiffy-is-merb-root-dir as-dir)
          as-dir
        (spiffy-merb-root-dir-for (spiffy-parent-directory filename))))))

; XXX test me
(defun spiffy-run-spec-under-point ()
  (interactive)
  (spiffy-run-in-directory
   "/Users/sam/awsm"    ; XXX totally punting here too
   (compile
    (spiffy-make-shell-command
     (spiffy-spec-binary-to-run-for (buffer-file-name))
     "-l"
     (format "%d" (line-number-at-pos)) ; defaults to line number at point
     (buffer-file-name)))))
