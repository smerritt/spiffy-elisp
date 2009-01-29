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
  (let ((merb-root (spiffy-merb-root-for filename)))
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

(defun spiffy-is-merb-root (dir)
  (file-exists-p (concat (file-name-as-directory dir) "bin/merb")))

(defun spiffy-merb-root-for (filename)
  (let ((as-dir (file-name-as-directory filename)))
    (if (string= (file-truename as-dir) (file-truename (spiffy-parent-directory as-dir)))
        nil    ; base case
      (if (spiffy-is-merb-root as-dir)
          as-dir
        (spiffy-merb-root-for (spiffy-parent-directory filename))))))

; holy fucking crap
; it's 2009
; where is my tail-call optimization?
(defun filter (predicate l)
  (let ((result '()))
    (while (not (null l))
      (if (funcall predicate (car l))
          (setq result (append result (list (car l)))))
      (setq l (cdr l)))
    result))

(defun spiffy-useful-directory-files (directory)
  (filter
   (lambda (filename) (and (not (string= filename ".")) (not (string= filename ".."))))
   (directory-files directory)))

(defun spiffy-find-interesting-files (directory interesting-p)
  (if (not (file-directory-p directory))
      (filter interesting-p (list directory))
    (let ((files (filter interesting-p (list directory))))
      ; XXX there's a reduce function in cl-extra... use it instead of mapcar+setq
      (mapcar (lambda (subdir-files) (setq files (append files subdir-files)))
              (mapcar (lambda (dir) (spiffy-find-interesting-files dir interesting-p))
                      (filter interesting-p
                              (mapcar (lambda (filename) (concat (file-name-as-directory directory) filename))
                                      (spiffy-useful-directory-files directory)))))
    files)))

; XXX refactor with spiffy-merb-root-dir-for

; XXX test me
(defun spiffy-run-spec-under-point ()
  (interactive)
  (save-buffer)
  (spiffy-run-in-directory
   (spiffy-merb-root-for (buffer-file-name))
   (compile
    (spiffy-make-shell-command
     (spiffy-spec-binary-to-run-for (buffer-file-name))
     "-l"
     (format "%d" (line-number-at-pos)) ; defaults to line number at point
     (buffer-file-name)))))

; XXX test + refactor with spiffy-run-spec-under-point
(defun spiffy-run-spec-file ()
  (interactive)
  (save-buffer)
  (spiffy-run-in-directory
   (spiffy-merb-root-for (buffer-file-name))
   (compile
    (spiffy-make-shell-command
     (spiffy-spec-binary-to-run-for (buffer-file-name))
     (buffer-file-name)))))

