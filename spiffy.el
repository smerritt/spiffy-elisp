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

(defun spiffy-is-project-root (directory)
  (file-exists-p (concat (file-name-as-directory directory) ".git")))

; XXX refactor with spiffy-merb-root-dir-for
(defun spiffy-project-root-for (filename)
  (let ((as-dir (file-name-as-directory filename)))
    (if (string= (file-truename as-dir) (file-truename (spiffy-parent-directory as-dir)))
        nil    ; base case
      (if (spiffy-is-project-root as-dir)
          as-dir
        (spiffy-project-root-for (spiffy-parent-directory filename))))))

(defun spiffy-project-files-for (file)
  (filter
   (lambda (path) (not (file-directory-p path)))
   (spiffy-find-interesting-files
    (spiffy-project-root-for file)
    (lambda (f) (and
                 (not (string-match ".git$" f))
                 (not (string-match "gems$" f))
                 (not (backup-file-name-p f)))))))

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

; XXX test me bozo
(defun spiffy-open-file-in-project ()
  (interactive)
  (find-file (spiffy-pick-file-in-project)))

(defun spiffy-pick-file-in-project ()
  (let ((iswitchb-make-buflist-hook
         (lambda ()
           (setq iswitchb-temp-buflist
                 (spiffy-project-files-for (buffer-file-name))))))
    (iswitchb-read-buffer "Open file: ")))

(defmacro spiffy-make-shifty-arrow (outer-function motion-function)
  `(defun ,outer-function ()
     (interactive)
     (unless mark-active
       (push-mark nil t t))    ; silently push a mark and don't whine at the user about it
     (funcall ,motion-function)))
(spiffy-make-shifty-arrow spiffy-arrow-up 'previous-line)
(spiffy-make-shifty-arrow spiffy-arrow-down 'next-line)
(spiffy-make-shifty-arrow spiffy-arrow-right 'forward-char)
(spiffy-make-shifty-arrow spiffy-arrow-left 'backward-char)

(defmacro spiffy-make-delimitizer (function-name left right)
  `(defun ,function-name (&optional start end)
     (interactive (if mark-active (list (region-beginning) (region-end))))
     (if start
         (progn
           (goto-char start)
           (insert ,left)
           (goto-char (1+ end))
           (insert ,right))
       (insert ,left))))

(spiffy-make-delimitizer spiffy-left-paren "(" ")")
(spiffy-make-delimitizer spiffy-left-bracket "[" "]")
(spiffy-make-delimitizer spiffy-left-curly "{" "}")
(spiffy-make-delimitizer spiffy-single-quote "'" "'")
(spiffy-make-delimitizer spiffy-double-quote "\"" "\"")
