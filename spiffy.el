;; spiffy.el: a bunch of stuff I use in my Ruby+Merb coding.
;; Probably not useful to anyone but me, but hey, give it a try.
;; Copyright 2009 Samuel Merritt <spam at andcheese dot org>
;; Homepage: http://github.com/smerritt/spiffy-elisp/
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; version 2 of the License.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
(provide 'spiffy)

(defun spiffy-cwd ()
  ; pwd returns "Directory /where/you/are/"; this gets rid of the baloney
  (substring (pwd) 10))

(defmacro spiffy-run-in-directory (dir &rest body)
  "Execute code in a particular current working directory"
  (let ((retval-var (make-symbol "retval"))
        (original-dir-var (make-symbol "original-dir")))
    `(let ((,original-dir-var (spiffy-cwd)))
       (unless (null ,dir) (cd ,dir))
       (setq ,retval-var (funcall (lambda () ,@body)))
       (cd ,original-dir-var)
       ,retval-var)))

(defun spiffy-make-shell-command (&rest parts)
  (mapconcat 'shell-quote-argument parts " "))

(defun spiffy-parent-directory (filename)
  (file-name-as-directory (expand-file-name (concat(file-name-as-directory filename) ".."))))

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
    (append (filter interesting-p (list directory))
            (reduce 'append
                    (mapcar (lambda (dir) (spiffy-find-interesting-files dir interesting-p))
                            (filter interesting-p
                                    (mapcar (lambda (filename) (concat (file-name-as-directory directory) filename))
                                            (spiffy-useful-directory-files directory))))))))

(defun spiffy-path-relative-to (basedir filename)
  (replace-regexp-in-string
   (if basedir
       (file-name-as-directory basedir)
     "")
   ""
   filename))

(defun spiffy-start-or-finish-keyboard-macro ()
  (interactive)
  (if defining-kbd-macro
      (kmacro-end-macro nil)
    (kmacro-start-macro nil)))

(defun spiffy-kill-region-or-line ()
  (interactive)
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (kill-line)))