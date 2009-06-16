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
   (lambda (filename) (and
                       (not (string= filename "."))
                       (not (string= filename ".."))))
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

(defun spiffy-buffer-or-temp-file-name ()
  (or (buffer-file-name)
      ; XXX use file-local-copy
      (let ((tempfile (make-temp-file "/tmp/spiffy"))
            (buffer-contents (buffer-string)))
        (with-current-buffer (find-file-noselect tempfile)
          (insert buffer-contents)
          (save-buffer 0)               ; no backup file for my temp file, thanks
          (buffer-file-name)))))

(defun spiffy-start-or-finish-keyboard-macro ()
  (interactive)
  (if defining-kbd-macro
      (kmacro-end-macro nil)
    (kmacro-start-macro nil)))

(defun spiffy-kill-region-or-line (&optional arg)
  (interactive "P")
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (kill-line arg)))

(defun spiffy-random-ndigit (digits)
  (let* ((min (expt 10 (1- digits)))
         (max (1- (expt 10 digits))))
    (+ min
       (random (- max min)))))

(defun spiffy-downfrom (n)
  "Generate the list (n n-1 n-2 ... 1).
With negative argument n, returns the list (n)."
  (cond
   ((<= n 1) (list n))
   (t (cons n (spiffy-downfrom (1- n))))))


(defun spiffy-random-ip ()
  "Generate a random IPv4 address in dotted-quad format."
  (mapconcat
   (lambda (dontcare) (int-to-string (1+ (random 254))))
   (spiffy-downfrom 4)                  ; or any list of length 4
   "."))

(defun spiffy-insert-random-ip ()
  "Insert a random IPv4 address in dotted-quad format."
  (interactive)
  (insert (spiffy-random-ip)))

(defun spiffy-insert-random-number (&optional digits)
  (interactive "P")                     ; not "p"; that defaults to 1
  (insert (int-to-string (spiffy-random-ndigit (or digits 5)))))

;; This keymap is composed of just the mnemonics I came up with.
;; The only reason it's defined here is because I find these easier
;; than remembering all the function names. There's no particular
;; organization here.
(defvar *spiffy-keymap* (make-sparse-keymap))
(define-key *spiffy-keymap* [(control ?\;) ?r ?i] 'spiffy-insert-random-ip)
(define-key *spiffy-keymap* [(control ?\;) ?r ?n] 'spiffy-insert-random-number)
(define-key *spiffy-keymap* [(control k)] 'spiffy-kill-region-or-line) ;; improves on kill-line
(define-key *spiffy-keymap* [(control ?\')] 'spiffy-start-or-finish-keyboard-macro)
(define-key *spiffy-keymap* [(meta ?\')] 'call-last-kbd-macro) ;; clobbers abbrev-prefix-mark
(define-key *spiffy-keymap* [(super ?\')] 'edit-last-kbd-macro)

(defvar spiffy-enable-minor-mode nil
  "Whether or not to enable the minor mode in a new buffer by default.
Default is nil.

To use, (setq spiffy-enable-minor-mode t) before requiring 'spiffy.")

(define-minor-mode spiffy-mode
  "Spiffy minor mode. Stuff that's generally useful when coding."
  spiffy-enable-minor-mode
  " s"
  *spiffy-keymap*)
