;; spiffy-ruby-mode: stuff I use in developing Ruby code.
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

(provide 'spiffy-ruby-mode)

(defvar *spiffy-ruby-keymap* (make-sparse-keymap) "Keybindings go in here")
(defun spiffy-ruby-define-key (key func)
  (define-key *spiffy-ruby-keymap* key func))

;; the theme behind these keybindings:
;;  control-fN:       do something
;;  control-shift-fN: do something similar
;;  super-fN:         repeat the last control-fN action

(spiffy-ruby-define-key [(meta r)] 'spiffy-ruby-run-spec-file)
(spiffy-ruby-define-key [(control f9)] 'spiffy-ruby-rdebug)
(spiffy-ruby-define-key [(control f10)] 'spiffy-ruby-run-spec-file)
(spiffy-ruby-define-key [(control shift f10)] 'spiffy-ruby-run-spec-under-point)
(spiffy-ruby-define-key [(super f10)] 'spiffy-ruby-rerun-last-test)
(spiffy-ruby-define-key [(meta R)] 'spiffy-ruby-run-spec-under-point)
(spiffy-ruby-define-key [(control ?\;) ?s ?c] 'spiffy-ruby-syntax-check)

(defun spiffy-ruby-run-spec-under-point ()
  (interactive)
  (spiffy-ruby-run-spec
   (buffer-file-name)
   "-c"
   "-l"
   (format "%d" (line-number-at-pos)))) ; defaults to line number at point

(defun spiffy-ruby-run-spec-file ()
  (interactive)
  (spiffy-ruby-run-spec (buffer-file-name) "-c"))

(defun spiffy-ruby-run-spec (specfile &rest spec-args)
  (save-buffer)
  (spiffy-run-in-directory
   (setq spiffy-ruby-last-test-dir (spiffy-ruby-merb-root-for specfile))
   (compile
    (setq
     spiffy-ruby-last-test-command
     (apply
      'spiffy-make-shell-command
      (cons
       (spiffy-ruby-spec-binary-to-run-for (buffer-file-name))
       (append spec-args (list specfile))))))))

;; XXX make these rings so that we can have the last N tests run
;;   (1 <= N <= 5, probably).
;; should this show the spec that's being run, too?
;;   spec puts the filename in failures...
(defun spiffy-ruby-rerun-last-test ()
  (interactive)
  (spiffy-run-in-directory
   spiffy-ruby-last-test-dir
   (compile spiffy-ruby-last-test-command)))

(defun spiffy-ruby-rdebug ()
  (interactive)
  (let ((root (spiffy-ruby-merb-root-for (buffer-file-name)))
        (rdebug-binary (spiffy-ruby-rdebug-binary-to-run-for (buffer-file-name))))
    (spiffy-run-in-directory
     root
     (let ((gud-rdebug-command-name (concat rdebug-binary " --emacs 3")))
       ;; so, rdebug unconditionally strips the directory part off the script
       ;; to debug. unfortunately, since we have to run from the merb root,
       ;; stripping that name off makes this not work.
       ;;
       ;; the first call to file-name-nondirectory is the one that's used to
       ;; screw that up, so for the first call, we act like the identity function,
       ;; then restore the function so future uses don't screw up other logic
       ;; inside rdebug.
       ;;
       ;; yes, this is a big fat ugly hack.
       (let ((original-definition (symbol-function 'file-name-nondirectory)))
         (flet ((file-name-nondirectory (name)
                                      (ad-safe-fset 'file-name-nondirectory original-definition)
                                      name))
           (call-interactively 'rdebug)))))))

(defun spiffy-ruby-syntax-check ()
  (interactive)
  (let ((compilation-buffer-name-function (lambda (x) "*syntax check*")))
    (compile
     (spiffy-make-shell-command "ruby" "-c" (buffer-file-name)))))

(defun spiffy-ruby-is-merb-root (dir)
  (file-exists-p (concat (file-name-as-directory dir) "bin/merb")))

(defun spiffy-ruby-merb-root-for (filename)
  (let ((as-dir (file-name-as-directory filename)))
    (if (string= (file-truename as-dir) (file-truename (spiffy-parent-directory as-dir)))
        nil                             ; base case
      (if (spiffy-ruby-is-merb-root as-dir)
          as-dir
        (spiffy-ruby-merb-root-for (spiffy-parent-directory filename))))))

(defun spiffy-ruby-spec-binary-to-run-for (filename)
  (spiffy-ruby-maybe-merbified-binary filename "spec" "bin"))

(defun spiffy-ruby-rdebug-binary-to-run-for (filename)
  (spiffy-ruby-maybe-merbified-binary filename "rdebug" "bin"))

(defun spiffy-ruby-maybe-merbified-binary (filename program relative-path)
  (let ((merb-root (spiffy-ruby-merb-root-for filename)))
    (if merb-root
        (reduce
         (lambda (full partial) (concat (file-name-as-directory full) partial))
         (list merb-root relative-path program))
      program)))             ; whatever the system's one is (hope it has one)

(defun spiffy-ruby-corresponding-filename (filename)
  (if (string-match "^spec" filename)
      (spiffy-ruby-corresponding-code-filename filename)
    (spiffy-ruby-corresponding-test-filename filename)))

(defun spiffy-ruby-corresponding-code-filename (filename)
  ;; the leaning tower of function calls... a code smell, methinks
  (replace-regexp-in-string
   "_spec.rb$" ".rb"
   (replace-regexp-in-string
    "^spec/" ""
    (replace-regexp-in-string
     "^spec/models/" "app/models/"
     (replace-regexp-in-string
      "spec/requests/" "app/controllers/"
      filename)))))

(defun spiffy-ruby-corresponding-test-filename (filename)
  (concat
   "spec/"
   (replace-regexp-in-string
    ".rb$" "_spec.rb"
    (replace-regexp-in-string
     "^app/" ""
     (replace-regexp-in-string
      "/controllers/" "/requests/"
      filename)))))

(define-minor-mode spiffy-ruby-mode
  "Spiffy Ruby minor mode. Stuff that's useful when you're coding in Ruby."
  nil
  " sRB"
  *spiffy-ruby-keymap*)
