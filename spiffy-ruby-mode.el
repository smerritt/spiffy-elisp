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

(spiffy-ruby-define-key [(control f9)] 'spiffy-ruby-rdebug)
(spiffy-ruby-define-key [(control f10)] 'spiffy-ruby-run-spec-file)
(spiffy-ruby-define-key [(control shift f10)] 'spiffy-ruby-run-spec-under-point)
(spiffy-ruby-define-key [(control ?\;) ?m ?i] 'spiffy-ruby-inf-merb)
(spiffy-ruby-define-key [(control ?\;) ?s ?b] 'spiffy-ruby-switch-code-and-test-buffer)
(spiffy-ruby-define-key [(control ?\;) ?r ?t] 'spiffy-ruby-rerun-last-test)
(spiffy-ruby-define-key [(meta r)] 'spiffy-ruby-run-spec-file)
(spiffy-ruby-define-key [(meta R)] 'spiffy-ruby-run-spec-under-point)
(spiffy-ruby-define-key [(control ?\;) ?s ?c] 'spiffy-ruby-syntax-check)
(spiffy-ruby-define-key [(control ?\;) ?r ?f] 'spiffy-ruby-run-file)

(defun spiffy-ruby-run-spec-under-point ()
  (interactive)
  (spiffy-ruby-run-spec
   (buffer-file-name)
   "-c"
   "-fs"
   "-l"
   (format "%d" (line-number-at-pos)))) ; defaults to line number at point

(defun spiffy-ruby-run-spec-file ()
  (interactive)
  (spiffy-ruby-run-spec (buffer-file-name) "-c" "-fs"))

(defun spiffy-ruby-run-spec (specfile &rest spec-args)
  (save-buffer)
  (spiffy-run-in-directory
   (setq spiffy-ruby-last-test-dir (spiffy-ruby-bundle-root-for specfile))
   (compile (setq spiffy-ruby-last-test-command
                  ;; don't shell-escape "bundle exec spec"; it doesn't help
                  (concat (spiffy-ruby-maybe-bundled-command (buffer-file-name)
                                                              "spec")
                          " "
                          (apply 'spiffy-make-shell-command
                                 (append spec-args (list specfile))))))))


;; XXX make these rings so that we can have the last N tests run
;;   (1 <= N <= 5, probably).
;; should this show the spec that's being run, too?
;;   spec puts the filename in failures...
(defun spiffy-ruby-rerun-last-test ()
  (interactive)
  (save-buffer)
  (spiffy-run-in-directory
   spiffy-ruby-last-test-dir
   (compile spiffy-ruby-last-test-command)))

(defun spiffy-ruby-rdebug ()
  (interactive)
  (let ((root (spiffy-ruby-bundle-root-for (buffer-file-name)))
        (rdebug-command (spiffy-ruby-maybe-bundled-command (buffer-file-name) "rdebug")))
    (spiffy-run-in-directory
     root
     (let ((gud-rdebug-command-name (concat rdebug-command " --emacs 3")))
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
     (spiffy-make-shell-command "ruby" "-c" (spiffy-buffer-or-temp-file-name)))))

(defun spiffy-ruby-run-file ()
  (interactive)
  (compile
   (spiffy-make-shell-command "ruby" (spiffy-buffer-or-temp-file-name))))

(defun spiffy-ruby-bundle-root-for (filename)
  (let ((root (locate-dominating-file filename "Gemfile")))
    (if root
        (expand-file-name root)
      root)))

(defun spiffy-ruby-maybe-bundled-command (filename program)
  (let ((bundle-root (spiffy-ruby-bundle-root-for filename)))
    (if bundle-root
        (concat "bundle exec " program)
      program)))

(defun spiffy-ruby-inf-merb ()
  (interactive)
  (spiffy-run-in-directory (spiffy-ruby-bundle-root-for (buffer-file-name))
                           (make-comint "merb -i"
                                        (spiffy-ruby-maybe-bundled-command (buffer-file-name) "merb")
                                        nil
                                        "-i"))
  (switch-to-buffer-other-window "*merb -i*"))

;; XXX test me
(defun spiffy-ruby-switch-code-and-test-buffer ()
  (interactive)
  (let* ((bundle-root (spiffy-ruby-bundle-root-for (buffer-file-name)))
         (other-file (concat
                      (file-name-as-directory bundle-root)
                      (spiffy-ruby-corresponding-filename
                       (spiffy-path-relative-to bundle-root (buffer-file-name))))))
    (if (and other-file (not (string= other-file (buffer-file-name))))
        (if (file-exists-p other-file)
            (find-file other-file)
          (error "File %s does not exist" other-file)))))

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
