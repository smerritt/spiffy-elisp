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

(spiffy-ruby-define-key [(meta r)] 'spiffy-ruby-run-spec-file)
(spiffy-ruby-define-key [(meta R)] 'spiffy-ruby-run-spec-under-point)
(spiffy-ruby-define-key [(control ?\;) ?s ?c] 'spiffy-ruby-syntax-check)

(defun spiffy-ruby-run-spec-under-point ()   ; XXX test it
  (interactive)
  (spiffy-ruby-run-spec
   (buffer-file-name)
   "-c"
   "-l"
   (format "%d" (line-number-at-pos)))) ; defaults to line number at point

(defun spiffy-ruby-run-spec-file ()          ; XXX test it
  (interactive)
  (spiffy-ruby-run-spec (buffer-file-name) "-c"))

(defun spiffy-ruby-run-spec (specfile &rest spec-args)
  (save-buffer)
  (spiffy-run-in-directory
   (spiffy-ruby-merb-root-for specfile)
   (compile
    (apply 'spiffy-make-shell-command
           (spiffy-ruby-spec-binary-to-run-for (buffer-file-name))
           specfile
           spec-args))))

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
  (let ((merb-root (spiffy-ruby-merb-root-for filename)))
    (if merb-root
        (concat (file-name-as-directory merb-root) "bin/spec")
      "spec")))                 ; whatever the system's spec binary is


(define-minor-mode spiffy-ruby-mode
  "Spiffy Ruby minor mode. Stuff that's useful when you're coding in Ruby."
  nil
  " sRB"
  *spiffy-ruby-keymap*
  )