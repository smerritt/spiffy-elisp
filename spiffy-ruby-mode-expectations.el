;; Tests for spiffy-ruby-mode.el.
;; Probably not useful to anyone but me, but hey, give it a try.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; version 2 of the License.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
(require 'el-expectations)
(require 'spiffy-ruby-mode)

(defvar ruby-file-buffer-filename nil "Filename of buffer used in with-ruby-file-buffer")
(defun setup-ruby-file-buffer ()
  (setq ruby-file-buffer-filename (make-temp-file "spiffy-ruby-mode-expectations"))
  (setq shell-quoted-ruby-file-buffer-filename (shell-quote-argument ruby-file-buffer-filename)))
(setup-ruby-file-buffer)

(defmacro with-ruby-file-buffer (&rest body)
  `(let (retval)
     (with-current-buffer (find-file-noselect ruby-file-buffer-filename)
       (delete-region (point-min) (point-max))
       (ruby-mode)
       (spiffy-ruby-mode t)
       (setq retval (progn ,@body))
       (kill-buffer nil)   ; or else it hangs around as an open buffer
       retval)))

(expectations
  (desc "spiffy-ruby-is-merb-root")
  (expect t
    (mocklet
        (((file-exists-p "/path/to/somewhere/config/init.rb") => t))
      (spiffy-ruby-is-merb-root "/path/to/somewhere/")))

  (expect nil
    (mocklet
        (((file-exists-p "/path/to/somewhere/config/init.rb") => nil))
      (spiffy-ruby-is-merb-root "/path/to/somewhere/")))

  (desc "spiffy-ruby-spec-binary-to-run-for")
  (expect "/somewhere/bin/spec"
    (mocklet
        (((spiffy-ruby-merb-root-for "/somewhere/spec/foo.rb") => "/somewhere")
         ((file-exists-p "/somewhere/bin/spec") => t))
      (spiffy-ruby-spec-binary-to-run-for "/somewhere/spec/foo.rb")))

  (expect "spec"     ; file doesn't exist (Merb app sans bundled gems)
    (mocklet
        (((spiffy-ruby-merb-root-for "/somewhere/spec/foo.rb") => "/somewhere")
         ((file-exists-p "/somewhere/bin/spec") => nil))
      (spiffy-ruby-spec-binary-to-run-for "/somewhere/spec/foo.rb")))

  (expect "spec"
    (mocklet
        (((spiffy-ruby-merb-root-for "/somewhere/spec/foo.rb") => nil))
      (spiffy-ruby-spec-binary-to-run-for "/somewhere/spec/foo.rb")))

  (desc "spiffy-ruby-rdebug-binary-to-run-for")
  (expect "/somewhere/bin/rdebug"
    (mocklet
        (((spiffy-ruby-merb-root-for "/somewhere/rdebug/foo.rb") => "/somewhere")
         ((file-exists-p "/somewhere/bin/rdebug") => t))
      (spiffy-ruby-rdebug-binary-to-run-for "/somewhere/rdebug/foo.rb")))

  (expect "rdebug"
    (mocklet
        (((spiffy-ruby-merb-root-for "/somewhere/rdebug/foo.rb") => nil))
      (spiffy-ruby-rdebug-binary-to-run-for "/somewhere/rdebug/foo.rb")))

  (desc "spiffy-ruby-merb-root-for")
  (expect "/my/project/"
    (flet ((file-exists-p (file) (equal file "/my/project/config/init.rb")))
      (spiffy-ruby-merb-root-for "/my/project/spec/models/foobar_spec.rb")))

  (desc "run spec under point")
  (expect "/tmp/"                       ; runs in the merb root
    (flet ((compile (x &optional y) (spiffy-cwd))
           (spiffy-ruby-merb-root-for (x) "/tmp"))
      (with-ruby-file-buffer
       (call-interactively 'spiffy-ruby-run-spec-under-point))))

  (expect (concat "spec -c -fs -l 1 " shell-quoted-ruby-file-buffer-filename)
    (flet ((compile (x &optional y) x))
      (with-ruby-file-buffer
       (call-interactively 'spiffy-ruby-run-spec-under-point))))

  (expect (concat "spec -c -fs -l 1 " shell-quoted-ruby-file-buffer-filename)
    (flet ((compile (x &optional y) x))
      (with-ruby-file-buffer
       (call-interactively 'spiffy-ruby-run-spec-under-point)
       spiffy-ruby-last-test-command)))

  (expect "/usr/bin"
    (flet ((compile (x &optional y) x)
           (spiffy-ruby-merb-root-for (x) "/usr/bin"))
      (with-ruby-file-buffer
       (call-interactively 'spiffy-ruby-run-spec-under-point)
       spiffy-ruby-last-test-dir)))

  (desc "run spec file")
  (expect "/tmp/"                       ; runs in the merb root
    (flet ((compile (x &optional y) (spiffy-cwd))
           (spiffy-ruby-merb-root-for (x) "/tmp"))
      (with-ruby-file-buffer
       (call-interactively 'spiffy-ruby-run-spec-file))))

  (expect (concat "spec -c -fs " shell-quoted-ruby-file-buffer-filename)
    (flet ((compile (x &optional y) x))
      (with-ruby-file-buffer
       (call-interactively 'spiffy-ruby-run-spec-file))))

  (expect (concat "spec -c -fs " shell-quoted-ruby-file-buffer-filename)
    (flet ((compile (x &optional y) x))
      (with-ruby-file-buffer
       (call-interactively 'spiffy-ruby-run-spec-file)
       spiffy-ruby-last-test-command)))

  (expect "/usr/bin"
    (flet ((compile (x &optional y) x)
           (spiffy-ruby-merb-root-for (x) "/usr/bin"))
      (with-ruby-file-buffer
       (call-interactively 'spiffy-ruby-run-spec-under-point)
       spiffy-ruby-last-test-dir)))

  (desc "re-run last test")
  (expect "ack -thpppt"
    (let ((spiffy-ruby-last-test-command "ack -thpppt"))
      (flet ((compile (x &optional y) x))
        (with-ruby-file-buffer
         (call-interactively 'spiffy-ruby-rerun-last-test)))))

  (desc "invoking rdebug")
  (expect "/tmp/"                       ; runs in the merb root
    (flet ((rdebug (&optional args) (spiffy-cwd))
           (spiffy-ruby-merb-root-for (x) "/tmp")
                                        ; flet + interactive don't play well together
           (call-interactively (x) (funcall x)))
      (with-ruby-file-buffer
                                        ; no point w/call-interactively since it's nerfed
       (spiffy-ruby-rdebug))))

  (expect "/tmp/bin/rdebug --emacs 3"  ; uses the merb-relative rdebug
    (let ((gud-rdebug-command-name "rdebug --emacs 3"))
      (flet ((rdebug (&optional args) gud-rdebug-command-name)
             (spiffy-ruby-merb-root-for (x) "/tmp")
             (call-interactively (x) (funcall x))
             (spiffy-ruby-rdebug-binary-to-run-for (x) "/tmp/bin/rdebug"))
        (with-ruby-file-buffer
         (spiffy-ruby-rdebug)))))

  (expect "original-rdebug"      ; don't screw with the original value
    (let ((gud-rdebug-command-name "original-rdebug"))
      (flet ((rdebug (&optional args) t)
             (spiffy-ruby-merb-root-for (x) "/tmp")
             (call-interactively (x) (funcall x)))
        (with-ruby-file-buffer
         (spiffy-ruby-rdebug))
        gud-rdebug-command-name)))

  ;; tests for the horrible hack
  ;; this is only because I cannot come up with a good way, short of really running rdebug
  ;; and waiting to see if it worked and dealing with all those fun race conditiony things
  ;; to test that this thing works.
  ;;
  ;; at least this way, I know that the hack is as I intend it to be, even if it's not
  ;; actually fixing anything.
  (expect '("/full/path/to/foo" "foo")
    (flet ((rdebug () (list
                       (file-name-nondirectory "/full/path/to/foo")
                       (file-name-nondirectory "/full/path/to/foo")))
           (call-interactively (x) (funcall x)))
      (with-ruby-file-buffer
       (spiffy-ruby-rdebug))))

  (desc "syntax check")
  (expect "*syntax check*"
    (flet ((compile (x &optional y) (funcall compilation-buffer-name-function "ruby-mode")))
      (with-ruby-file-buffer
       (call-interactively 'spiffy-ruby-syntax-check))))

  (expect (concat "ruby -c " shell-quoted-ruby-file-buffer-filename)
    (flet ((compile (command &optional dontcare) command))
      (with-ruby-file-buffer
       (call-interactively 'spiffy-ruby-syntax-check))))

  (desc "switch between spec + implementation")
  (expect "app/models/bottle.rb"
    (spiffy-ruby-corresponding-filename "spec/models/bottle_spec.rb"))

  (expect "spec/models/bottle_spec.rb"
    (spiffy-ruby-corresponding-filename "app/models/bottle.rb"))

  (expect "spec/requests/bottles_spec.rb"
    (spiffy-ruby-corresponding-filename "app/controllers/bottles.rb"))

  (expect "app/controllers/bottles.rb"
    (spiffy-ruby-corresponding-filename "spec/requests/bottles_spec.rb"))

  (expect "spec/lib/bottle_util_spec.rb"
    (spiffy-ruby-corresponding-filename "lib/bottle_util.rb"))

  (expect "lib/bottle_util.rb"
    (spiffy-ruby-corresponding-filename "spec/lib/bottle_util_spec.rb"))
)