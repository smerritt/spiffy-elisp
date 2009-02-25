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

(defmacro with-ruby-file-buffer (&rest body)
  `(let ((tempfile (make-temp-file "spiffy-ruby-mode-expectations"))
         retval)
     (with-current-buffer (find-file-noselect tempfile)
       (ruby-mode)
       (spiffy-ruby-mode t)
       (setq retval (progn ,@body))
       (kill-buffer nil)   ; or else it hangs around as an open buffer
       retval)))

(expectations
  (desc "spiffy-ruby-is-merb-root")
  (expect t
    (mocklet
        (((file-exists-p "/path/to/somewhere/bin/merb") => t))
      (spiffy-ruby-is-merb-root "/path/to/somewhere/")))

  (expect nil
    (mocklet
        (((file-exists-p "/path/to/somewhere/bin/merb") => nil))
      (spiffy-ruby-is-merb-root "/path/to/somewhere/")))

  (desc "spiffy-ruby-spec-binary-to-run-for")
  (expect "/somewhere/bin/spec"
    (mocklet
        (((spiffy-ruby-merb-root-for "/somewhere/spec/foo.rb") => "/somewhere"))
      (spiffy-ruby-spec-binary-to-run-for "/somewhere/spec/foo.rb")))

  (expect "spec"
    (mocklet
        (((spiffy-ruby-merb-root-for "/somewhere/spec/foo.rb") => nil))
      (spiffy-ruby-spec-binary-to-run-for "/somewhere/spec/foo.rb")))

  (desc "spiffy-ruby-rdebug-binary-to-run-for")
  (expect "/somewhere/bin/rdebug"
    (mocklet
    (((spiffy-ruby-merb-root-for "/somewhere/rdebug/foo.rb") => "/somewhere"))
    (spiffy-ruby-rdebug-binary-to-run-for "/somewhere/rdebug/foo.rb")))

  (expect "rdebug"
    (mocklet
    (((spiffy-ruby-merb-root-for "/somewhere/rdebug/foo.rb") => nil))
    (spiffy-ruby-rdebug-binary-to-run-for "/somewhere/rdebug/foo.rb")))

  (desc "spiffy-ruby-merb-root-for")
  (expect "/my/project/"
    (flet ((file-exists-p (file) (equal file "/my/project/bin/merb")))
      (spiffy-ruby-merb-root-for "/my/project/spec/models/foobar_spec.rb")))

  (desc "run spec under point")
  (expect "/tmp/"    ; runs in the merb root
    (flet ((compile (x &optional y) (spiffy-cwd))
           (spiffy-ruby-merb-root-for (x) "/tmp"))
      (with-ruby-file-buffer
       (call-interactively 'spiffy-ruby-run-spec-under-point))))

  (expect 0
    (string-match
     "spec -c -l 1 /"
     (flet ((compile (x &optional y) x))
       (with-ruby-file-buffer
        (call-interactively 'spiffy-ruby-run-spec-under-point)))))

  (expect 0
    (string-match
     "spec -c -l 1 /"
     (flet ((compile (x &optional y) x))
       (with-ruby-file-buffer
        (call-interactively 'spiffy-ruby-run-spec-under-point)
        spiffy-ruby-last-test-command))))

  (expect "/usr/bin"
    (flet ((compile (x &optional y) x)
           (spiffy-ruby-merb-root-for (x) "/usr/bin"))
      (with-ruby-file-buffer
       (call-interactively 'spiffy-ruby-run-spec-under-point)
       spiffy-ruby-last-test-dir)))

  (desc "run spec file")
  (expect "/tmp/"                     ; runs in the merb root
    (flet ((compile (x &optional y) (spiffy-cwd))
           (spiffy-ruby-merb-root-for (x) "/tmp"))
      (with-ruby-file-buffer
       (call-interactively 'spiffy-ruby-run-spec-file))))

  (expect 0
    (string-match
     "spec -c /"
     (flet ((compile (x &optional y) x))
       (with-ruby-file-buffer
        (call-interactively 'spiffy-ruby-run-spec-file)))))

  (expect 0
    (string-match
     "spec -c /"
     (flet ((compile (x &optional y) x))
       (with-ruby-file-buffer
        (call-interactively 'spiffy-ruby-run-spec-file)
        spiffy-ruby-last-test-command))))

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

  (desc "syntax check")
  (expect "*syntax check*"
    (flet ((compile (x &optional y) (funcall compilation-buffer-name-function "ruby-mode")))
      (with-ruby-file-buffer
       (call-interactively 'spiffy-ruby-syntax-check))))

  (expect 0
    (string-match
     "ruby -c /"     ; we don't actually know the file name here
     (flet ((compile (command &optional dontcare) command))
       (with-ruby-file-buffer
        (call-interactively 'spiffy-ruby-syntax-check)))))

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