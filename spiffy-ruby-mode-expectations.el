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

  (desc "spiffy-ruby-merb-root-for")
  (expect "/my/project/"
    (flet ((file-exists-p (file) (equal file "/my/project/bin/merb")))
      (spiffy-ruby-merb-root-for "/my/project/spec/models/foobar_spec.rb")))

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
)