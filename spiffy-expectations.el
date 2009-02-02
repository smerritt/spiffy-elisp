;; Tests for spiffy.el: a bunch of stuff I use in my Ruby+Merb coding.
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
(require 'spiffy)
(require 'fancy-pants-function-replacement)

(setq tempdir (make-temp-file "spiffy-expectations" t))

(expectations
  ;; generic stuff
  (desc "spiffy-cwd")
  (expect "/tmp/"
    (cd "/tmp")
    (spiffy-cwd))
  (expect "/"
    (cd "/")
    (spiffy-cwd))
  (expect t       ; this is just for cleanup
    (cd (expand-file-name (concat(file-name-as-directory (buffer-file-name)) "..")))
    t)

  (desc "spiffy-make-shell-command")
  (expect "bin/spec -c -l 30 spec/models/foo.spec"
    (spiffy-make-shell-command "bin/spec" "-c" "-l" "30" "spec/models/foo.spec"))

  (expect "cat \"file with spaces\""
    (spiffy-make-shell-command "cat" "file with spaces"))

  (desc "spiffy-parent-directory")
  (expect "/usr/bin/"
    (spiffy-parent-directory "/usr/bin/X11"))
  (expect "/usr/bin/"
    (spiffy-parent-directory "/usr/bin/X11/"))

  (desc "spiffy-run-in-directory")
  (expect (spiffy-cwd)      ; first, it doesn't affect your current working directory
    (spiffy-run-in-directory
     "/tmp"
     t)
    (spiffy-cwd))

  (expect "/usr/bin/"       ; second, it does execute its body in the named directory
    (spiffy-run-in-directory
     "/usr/bin"
     (spiffy-cwd)))


  ;; merb-specific stuff
  (desc "spiffy-is-merb-root")
  (expect t
    (mocklet
        (((file-exists-p "/path/to/somewhere/bin/merb") => t))
      (spiffy-is-merb-root "/path/to/somewhere/")))

  (expect nil
    (mocklet
        (((file-exists-p "/path/to/somewhere/bin/merb") => nil))
      (spiffy-is-merb-root "/path/to/somewhere/")))

  (desc "spiffy-spec-binary-to-run-for")
  (expect "/somewhere/bin/spec"
    (mocklet
        (((spiffy-merb-root-for "/somewhere/spec/foo.rb") => "/somewhere"))
      (spiffy-spec-binary-to-run-for "/somewhere/spec/foo.rb")))

  (expect "spec"
    (mocklet
        (((spiffy-merb-root-for "/somewhere/spec/foo.rb") => nil))
      (spiffy-spec-binary-to-run-for "/somewhere/spec/foo.rb")))

  (desc "spiffy-merb-root-for")
  (expect "/my/project/"
    (flet ((file-exists-p (file) (equal file "/my/project/bin/merb")))
      (spiffy-merb-root-for "/my/project/spec/models/foobar_spec.rb")))

)

