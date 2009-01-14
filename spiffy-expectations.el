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

  (desc "spiffy-find-files")
  ; XXX test not portable due to hardcoded slashes, but I'm lazy
  (expect (list
           (concat tempdir "/bar")
           (concat tempdir "/foo")
           (concat tempdir "/subdir/bar"))
    (progn
      (append-to-file 5 10 (concat tempdir "/bar"))
      (append-to-file 5 10 (concat tempdir "/foo"))
      (make-directory (concat tempdir "/subdir"))
      (append-to-file 5 10 (concat tempdir "/subdir/bar"))
      (sort
       (spiffy-find-files tempdir)
       (lambda (a b) (string< a b)))))

  ;; merb-specific stuff
  (desc "spiffy-is-merb-root-dir")
  (expect t
    (mocklet
        (((file-exists-p "/path/to/somewhere/bin/merb") => t))
      (spiffy-is-merb-root-dir "/path/to/somewhere/")))

  (expect nil
    (mocklet
        (((file-exists-p "/path/to/somewhere/bin/merb") => nil))
      (spiffy-is-merb-root-dir "/path/to/somewhere/")))

  (desc "spiffy-spec-binary-to-run-for")
  (expect "/somewhere/bin/spec"
    (mocklet
        (((spiffy-merb-root-dir-for "/somewhere/spec/foo.rb") => "/somewhere"))
      (spiffy-spec-binary-to-run-for "/somewhere/spec/foo.rb")))

  (expect "spec"
    (mocklet
        (((spiffy-merb-root-dir-for "/somewhere/spec/foo.rb") => nil))
      (spiffy-spec-binary-to-run-for "/somewhere/spec/foo.rb")))

  (desc "spiffy-merb-root-dir-for")
  (expect "/my/project/"
    (with-temporary-function-replacement
     (file-exists-p (lambda (file) (equal file "/my/project/bin/merb")))
     (spiffy-merb-root-dir-for "/my/project/spec/models/foobar_spec.rb")))
   
)



