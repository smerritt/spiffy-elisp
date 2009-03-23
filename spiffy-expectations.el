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

(setq tempdir (make-temp-file "spiffy-expectations" t))

(defun digits-in (x)
  (1+ (truncate (log10 x))))

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

  (expect "cat file\\ with\\ spaces"
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

  (desc "relative paths")
  (expect "src/rand.c"    ; no trailing /
    (spiffy-path-relative-to "/home/sam/prng" "/home/sam/prng/src/rand.c"))

  (expect "src/rand.c"    ; trailing /
    (spiffy-path-relative-to "/home/sam/prng/" "/home/sam/prng/src/rand.c"))

  (expect "/home/sam/prng/src/rand.c"
    (spiffy-path-relative-to "/usr/bin" "/home/sam/prng/src/rand.c"))

  (expect "/home/sam/prng/src/rand.c"
    (spiffy-path-relative-to nil "/home/sam/prng/src/rand.c"))

  (desc "kill region or line")
  (expect "ad\nefgh"
    (with-temp-buffer
      (insert "abcd\nefgh")
      (push-mark (+ 1 (point-min)) t t)
      (goto-char (+ 3 (point-min)))
      (spiffy-kill-region-or-line)
      (buffer-string)))

  (expect "abc\nefgh"
    (with-temp-buffer
      (insert "abcd\nefgh")
      (goto-char (+ 3 (point-min)))
      (spiffy-kill-region-or-line)
      (buffer-string)))

  ;; behave like kill-line wrt prefix argument
  (expect "d\nefgh"
    (with-temp-buffer
      (insert "abcd\nefgh")
      (goto-char (+ 3 (point-min)))
      (spiffy-kill-region-or-line 0)
      (buffer-string)))

  (desc "random numbers")
  (expect 1 (digits-in 5))
  (expect 2 (digits-in 99))
  (expect 3 (digits-in 100))

  ; lower bound
  (expect 3
    (flet ((random (x) 0))
      (digits-in (spiffy-random-ndigit 3))))

  (expect 5
    (flet ((random (x) 0))
      (digits-in (spiffy-random-ndigit 5))))

  ; upper bound
  (expect 3
    (flet ((random (x) x))
      (digits-in (spiffy-random-ndigit 3))))

  (expect 5
    (flet ((random (x) x))
      (digits-in (spiffy-random-ndigit 5))))

  ; different randomly-generated number --> different result
  (expect nil
    (=
     (flet ((random (x) 456)) (spiffy-random-ndigit 4))
     (flet ((random (x) 567)) (spiffy-random-ndigit 4))))

  (desc "inserting a random number")
  (expect 5                             ; default
    (with-temp-buffer
      (call-interactively 'spiffy-insert-random-number)
      (length (buffer-string))))

  (expect 7
    (with-temp-buffer
      (spiffy-insert-random-number 7)
      (length (buffer-string))))

  (desc "inserting a random IP")
  (expect 0
    (string-match
     "\\(\\([0-9]\\)\\{1,3\\}\\.\\)\\{3\\}\\([0-9]\\)\\{1,3\\}" ; dotted quad IPv4
     (with-temp-buffer
       (call-interactively 'spiffy-insert-random-ip)
       (buffer-string))))
)
