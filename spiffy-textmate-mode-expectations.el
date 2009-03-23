;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; version 2 of the License.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
(require 'el-expectations)
(require 'spiffy-textmate-mode)

(setq tempdir (make-temp-file "spiffy-tm-expectations" t))

(defmacro with-gibberish-buffer (&rest body)
  `(with-temp-buffer
     (insert "abcdef\n")
     (insert "ghijkl\n")
     (insert "123456\n")
     (goto-char (point-min))   ; get to the i
     (forward-line 1)
     (forward-char 2)
     ,@body))

(defmacro with-small-gibberish-buffer (&rest body)
  `(with-temp-buffer
     (insert "abcdef")
     (goto-char (point-min))
     ,@body))

(defmacro with-ruby-buffer (&rest body)
  `(with-temp-buffer
     (ruby-mode)
     (let ((comment-style 'plain))      ; so tests don't fail due to comment preferences
       (insert "class Foo\n")
       (insert "  def foo?\n")
       (insert "    true\n")
       (insert "  end\n")
       (insert "end\n")
       (goto-char (point-min))
       ,@body)))

(defmacro with-short-scoot-test-buffer (&rest body)
  `(with-temp-buffer
    (insert "abXYef")
    (goto-char (+ 2 (point-min))) ; region contains "XY"
    (push-mark (+ 2 (point)) t t)
    ,@body))

(defmacro with-long-scoot-test-buffer (&rest body)
  `(with-temp-buffer
    (insert "*\n*\n*\nabXYdf\n*\n*\n*")
    (goto-char (+ 6 (point-min))) ; region contains "abXYdf\n"
    (push-mark (+ 7 (point)) t t)
    ,@body))

(defun text-in-region ()
  (buffer-substring (region-beginning) (region-end)))

(expectations
  (desc "spiffy-tm-project-files-for")
  (expect (list
           (concat tempdir "/myproj/model/beer.rb")
           (concat tempdir "/myproj/spec/beer_spec.rb"))
    (progn
      (make-directory (concat tempdir "/myproj"))
      (make-directory (concat tempdir "/myproj/model"))
      (make-directory (concat tempdir "/myproj/spec"))
      (make-directory (concat tempdir "/myproj/.git"))    ; not in result
      (append-to-file 5 10 (concat tempdir "/myproj/model/beer.rb"))
      (append-to-file 5 10 (concat tempdir "/myproj/spec/beer_spec.rb"))
      (append-to-file 5 10 (concat tempdir "/myproj/.git/HEAD"))  ; not in result
      (append-to-file 5 10 (concat tempdir "/myproj/model/beer.rb~"))  ; not in result
      (sort
       (spiffy-tm-project-files-for (concat tempdir "/myproj/model/beer.rb"))
       (lambda (a b) (string< a b)))))

  ;; stuff concerning Git projects (could be extended to others)
  (desc "spiffy-tm-is-project-root")
  (expect t
    (mocklet
        (((file-exists-p "/path/to/project/.git") => t))
      (spiffy-tm-is-project-root "/path/to/project/")))

  (desc "find in project")
  (expect "/usr/tmp/"    ; find in the project root
    (mocklet
        (((grep-compute-defaults) => t))     ; will cause error if not called, just like the real one
      (flet ((rgrep (regex &optional pattern path) path)
             (spiffy-tm-project-root-for (x) "/usr/tmp/"))
        (spiffy-tm-grep-project "defun"))))

  (expect "*"                           ; search in all files
    (flet ((rgrep (regex &optional pattern path) pattern)
           (spiffy-tm-project-root-for (x) "/usr/tmp/"))
      (spiffy-tm-grep-project "defun")))

  (expect "defun"                       ; search for the specified pattern
    (flet ((rgrep (regex &optional pattern path) regex)
           (spiffy-tm-project-root-for (x) "/usr/tmp/"))
      (spiffy-tm-grep-project "defun")))

  (desc "arrows")
  (expect "ijk"
    (with-gibberish-buffer
     (spiffy-tm-arrow-right)
     (spiffy-tm-arrow-right)
     (spiffy-tm-arrow-right)
     (text-in-region)))

  (expect "\ngh"
    (with-gibberish-buffer
     (spiffy-tm-arrow-left)
     (spiffy-tm-arrow-left)
     (spiffy-tm-arrow-left)
     (text-in-region)))

  (expect "ijkl\n12"
    (with-gibberish-buffer
     (spiffy-tm-arrow-down)
     (text-in-region)))

  (expect "cdef\ngh"
    (with-gibberish-buffer
     (spiffy-tm-arrow-up)
     (text-in-region)))

  (expect " e_f"
    (with-temp-buffer
      (insert "a_b c_d e_f")
      (goto-char (point-min))
      (spiffy-tm-forward-liberal-word)
      (spiffy-tm-forward-liberal-word)
      (buffer-substring (point) (point-max))))

  (expect " e_f"
    (with-temp-buffer
      (insert "a_b c_d e_f")
      (goto-char (point-min))
      (spiffy-tm-forward-liberal-word 2)
      (buffer-substring (point) (point-max))))

  (expect "a_b "
    (with-temp-buffer
      (insert "a_b c_d e_f")
      (goto-char (point-max))
      (spiffy-tm-backward-liberal-word)
      (spiffy-tm-backward-liberal-word)
      (buffer-substring (point-min) (point))))

  (desc "spiffy shift-arrows")
  (expect "ijkl"
    (with-gibberish-buffer
     (spiffy-tm-arrow-right-word)
     (text-in-region)))

  (expect "gh"
    (with-gibberish-buffer
     (spiffy-tm-arrow-left-word)
     (text-in-region)))

  (expect "a_b"
    (with-temp-buffer
      (insert "a_b c_d e_f")
      (goto-char (point-min))
      (spiffy-tm-arrow-forward-liberal-word)
      (text-in-region)))

  (expect "e_f"
    (with-temp-buffer
      (insert "a_b c_d e_f")
      (goto-char (point-max))
      (spiffy-tm-arrow-backward-liberal-word)
      (text-in-region)))

  (expect "abc de"
    (with-temp-buffer
     (insert "abc def ghi\n123")
     (goto-char (+ 6 (point-min)))
     (spiffy-tm-arrow-left-line)
     (text-in-region)))

  (expect "f ghi"
    (with-temp-buffer
     (insert "abc def ghi\n123")
     (goto-char (+ 6 (point-min)))
     (spiffy-tm-arrow-right-line)
     (text-in-region)))

  (expect "abcdef\ngh"
    (with-gibberish-buffer
     (spiffy-tm-arrow-bob)
     (text-in-region)))

  (expect "ijkl\n123456\n"
    (with-gibberish-buffer
     (spiffy-tm-arrow-eob)
     (text-in-region)))

  (desc "select-line")
  ;; select current line when mark is not active
  (expect "def"
    (with-temp-buffer
      (insert "abc\ndef\nghi\njkl")
      (goto-char (+ 5 (point-min)))
      (spiffy-tm-select-line)
      (text-in-region)))

  ;; push region boundaries outward to have only whole lines selected
  (expect "def\nghi\n"
    (with-temp-buffer
      (insert "abc\ndef\nghi\njkl")
      (push-mark (+ 5 (point-min)) t t)  ; after d
      (goto-char (+ 10 (point-min)))     ; after h
      (spiffy-tm-select-line)
      (text-in-region)))

  ;; same as before, but with point before mark
  (expect "def\nghi\n"
    (with-temp-buffer
      (insert "abc\ndef\nghi\njkl")
      (push-mark (+ 5 (point-min)) t t)  ; after d
      (goto-char (+ 10 (point-min)))     ; after h
      (exchange-point-and-mark)
      (spiffy-tm-select-line)
      (text-in-region)))

  ;; be okay at the end of the buffer
  (expect "def\nghi"
    (with-temp-buffer
      (insert "abc\ndef\nghi")
      (push-mark (+ 5 (point-min)) t t)  ; after d
      (goto-char (+ 10 (point-min)))     ; after h
      (spiffy-tm-select-line)
      (text-in-region)))

  (desc "spiffy fancy left delimiters")
  (expect "a(bcd)ef"
    (with-small-gibberish-buffer
     (forward-char)
     (push-mark nil t t)
     (forward-char 3)
     (spiffy-tm-left-paren (region-beginning) (region-end))
     (buffer-string)))

  (expect "a(bcdef)"
    (with-small-gibberish-buffer
     (forward-char)
     (push-mark nil t t)
     (forward-char 5)
     (spiffy-tm-left-paren (region-beginning) (region-end))
     (buffer-string)))

  (expect "a()bcdef"
    (with-small-gibberish-buffer
     (forward-char)
     (spiffy-tm-left-paren)
     (buffer-string)))

  (expect ?\)
    (with-small-gibberish-buffer
     (spiffy-tm-left-paren)
     (char-after)))

  (expect "a[bcd]ef"
    (with-small-gibberish-buffer
     (forward-char)
     (push-mark nil t t)
     (forward-char 3)
     (spiffy-tm-left-bracket (region-beginning) (region-end))
     (buffer-string)))

  (expect "a[]bcdef"
    (with-small-gibberish-buffer
     (forward-char)
     (spiffy-tm-left-bracket)
     (buffer-string)))

  (expect "a{bcd}ef"
    (with-small-gibberish-buffer
     (forward-char)
     (push-mark nil t t)
     (forward-char 3)
     (spiffy-tm-left-curly (region-beginning) (region-end))
     (buffer-string)))

  ; paren blinking
  (expect "called"
    (let* ((blink "not called"))
      (with-temp-buffer
        (let ((blink-paren-function
               (lambda () (message "foo") (setq blink "called"))))
          (spiffy-tm-left-paren)
          (funcall blink-paren-function)
          blink))))

  (expect t                             ; passes by not crashing
    (with-temp-buffer
      (let ((blink-paren-function nil))
          (spiffy-tm-left-paren)
          t)))

  (desc "spiffy fancy right delimiters")
  (expect "abc)d)ef"
    (with-temp-buffer
     (insert "abcd)ef")
     (goto-char 4)   ; looking at d
     (spiffy-tm-right-paren)
     (forward-char)
     (spiffy-tm-right-paren)
     (buffer-string)))

  (expect "abc}d}ef"
    (with-temp-buffer
     (insert "abcd}ef")
     (goto-char 4)   ; looking at d
     (spiffy-tm-right-curly)
     (forward-char)
     (spiffy-tm-right-curly)
     (buffer-string)))

  (desc "delete a left-delimiter sometimes takes out the right delimiter")
  (expect "abcd"
    (with-temp-buffer
      (insert "ab()cd")
      (goto-char (point-min))
      (forward-char 3)          ; between the parens
      (spiffy-tm-backspace)
      (buffer-string)))

  ; and the others too
  (expect "abcd"
    (with-temp-buffer
      (insert "ab[{}]cd")
      (goto-char (point-min))
      (forward-char 4)          ; between the balanced delimiters
      (spiffy-tm-backspace)
      (spiffy-tm-backspace)
      (buffer-string)))

  ; don't do it if the parens are not balanced
  (expect "ab)cd"
    (with-temp-buffer
      (insert "ab{)cd")
      (goto-char (point-min))
      (forward-char 3)
      (spiffy-tm-backspace)
      (buffer-string)))

  ; pass along arguments
  (expect "a"
    (with-temp-buffer
      (insert "abcd[()]")
      (goto-char (point-max))
      (backward-char 2)        ; between the parens
      (spiffy-tm-backspace 5)
      (buffer-string)))

  (desc "delete the region if active")
  (expect "ad"
    (with-temp-buffer
      (insert "abcd")
      (goto-char (point-min))
      (forward-char 1)
      (push-mark nil t t)
      (forward-char 2)
      (spiffy-tm-backspace)
      (buffer-string)))

  (desc "delete key")
  (expect "ad"
    (with-temp-buffer
      (insert "abcd")
      (goto-char (point-min))
      (forward-char 1)
      (push-mark nil t t)
      (forward-char 2)
      (spiffy-tm-delete)
      (buffer-string)))

  (expect "acd"
    (with-temp-buffer
      (insert "abcd")
      (goto-char (point-min))
      (forward-char 1)
      (spiffy-tm-delete)
      (buffer-string)))

  (desc "select current word or kill region")
  (expect "abc"      ; select current word
    (with-temp-buffer
      (insert "abc def ghi")
      (goto-char 2)  ; b
      (spiffy-tm-select-current-word-or-kill-region)
      (text-in-region)))

  (expect " def ghi"      ; kill-region
    (with-temp-buffer
      (insert "abc def ghi")
      (goto-char 2)  ; b
      (spiffy-tm-select-current-word-or-kill-region) ; select
      (spiffy-tm-select-current-word-or-kill-region) ; kill
      (buffer-string)))

  (desc "kill whole line")
  (expect "abcdef\n123456\n"
    (with-gibberish-buffer
     (spiffy-tm-kill-entire-line)
     (buffer-string)))

  (expect ""
    (with-temp-buffer
      (insert "blah")     ; no newline
      (spiffy-tm-kill-entire-line)
      (buffer-string)))

  (expect "abc\n456"
    (with-temp-buffer
      (insert "abc\ndef\n123\n456")
      (goto-char (+ 5 (point-min)))
      (push-mark nil t t)
      (forward-line)
      (spiffy-tm-kill-entire-line)
      (buffer-string)))

  (desc "commenting stuff out")
  ; no region? comment the line you're on
  (expect "# class Foo\n  def foo?\n    true\n  end\nend\n"
    (with-ruby-buffer
     (spiffy-tm-comment-dwim)
     (buffer-string)))

  ; no region and on a comment? uncomment line
  (expect "class Foo\n  def foo?\n    true\n  end\nend\n"
    (with-ruby-buffer
     (insert "# ")
     (spiffy-tm-comment-dwim)
     (buffer-string)))

  ; no region? do stuff and move down a line
  (expect "  def foo?"
    (with-ruby-buffer
     (spiffy-tm-comment-dwim)
     (buffer-substring (point-at-bol) (point-at-eol))))

  ; don't bump your head on the end of the buffer (passes by not erroring out)
  (expect t
    (with-ruby-buffer
     (goto-char (point-max))
     (spiffy-tm-comment-dwim)
     t))

  ; region selected? comment out the whole thing
  (expect "# class Foo\n#   def foo?\n    true\n  end\nend\n"
    (with-ruby-buffer
     (push-mark nil t t)
     (next-line 2)
     (spiffy-tm-comment-dwim)
     (buffer-string)))

  ; region selected and all comments? uncomment the whole thing
  (expect "class Foo\n  def foo?\n    true\n  end\nend\n"
    (with-ruby-buffer
     (insert "# ")
     (next-line)
     (move-beginning-of-line nil)
     (insert "# ")
     (goto-char (point-min))
     (push-mark nil t t)
     (next-line 2)
     (spiffy-tm-comment-dwim)
     (buffer-string)))

  (desc "meta-return")
  (expect "abcd\nzyx"
    (with-temp-buffer
      (insert "abcd")
      (backward-char 2)
      (spiffy-tm-put-newline-at-eol)
      (insert "zyx")
      (buffer-string)))

  (desc "open line above current one and indent")
  (expect "abc\n\ndef"
    (with-temp-buffer
      (insert "abc\ndef")
      (goto-char (1- (point-max)))
      (call-interactively 'spiffy-tm-open-line-before)
      (buffer-string)))

  (expect "abc\n\n\ndef"
    (with-temp-buffer
      (insert "abc\ndef")
      (goto-char (1- (point-max)))
      (spiffy-tm-open-line-before 2)
      (buffer-string)))

  (expect "class Foo\n  def foo?\n    \n    true\n  end\nend\n"
    (with-ruby-buffer
     (goto-char (point-min))
     (forward-line 2)     ; on 'true'
     (spiffy-tm-open-line-before)
     (buffer-string)))

  (desc "scoot right")
  (expect "abefXY"
    (with-short-scoot-test-buffer
     (call-interactively 'spiffy-tm-scoot-right)
     (call-interactively 'spiffy-tm-scoot-right)
     (buffer-string)))

  ; leave the same text in the region
  (expect "XY"
    (with-short-scoot-test-buffer
     (call-interactively 'spiffy-tm-scoot-right)
     (text-in-region)))

  ; and leave the text highlighted
  (expect nil
    (with-short-scoot-test-buffer
      (call-interactively 'spiffy-tm-scoot-right)
      deactivate-mark))

  ; don't error out at buffer's end
  (expect "abefXY"
    (with-short-scoot-test-buffer
      ; XXX why doesn't dotimes work here? it seems to only call once.
      (call-interactively 'spiffy-tm-scoot-right)
      (call-interactively 'spiffy-tm-scoot-right)
      (call-interactively 'spiffy-tm-scoot-right)
      (buffer-string)))

  (desc "scoot left")
  (expect "XYabef"
    (with-short-scoot-test-buffer
      (call-interactively 'spiffy-tm-scoot-left)
      (call-interactively 'spiffy-tm-scoot-left)
      (buffer-string)))

  (expect "XYabef"       ; watch for beginning of buffer
    (with-short-scoot-test-buffer
      (call-interactively 'spiffy-tm-scoot-left)
      (call-interactively 'spiffy-tm-scoot-left)
      (call-interactively 'spiffy-tm-scoot-left)
      (buffer-string)))

  (desc "scoot up")
  (expect "*\nabXYdf\n*\n*\n*\n*\n*"
    (with-long-scoot-test-buffer
     (call-interactively 'spiffy-tm-scoot-up)
     (call-interactively 'spiffy-tm-scoot-up)
     (buffer-string)))

  (expect "abXYdf\n*\n*\n*\n*\n*\n*"
    (with-long-scoot-test-buffer
     (call-interactively 'spiffy-tm-scoot-up)
     (call-interactively 'spiffy-tm-scoot-up)
     (call-interactively 'spiffy-tm-scoot-up)
     (call-interactively 'spiffy-tm-scoot-up)
     (buffer-string)))

  ; no active region
  (expect "*\nabXYdf\n*\n*\n*\n*\n*"
    (with-long-scoot-test-buffer
     (deactivate-mark)
     (call-interactively 'spiffy-tm-scoot-up)
     (call-interactively 'spiffy-tm-scoot-up)
     (buffer-string)))

  (expect "abc"     ; watch out for being on the last line in the buffer
    (with-temp-buffer
      (insert "abc")
      (goto-char (point-min))
      (call-interactively 'spiffy-tm-scoot-down)
      (buffer-string)))

  (desc "scoot down")
  (expect "*\n*\n*\n*\n*\nabXYdf\n*"
    (with-long-scoot-test-buffer
     (call-interactively 'spiffy-tm-scoot-down)
     (call-interactively 'spiffy-tm-scoot-down)
     (buffer-string)))

  (expect "*\n*\n*\n*\n*\n*abXYdf\n"     ; hit eob
    (with-long-scoot-test-buffer
     (call-interactively 'spiffy-tm-scoot-down)
     (call-interactively 'spiffy-tm-scoot-down)
     (call-interactively 'spiffy-tm-scoot-down)
     (call-interactively 'spiffy-tm-scoot-down)
     (buffer-string)))

  ;; without an active region
  (expect "*\n*\n*\n*\nabXYdf\n*\n*"
    (with-long-scoot-test-buffer
     (deactivate-mark)
     (call-interactively 'spiffy-tm-scoot-down)
     (buffer-string)))

  (expect "*\n*\n*\n*\n*\nabXYdf\n*"
    (with-long-scoot-test-buffer
     (deactivate-mark)
     (call-interactively 'spiffy-tm-scoot-down)
     (call-interactively 'spiffy-tm-scoot-down)
     (buffer-string)))

  (expect nil
    (with-long-scoot-test-buffer
     (deactivate-mark)
     (call-interactively 'spiffy-tm-scoot-down)
     mark-active))

  (expect "abc"     ; watch out for being on the last line in the buffer
    (with-temp-buffer
      (insert "abc")
      (goto-char (point-min))
      (call-interactively 'spiffy-tm-scoot-down)
      (buffer-string)))

  (desc "indent when yanking")
  (expect "class Foo\n  def foo?\n    true\n  end\nend\n"
    (with-ruby-buffer
     (forward-line)
     (forward-char 2)
     (kill-line)
     (backward-delete-char-untabify 2)
     (call-interactively 'spiffy-tm-yank-and-indent)
     (buffer-string)))

  (expect "class Foo\n  def baz?\n    true\n  end\nend\n"      ; now with a prefix arg
    (with-ruby-buffer
     (kill-new "def baz?")
     (kill-new "def bar?")
     (forward-line)
     (forward-char 2)
     (kill-line)
     (backward-delete-char-untabify 2)
     (spiffy-tm-yank-and-indent 3)
     (buffer-string)))

  (expect 'yank
    (with-temp-buffer
      (setq this-command 'stuff)
      (kill-new "dontcare")
      (call-interactively 'spiffy-tm-yank-and-indent)
      this-command))

  ; yank-pop works too
  (expect "class Foo\n  def baz?\n    true\n  end\nend\n"      ; now with a prefix arg
    (with-ruby-buffer
     (setq last-command 'yank)
     (kill-new "def baz?")
     (kill-new "def bar?")
     (forward-line)
     (forward-char 2)
     (kill-line)
     (backward-delete-char-untabify 2)
     (call-interactively 'spiffy-tm-yank-and-indent)
     (call-interactively 'spiffy-tm-yank-pop-and-indent)
     (call-interactively 'spiffy-tm-yank-pop-and-indent)
     (buffer-string)))

  (expect "class Foo\n  def baz?\n    true\n  end\nend\n" ; now with a prefix arg
    (with-ruby-buffer
     (setq last-command 'yank)
     (kill-new "def baz?")
     (kill-new "def bar?")
     (forward-line)
     (forward-char 2)
     (kill-line)
     (backward-delete-char-untabify 2)
     (call-interactively 'spiffy-tm-yank-and-indent)
     (spiffy-tm-yank-pop-and-indent 2)
     (buffer-string)))
)