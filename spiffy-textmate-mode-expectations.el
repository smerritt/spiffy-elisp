(require 'el-expectations)
(require 'spiffy-textmate-mode)
(require 'fancy-pants-function-replacement)

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

  (desc "spiffy shift-arrows")
  (expect "ijk"
    (with-gibberish-buffer
     (spiffy-arrow-right)
     (spiffy-arrow-right)
     (spiffy-arrow-right)
     (buffer-substring (region-beginning) (region-end))))

  (expect "\ngh"
    (with-gibberish-buffer
     (spiffy-arrow-left)
     (spiffy-arrow-left)
     (spiffy-arrow-left)
     (buffer-substring (region-beginning) (region-end))))

  (expect "ijkl\n12"
    (with-gibberish-buffer
     (spiffy-arrow-down)
     (buffer-substring (region-beginning) (region-end))))

  (expect "cdef\ngh"
    (with-gibberish-buffer
     (spiffy-arrow-up)
     (buffer-substring (region-beginning) (region-end))))

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
     (forward-char)
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

;;   (expect "a'bcd'ef"
;;     (with-small-gibberish-buffer
;;      (forward-char)
;;      (push-mark nil t t)
;;      (forward-char 3)
;;      (spiffy-tm-left-single-quote (region-beginning) (region-end))
;;      (buffer-string)))

;;   (expect "a\"bcd\"ef"
;;     (with-small-gibberish-buffer
;;      (forward-char)
;;      (push-mark nil t t)
;;      (forward-char 3)
;;      (spiffy-tm-left-double-quote (region-beginning) (region-end))
;;      (buffer-string)))

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

;;   (expect "abc'd'ef"
;;     (with-temp-buffer
;;      (insert "abcd'ef")
;;      (goto-char 4)   ; looking at d
;;      (spiffy-tm-right-single-quote)
;;      (forward-char)
;;      (spiffy-tm-right-single-quote)
;;      (buffer-string)))
)