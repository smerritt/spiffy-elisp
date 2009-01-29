(provide 'spiffy-textmate-mode)

; XXX test me bozo
(defun spiffy-open-file-in-project ()
  (interactive)
  (find-file (spiffy-pick-file-in-project)))

(defun spiffy-pick-file-in-project ()
  (let*
      ((project-root (spiffy-project-root-for (buffer-file-name)))
       (iswitchb-make-buflist-hook
        (lambda ()
          (setq
           iswitchb-temp-buflist
           (mapcar (lambda (x) (substring x (length project-root)))
                   (spiffy-project-files-for (buffer-file-name)))))))
    (concat project-root (iswitchb-read-buffer "Open file: "))))

(defmacro spiffy-make-shifty-arrow (outer-function motion-function)
  `(defun ,outer-function ()
     (interactive)
     (unless mark-active
       (push-mark nil t t))    ; silently push a mark and don't whine at the user about it
     (funcall ,motion-function)))
(spiffy-make-shifty-arrow spiffy-arrow-up 'previous-line)
(spiffy-make-shifty-arrow spiffy-arrow-down 'next-line)
(spiffy-make-shifty-arrow spiffy-arrow-right 'forward-char)
(spiffy-make-shifty-arrow spiffy-arrow-left 'backward-char)

(defmacro spiffy-tm-make-delimitizers (left-form right-form function-suffix-form)
  (let ((function-suffix (eval function-suffix-form))
        (left (eval left-form))
        (right (eval right-form)))
    `(progn
       (defun ,(intern (concat "spiffy-tm-left-" function-suffix)) (&optional start end)
         (interactive (if mark-active (list (region-beginning) (region-end))))
         (if start
             (progn
               (goto-char start)
               (insert ,left)
               (goto-char (1+ end))
               (insert ,right))
           (insert ,left ,right)))
       (defun ,(intern (concat "spiffy-tm-right-" function-suffix)) ()
         (interactive)
         (if (looking-at (char-to-string ,right))
             (forward-char)
           (insert ,right))))))

(setq spiffy-tm-paired-characters '(
                                    (?\( ?\) "paren")
                                    (?\[ ?\] "bracket")
                                    (?\{ ?\} "curly")
                                    (?\" ?\" "double-quote")
                                    (?\' ?\' "single-quote")))

(mapcar (lambda (spec) (spiffy-tm-make-delimitizers (car spec) (cadr spec) (caddr spec))) spiffy-tm-paired-characters)
