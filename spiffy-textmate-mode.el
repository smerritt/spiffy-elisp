(require 'spiffy)
(provide 'spiffy-textmate-mode)

; XXX test me bozo
(defun spiffy-tm-open-file-in-project ()
  (interactive)
  (find-file (spiffy-tm-pick-file-in-project)))

(defun spiffy-tm-pick-file-in-project ()
  (let*
      ((project-root (spiffy-tm-project-root-for (buffer-file-name)))
       (iswitchb-make-buflist-hook
        (lambda ()
          (setq
           iswitchb-temp-buflist
           (mapcar (lambda (x) (substring x (length project-root)))
                   (spiffy-tm-project-files-for (buffer-file-name)))))))
    (concat project-root (iswitchb-read-buffer "Open file: "))))

(defun spiffy-tm-is-project-root (directory)
  (file-exists-p (concat (file-name-as-directory directory) ".git")))

(defun spiffy-tm-project-root-for (filename)
  (let ((as-dir (file-name-as-directory filename)))
    (if (string= (file-truename as-dir) (file-truename (spiffy-parent-directory as-dir)))
        nil    ; base case
      (if (spiffy-tm-is-project-root as-dir)
          as-dir
        (spiffy-tm-project-root-for (spiffy-parent-directory filename))))))

(defun spiffy-tm-project-files-for (file)
  (filter
   (lambda (path) (not (file-directory-p path)))
   (spiffy-find-interesting-files
    (spiffy-project-root-for file)
    (lambda (f) (and
                 (not (string-match ".git$" f))
                 (not (string-match "gems$" f))
                 (not (backup-file-name-p f)))))))


;;;;;;;;;; Navigation + editing
(defmacro spiffy-tm-make-shifty-arrow (outer-function motion-function)
  `(defun ,outer-function ()
     (interactive)
     (unless mark-active
       (push-mark nil t t))    ; silently push a mark and don't whine at the user about it
     (funcall ,motion-function)))
(spiffy-tm-make-shifty-arrow spiffy-tm-arrow-up 'previous-line)
(spiffy-tm-make-shifty-arrow spiffy-tm-arrow-down 'next-line)
(spiffy-tm-make-shifty-arrow spiffy-tm-arrow-right 'forward-char)
(spiffy-tm-make-shifty-arrow spiffy-tm-arrow-left 'backward-char)

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
           (progn
             (insert ,left ,right)
             (backward-char 1))))
       (defun ,(intern (concat "spiffy-tm-right-" function-suffix)) ()
         (interactive)
         (if (looking-at (char-to-string ,right))
             (forward-char)
           (insert ,right))))))

(setq spiffy-tm-paired-characters '(
                                    (?\( ?\) "paren")
                                    (?\[ ?\] "bracket")
;;                                     (?\" ?\" "double-quote")
;;                                     (?\' ?\' "single-quote")
                                    (?\{ ?\} "curly")))

(mapcar (lambda (spec) (spiffy-tm-make-delimitizers (car spec) (cadr spec) (caddr spec))) spiffy-tm-paired-characters)
