(require 'spiffy)
(provide 'spiffy-textmate-mode)

(defvar *spiffy-tm-keymap* (make-sparse-keymap) "Keybindings go in here")
(defun spiffy-tm-define-key (key func)
  (define-key *spiffy-tm-keymap* key func))

; builtins that normally just live on different keys
(spiffy-tm-define-key [(meta l)] 'goto-line)
; stuff that's defined in here
(spiffy-tm-define-key [(shift up)] 'spiffy-tm-arrow-up)
(spiffy-tm-define-key [(shift down)] 'spiffy-tm-arrow-down)
(spiffy-tm-define-key [(shift left)] 'spiffy-tm-arrow-left)
(spiffy-tm-define-key [(shift right)] 'spiffy-tm-arrow-right)
(spiffy-tm-define-key [(meta T)] 'spiffy-tm-open-file-in-project)
(spiffy-tm-define-key [(backspace)] 'spiffy-tm-backspace)
(spiffy-tm-define-key [(control w)] 'spiffy-tm-select-current-word-or-kill-region)
(spiffy-tm-define-key [(control K)] 'spiffy-tm-kill-entire-line)

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
    (spiffy-tm-project-root-for file)
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
  (let* ((function-suffix (eval function-suffix-form))
         (left (eval left-form))
         (right (eval right-form))
         (left-func-name (intern (concat "spiffy-tm-left-" function-suffix)))
         (right-func-name (intern (concat "spiffy-tm-right-" function-suffix))))
    `(progn
       (defun ,left-func-name (&optional start end)
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
       (defun ,right-func-name ()
         (interactive)
         (if (looking-at (char-to-string ,right))
             (forward-char)
           (insert ,right))
         ; XXX test the calling of blink-paren-function
         (if blink-paren-function (funcall blink-paren-function)))
       (spiffy-tm-define-key [,left] ',left-func-name)
       (spiffy-tm-define-key [,right] ',right-func-name))))

(setq spiffy-tm-paired-characters '(
                                    (?\( ?\) "paren")
                                    (?\[ ?\] "bracket")
;;                                     (?\" ?\" "double-quote")
;;                                     (?\' ?\' "single-quote")
                                    (?\{ ?\} "curly")))
(setq spiffy-tm-close-delimiter
      (mapcar (lambda (x) (cons (car x) (cadr x))) spiffy-tm-paired-characters))

(mapcar (lambda (spec) (spiffy-tm-make-delimitizers (car spec) (cadr spec) (caddr spec))) spiffy-tm-paired-characters)

(defun spiffy-tm-backspace ()
  (interactive)
  (if (and
       (member (char-before) (mapcar 'car spiffy-tm-paired-characters))
       (eq (cdr (assoc (char-before) spiffy-tm-close-delimiter)) (char-after)))
      (progn
        (forward-char)
        (backward-delete-char-untabify 1)))
  (backward-delete-char-untabify 1))

(defun spiffy-tm-select-word-under-point ()
  (interactive)
  (while (looking-at "\\w+")
    (forward-char))
  (push-mark (point) nil t)
  (backward-word))

(defun spiffy-tm-select-current-word-or-kill-region ()
 (interactive)
 (if mark-active
     (kill-region (region-beginning) (region-end))
   (spiffy-tm-select-word-under-point)))

(defun spiffy-tm-kill-entire-line ()
  (interactive)
  (let*
      ((start (save-excursion
                (move-beginning-of-line nil)
                (point)))
       (end (save-excursion
              (move-end-of-line nil)
              (forward-char)
              (point))))
    (kill-region start end)))
;;; Tie it all together
(define-minor-mode spiffy-textmate-mode "Spiffy Textmate minor mode. There are many like it, but this one is spiffy."
  t
  " SpiffyTM"
  *spiffy-tm-keymap*)

