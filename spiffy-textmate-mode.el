;; spiffy-textmate-mode: yet another Textmate minor mode for Emacs.
;; Copyright 2009 Samuel Merritt <spam at andcheese dot org>
;; Homepage: http://github.com/smerritt/spiffy-elisp/
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; version 2 of the License.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
(require 'grep)
(require 'ido)
(require 'spiffy)
(provide 'spiffy-textmate-mode)

(defvar *spiffy-tm-keymap* (make-sparse-keymap) "Keybindings go in here")
(defun spiffy-tm-define-key (key func)
  (define-key *spiffy-tm-keymap* key func))

; builtins on different keys / one-liners
(spiffy-tm-define-key [(meta l)] 'goto-line)
(spiffy-tm-define-key [(meta delete)] (lambda () (interactive) (kill-line)))
(spiffy-tm-define-key [(meta backspace)] (lambda () (interactive) (kill-line 0)))
(spiffy-tm-define-key [(meta down)] 'end-of-buffer)
(spiffy-tm-define-key [(meta up)] 'beginning-of-buffer)
(spiffy-tm-define-key [(meta left)] (lambda () (interactive) (move-beginning-of-line nil)))
(spiffy-tm-define-key [(meta right)] (lambda () (interactive) (move-end-of-line nil)))
(spiffy-tm-define-key [(meta super ?\])] 'align)

;; stuff that's defined in here
(spiffy-tm-define-key [(shift up)] 'spiffy-tm-arrow-up)
(spiffy-tm-define-key [(shift down)] 'spiffy-tm-arrow-down)
(spiffy-tm-define-key [(shift left)] 'spiffy-tm-arrow-left)
(spiffy-tm-define-key [(shift right)] 'spiffy-tm-arrow-right)
; control shift up/down don't select text, and don't seem to do anything useful.
(spiffy-tm-define-key [(control shift left)] 'spiffy-tm-arrow-left-word)
(spiffy-tm-define-key [(control shift right)] 'spiffy-tm-arrow-right-word)
(spiffy-tm-define-key [(meta shift up)] 'spiffy-tm-arrow-bob)
(spiffy-tm-define-key [(meta shift down)] 'spiffy-tm-arrow-eob)
(spiffy-tm-define-key [(meta shift left)] 'spiffy-tm-arrow-left-line)
(spiffy-tm-define-key [(meta shift right)] 'spiffy-tm-arrow-right-line)
(spiffy-tm-define-key [(control meta up)] 'spiffy-tm-scoot-up)
(spiffy-tm-define-key [(control meta down)] 'spiffy-tm-scoot-down)
(spiffy-tm-define-key [(control meta left)] 'spiffy-tm-scoot-left)
(spiffy-tm-define-key [(control meta right)] 'spiffy-tm-scoot-right)
(spiffy-tm-define-key [(meta F)] 'spiffy-tm-grep-project)
(spiffy-tm-define-key [(meta t)] 'spiffy-tm-open-file-in-project)
(spiffy-tm-define-key [(control x) ?4 (meta t)] 'spiffy-tm-open-file-in-project-other-window)
(spiffy-tm-define-key [(control x) ?5 (meta t)] 'spiffy-tm-open-file-in-project-other-frame)
(spiffy-tm-define-key [(backspace)] 'spiffy-tm-backspace)
(spiffy-tm-define-key [(control w)] 'spiffy-tm-select-current-word-or-kill-region)
(spiffy-tm-define-key [(control K)] 'spiffy-tm-kill-entire-line)
(spiffy-tm-define-key [(control y)] 'spiffy-tm-yank-and-indent)
(spiffy-tm-define-key [(meta y)] 'spiffy-tm-yank-pop-and-indent)
(spiffy-tm-define-key [(meta /)] 'spiffy-tm-comment-dwim)
(spiffy-tm-define-key [(meta return)] 'spiffy-tm-put-newline-at-eol)
(spiffy-tm-define-key [(meta super return)] 'spiffy-tm-open-line-before)
(spiffy-tm-define-key [(meta L)] 'spiffy-tm-select-line)    ; this function is f'in metal
; NB: the balanced-delimiter keybindings get added to the keymap by
; spiffy-tm-make-delimitizers.

; XXX test me bozo
(defun spiffy-tm-open-file-in-project ()
  "Choose a file in the current project. Opens in the current window.
The project root is the directory with a .git directory in it."
  (interactive)
  (find-file (spiffy-tm-pick-file-in-project)))

(defun spiffy-tm-open-file-in-project-other-window ()
  "Choose a file in the current project. Opens in the other window.
The project root is the directory with a .git directory in it."
  (interactive)
  (find-file-other-window (spiffy-tm-pick-file-in-project)))

(defun spiffy-tm-open-file-in-project-other-frame ()
  "Choose a file in the current project. Opens in a new frame.
The project root is the directory with a .git directory in it."
  (interactive)
  (find-file-other-frame (spiffy-tm-pick-file-in-project)))

(defvar spiffy-tm-file-visited-times (make-hash-table :test 'equal)
  "Records the last-visited-time of files. Internal use only.")
(defadvice find-file-noselect (after spiffy-tm-record-file-visit-times activate)
  (let ((filename (ad-get-arg 0)))
    (puthash filename (time-to-seconds (current-time)) spiffy-tm-file-visited-times)))

(defun spiffy-tm-pick-file-in-project ()
  (let*
      ((project-root (spiffy-tm-project-root-for (buffer-file-name))))
    (if (null project-root)
        (error "Cannot find project root for buffer %s (associated with file %s)" (buffer-name) (buffer-file-name)))
    (concat
     project-root
     (ido-completing-read
      "Open file: "
      (mapcar (lambda (x) (substring x (length project-root)))
              (sort (spiffy-tm-project-files-for (buffer-file-name))
                    ; most-recently-visited first except that the current buffer shouldn't top the list
                    (lambda (a b)
                      (let ((aval (if (equal (buffer-file-name) a)
                                      0
                                    (gethash a spiffy-tm-file-visited-times 0)))
                            (bval (if (equal (buffer-file-name) b)
                                      0
                                    (gethash b spiffy-tm-file-visited-times 0))))
                        (> aval bval)))))))))

(defun spiffy-tm-grep-project (regexp)
  (interactive
   (list (grep-read-regexp)))
  (grep-compute-defaults)      ; rgrep only does this when called interactively
  (rgrep regexp "*" (spiffy-tm-project-root-for (buffer-file-name))))

(defun spiffy-tm-is-project-root (directory)
  (file-exists-p (concat (file-name-as-directory directory) ".git")))

(defun spiffy-tm-project-root-for (filename)
  (if (null filename)
      nil
    (let ((as-dir (file-name-as-directory filename)))
      (if (string= (file-truename as-dir) (file-truename (spiffy-parent-directory as-dir)))
          nil    ; base case
        (if (spiffy-tm-is-project-root as-dir)
            as-dir
          (spiffy-tm-project-root-for (spiffy-parent-directory filename)))))))

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
     "Move point as you'd expect. Before moving, set the mark if it's not active."
     (interactive)
     (unless mark-active
       (push-mark nil t t))    ; silently push a mark and don't whine at the user about it
     (funcall ,motion-function)))

(spiffy-tm-make-shifty-arrow spiffy-tm-arrow-up 'previous-line)
(spiffy-tm-make-shifty-arrow spiffy-tm-arrow-down 'next-line)
(spiffy-tm-make-shifty-arrow spiffy-tm-arrow-right 'forward-char)
(spiffy-tm-make-shifty-arrow spiffy-tm-arrow-left 'backward-char)
(spiffy-tm-make-shifty-arrow spiffy-tm-arrow-right-word 'forward-word)
(spiffy-tm-make-shifty-arrow spiffy-tm-arrow-left-word 'backward-word)
(spiffy-tm-make-shifty-arrow spiffy-tm-arrow-left-line (lambda () (move-beginning-of-line nil)))
(spiffy-tm-make-shifty-arrow spiffy-tm-arrow-right-line (lambda () (move-end-of-line nil)))
(spiffy-tm-make-shifty-arrow spiffy-tm-arrow-bob (lambda () (goto-char (point-min))))
(spiffy-tm-make-shifty-arrow spiffy-tm-arrow-eob (lambda () (goto-char (point-max))))

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
         (if blink-paren-function (funcall blink-paren-function)))
       (spiffy-tm-define-key [,left] ',left-func-name)
       (spiffy-tm-define-key [,right] ',right-func-name))))

(setq spiffy-tm-paired-characters '(
                                    (?\( ?\) "paren")
                                    (?\[ ?\] "bracket")
                                    (?\{ ?\} "curly")))

; make an alist out of the balanced delimiters
(setq spiffy-tm-close-delimiter
      (mapcar (lambda (x) (cons (car x) (cadr x))) spiffy-tm-paired-characters))

(mapcar (lambda (spec) (spiffy-tm-make-delimitizers (car spec) (cadr spec) (caddr spec))) spiffy-tm-paired-characters)

(defun spiffy-tm-backspace (&optional arg)
  "Delete the region (if active), else delete balanced (), [], etc (if in the middle), else delete a character."
  (interactive "p")
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (if (and
         (member (char-before) (mapcar 'car spiffy-tm-paired-characters))
         (eq (cdr (assoc (char-before) spiffy-tm-close-delimiter)) (char-after)))
        (progn
          (forward-char)
          (backward-delete-char-untabify 1)))
    (backward-delete-char-untabify 1))
  (if (and arg (< 1 arg))
      (spiffy-tm-backspace (1- arg))))

(defun spiffy-tm-select-word-under-point ()
  (while (looking-at "\\w+")
    (forward-char))
  (push-mark (point) nil t)
  (backward-word))

(defun spiffy-tm-select-line ()
  (interactive)
  (if mark-active
      (progn
        (if (< (point) (mark))
            (exchange-point-and-mark))
        (let ((start (point)))
          (goto-char (mark))
          (set-mark (point-at-bol))   ;; yes, the dreaded set-mark! damn your warnings!
          (goto-char start))
        (end-of-line)
        (spiffy-tm-safe-forward-char))
    (progn
      (push-mark (point-at-bol) t t)
      (end-of-line))))

(defun spiffy-tm-select-current-word-or-kill-region ()
  "If there is no active mark, select the word under point (Textmate behavior).
If the mark is active, kill the region (Emacs behavior)."
  (interactive)
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (spiffy-tm-select-word-under-point)))

(defun spiffy-tm-kill-entire-line ()
  "Kill the entire current line."
  (interactive)
  (let*
      ((end (save-excursion
              (move-end-of-line nil)
              (spiffy-tm-safe-forward-char)
              (point))))
    (kill-region (point-at-bol) end)))

(defun spiffy-tm-comment-dwim ()
  "Comment/uncomment either the current line or the region."
  (interactive)
  (if mark-active
      (comment-dwim nil)
    (progn
      (save-excursion
        (push-mark (point-at-bol) t t)
        (move-end-of-line nil)
        (comment-dwim nil))
      (move-end-of-line nil)
      (spiffy-tm-safe-forward-char))))

(defun spiffy-tm-put-newline-at-eol ()
  "Go to the end of the line and insert a newline."
  (interactive)
  (move-end-of-line nil)
  (funcall (key-binding "\r")))

(defun spiffy-tm-open-line-before ()
  (interactive)
  (move-beginning-of-line nil)
  (open-line 1)
  (indent-according-to-mode))


(defun spiffy-tm-yank-and-indent (arg)
  "Reinsert and then indent the last stretch of killed text."
  (interactive "p")
  (spiffy-tm-yank-with 'yank arg))

(defun spiffy-tm-yank-pop-and-indent (arg)
  "Replace just-yanked stretch of killed text with a different stretch.
After replacing, indent it."
  (interactive "p")
  (spiffy-tm-yank-with 'yank-pop arg))

(defun spiffy-tm-yank-with (yanker arg)
  (let ((mark-even-if-inactive t))
    (funcall yanker arg)
    (indent-region (region-beginning) (region-end)))
  (setq this-command 'yank))

;; scoot
(defun spiffy-tm-scoot (start end cant-move move)
  (let ((text (buffer-substring start end))
        (mark-was-active mark-active))
    (delete-region start end)
    (unless (funcall cant-move) (funcall move))
    (let ((new-start (point)))
      (insert text)
      (if mark-was-active (set-mark (point)))
      (goto-char new-start)))
  (setq deactivate-mark nil))

(defun spiffy-tm-scoot-right (start end)
  "Move the region one character to the right."
  (interactive "r")
  (spiffy-tm-scoot start end 'eobp 'forward-char))

(defun spiffy-tm-scoot-left (start end)
  "Move the region one character to the left."
  (interactive "r")
  (spiffy-tm-scoot start end 'bobp 'backward-char))

(defun spiffy-tm-scoot-up (&optional start end)
  "Move the region or current line up one line."
  (interactive (if mark-active (list (region-beginning) (region-end))))
  (spiffy-tm-scoot
   (or start (point-at-bol))
   (or end (if (= (point-at-eol) (point-max))
               (point-at-eol)
             (1+ (point-at-eol))))
   (lambda () (= (line-number-at-pos (point)) (line-number-at-pos (point-min))))
   (lambda () (forward-line -1))))

(defun spiffy-tm-scoot-down (&optional start end)
  "Move the region or current line down one line."
  (interactive (if mark-active (list (region-beginning) (region-end))))
  (spiffy-tm-scoot
   (or start (point-at-bol))
   (or end (if (= (point-at-eol) (point-max))
               (point-at-eol)
             (1+ (point-at-eol))))
   (lambda () nil)
   (lambda ()
     (if (= (line-number-at-pos (point)) (line-number-at-pos (point-max)))
         (goto-char (point-max))
       (forward-line)))))

;;; Utility functions
(defun spiffy-tm-safe-forward-char ()
  (unless (eobp) (forward-char)))

;;; Tie it all together
(define-minor-mode spiffy-textmate-mode "Spiffy Textmate minor mode. There are many like it, but this one is spiffy."
  nil
  " sTM"
  *spiffy-tm-keymap*)
