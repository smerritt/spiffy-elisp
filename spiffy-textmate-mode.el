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
(spiffy-tm-define-key [(meta t)] 'spiffy-tm-open-file-in-project)
(spiffy-tm-define-key [(backspace)] 'spiffy-tm-backspace)
(spiffy-tm-define-key [(control w)] 'spiffy-tm-select-current-word-or-kill-region)
(spiffy-tm-define-key [(control K)] 'spiffy-tm-kill-entire-line)
(spiffy-tm-define-key [(meta /)] 'spiffy-tm-comment-dwim)
(spiffy-tm-define-key [(meta return)] 'spiffy-tm-put-newline-at-eol)
(spiffy-tm-define-key [(meta L)] 'spiffy-tm-select-line)    ; this function is f'in metal

; XXX test me bozo
(defun spiffy-tm-open-file-in-project ()
  "Choose a file in the current project. The project root is the directory with a .git directory in it."
  (interactive)
  (find-file (spiffy-tm-pick-file-in-project)))

(defun spiffy-tm-pick-file-in-project ()
  (let*
      ((project-root (spiffy-tm-project-root-for (buffer-file-name))))
    (concat
     project-root
     (ido-completing-read
      "Open file: "
      (mapcar (lambda (x) (substring x (length project-root)))
              (spiffy-tm-project-files-for (buffer-file-name)))))))

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
     "Move point by one character. Before moving, set the mark if there's no active mark."
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
         ; XXX test the calling of blink-paren-function
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
        (unless (eobp) (forward-char)))
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
              (unless (eobp) (forward-char))
              (point))))
    (kill-region (point-at-bol) end)))

(defun spiffy-tm-comment-dwim ()
  "Comment/uncomment either the current line or the region."
  (interactive)
  (if mark-active
      (comment-dwim nil)
    (save-excursion
      (move-beginning-of-line nil)
      (push-mark (point) t t)
      (move-end-of-line nil)
      (comment-dwim nil))))

(defun spiffy-tm-put-newline-at-eol ()
  "Go to the end of the line and insert a newline."
  (interactive)
  (move-end-of-line nil)
  (funcall (key-binding "\r")))

;;; Tie it all together
(define-minor-mode spiffy-textmate-mode "Spiffy Textmate minor mode. There are many like it, but this one is spiffy."
  nil
  " SpiffyTM"
  *spiffy-tm-keymap*)
