;;; spiffy-macos-mode.el: make a couple keys work like you want on the Mac
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
(provide 'spiffy-macos-mode)

(defvar *spiffy-macos-keymap* (make-sparse-keymap) "Keybindings go in here")
(define-key *spiffy-macos-keymap* [(meta s)] 'save-buffer)
(define-key *spiffy-macos-keymap* [(meta c)] 'kill-ring-save)
(define-key *spiffy-macos-keymap* [(meta v)] 'yank)
(define-key *spiffy-macos-keymap* [(meta z)] 'undo)

;; Tie it all together
(define-minor-mode spiffy-macos-mode
  "Tiny MacOS minor mode. Makes a couple keys work like you'd expect."
  t
  " Mac"
  *spiffy-macos-keymap*)
