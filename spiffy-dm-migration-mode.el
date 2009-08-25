;; spiffy-dm-migration-mode: useful function(s) for writing DataMapper
;; migrations.
;;
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
(provide 'spiffy-dm-migration-mode)

(defvar spiffy-dm-migration-default-up ""
  "Default value for the up block in a new migration.")

(defvar spiffy-dm-migration-default-down ""
  "Default value for the down block in a new migration.")

(defvar *spiffy-dm-migration-keymap* (make-sparse-keymap))

(define-key *spiffy-dm-migration-keymap* [(control ?\;) ?n ?m] 'spiffy-dm-migration-new-migration)
(defun spiffy-dm-migration-new-migration ()
  "Insert a new migration at the end of the buffer."
  (interactive)
  (let ((next-migration (1+
                         (count-matches "migration [0-9]+"
                                        (point-min)
                                        (point-max))))
        (start-pos (point-max)))
    (goto-char start-pos)
    (insert "migration "
            (int-to-string next-migration)
            ", :\"what-it-does")
    (let ((between-the-quotes (point)))
      (insert "\" do\n"
              "up do\n"
              spiffy-dm-migration-default-up
              "\nend\n"
              "\n"
              "down do\n"
              spiffy-dm-migration-default-down
              "\nend\n"
              "end\n\n")
      (indent-region start-pos (point)))))

(defun spiffy-dm-migration-mode-maybe-enable ()
  "Enable spiffy-dm-migration-mode if this buffer looks like it has migrations"
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "migration"
                        (point-max)
                        t)
        (spiffy-dm-migration-mode t))))

(define-minor-mode spiffy-dm-migration-mode
  "DataMapper migration mode. Makes creating new migrations easy."
  nil
  " sDMM"
  *spiffy-dm-migration-keymap*)

