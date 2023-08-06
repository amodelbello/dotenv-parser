;;; dot-env.el --- Dotenv functionality for emacs -*- lexical-binding: t -*-

;; Author: Amo DelBello
;; Maintainer: Amo DelBello
;; Version: 1.0.0
;; Package-Requires: ((s "1.13.0"))
;; Homepage: https://github.com/amodelbello/dot-env.el
;; Keywords: convenience, dotenv, environment, configuration


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 's)

(defvar dot-env-filepath (format "%s%s" user-emacs-directory ".env")
  "Path to the .env file.")

(defvar dot-env-environment '()
  "An alist that stores .env variables.")

(defun dot-env-get-file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun dot-env-get-lines (str)
  "Get all of the valid lines from STR.
Returns a list of matches in `((full line, key, value) ...)` form.
Returns nil if no matches."
  (s-match-strings-all
   (rx (: (| line-start line-start)
          (* space)
          (? (: "export" (+ space)))
          (group (+ (in alnum "_" "-")))
          (| (: (* space) "=" (*? space))
             (: ":" (+? space)))
          (? (group
              (| (: (* space) "'" (* (| (: (syntax escape) "'") (not "'"))) "'")
                 (: (* space) "`" (* (| (: (syntax escape) "`") (not "`"))) "`")
                 (: (* space)
                    (syntax string-quote)
                    (* (| (: (syntax escape) (syntax string-quote))
                          (not (syntax string-quote))))
                    (syntax string-quote))
                 (+ (not (in "#" "\n")))
                 (* space)
                 (? (: "#" (* nonl)))
                 (| line-end line-end))))))
   str))

(defun dot-env-clense-value (raw-value)
  "Remove containing quotes and trim whitespace in RAW-VALUE."
  (replace-regexp-in-string                                     ; remove outside quotes
   "^\\(['\"`]\\)\\([[:ascii:]\\|[:nonascii:]]*\\)\\1"
   "\\2"
   (string-trim (or raw-value ""))))                            ; trim whitespace


(defun dot-env-parse (dotenv-str)
  "Parse DOTENV-STR."
  (interactive)
  (let ((lines (dot-env-get-lines dotenv-str))
        (output))
    (dolist (item lines output)
      (let ((key (intern (nth 1 item)))
            (value (dot-env-clense-value (nth 2 item))))
        (setq output (if (assoc key output)
                         (cons (list key value)
                               (assq-delete-all key output))
                       (cons (append (list key value))
                             output)))))
    (setq output (nreverse output))))

(defun dot-env-config (&optional path)
  "Load the values from file located at PATH and return them.
PATH defaults to `user-emacs-directory'/.env."
  (interactive)
  (let* ((path (or path dot-env-filepath))
         (environment (dot-env-parse (dot-env-get-file-contents path))))
    (setq dot-env-environment environment)
    environment))

(defun dot-env-populate (alist &optional override)
  "Load the values from ALIST into `dot-env-environment'.
If OVERRIDE is non-nil, override existing values.
ALIST should be in the form of '((symbol string))
Populates dot-env-environment and returns it."
  (interactive)
  (setq dot-env-environment
        (let ((override (or override nil))
              (output dot-env-environment))
          (dolist (item alist output)
            (let ((key (nth 0 item))
                  (value (dot-env-clense-value (nth 1 item))))
              (setq output (if (assoc key output)
                               (if (not (null override))
                                   (cons (list key value)
                                         (assq-delete-all key output))
                                 output)
                             (cons (append (list key value))
                                   output))))
            output))))

(defun dot-env-get (field &optional default)
  "Get the value of FIELD from dot-env-environment.
Use DEFAULT if no value is found."
  (interactive)
  (or
   (car (cdr (assoc field dot-env-environment)))
   default))

(provide 'dot-env)

;;; dot-env.el ends here
