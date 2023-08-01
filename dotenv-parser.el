;;; dotenv-parser.el --- Parse a dotenv file -*- lexical-binding: t -*-

;; Author: Amo DelBello
;; Maintainer: Amo DelBello
;; Version: 1.0.0
;; Package-Requires: (dependencies)
;; Homepage: homepage
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

(defun dotenv-get-lines (str)
  "Get all of the valid lines from STR.
Returns a list of matches in `(full line, key, value)` form.
Returns nil if no matches."
  (s-match-strings-all
   (rx (: (| line-start line-start)
          (* space)
          (? (: "export" (+ space)))
          (group (+ (in alnum "_" "-")))
          (| (: (* space) "=" (*? space))
             (: ":" (+? space)))
          (? (group (| (: (* space) "'" (* (| (: (syntax escape) "'") (not "'"))) "'")
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

(defun alist->hash-table (alist)
  "Convert ALIST to a hash-table."
  (let ((hash-table (make-hash-table :test 'equal)))
    (dolist (item alist)
      (puthash (car item) (nth 1 item) hash-table))
    hash-table))

(defun alist->plist (alist)
  "Convert ALIST to a plist."
  (let (value)
    (dolist (item alist value)
      (setq value (plist-put value (car item) (cadr item))))))

(defun dotenv-parse-string (dotenv-str &optional output-type)
  "Parse the DOTENV-STR into a Lisp OUTPUT-TYPE.
The returned output will be a hashtable, an alist, or a plist.
If there are duplicate keys in the DOTENV-STR, all but the last one are
ignored.

The value for OUTPUT-TYPE can be ‘hash-table’, ‘alist’ or ‘plist’.  It
defaults to ‘hash-table’."
  (interactive)
  (let* ((output-type (or output-type "hash-table"))
         (lines (dotenv-get-lines dotenv-str))
         (output))
    (dolist (item lines output)
      (let ((key (intern (nth 1 item)))
            (value (string-trim (or (nth 2 item) ""))))
        (setq output (if (assoc key output)
                         (cons (list key value)
                               (assq-delete-all key output))
                       (cons (append (list key value))
                             output)))))
    (setq output (nreverse output))
    (cond ((string= output-type "alist") output)
          ((string= output-type "plist") (alist->plist output))
          ((string= output-type "hash-table") (alist->hash-table output))
          (t (error (format "Unknown output-type: %s. Must be one of 'hash-table', 'alist', or 'plist'." output-type))))
    ))

(provide 'dotenv-parser)

;;; dotenv-parser.el ends here
