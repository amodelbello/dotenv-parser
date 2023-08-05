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

(defun dot-env-parse-string (dotenv-str)
  "Parse the DOTENV-STR."
  (interactive)
  (let* ((lines (dot-env-get-lines dotenv-str))
         (output))
    (dolist (item lines output)
      (let* ((key (intern (nth 1 item)))
             (value (replace-regexp-in-string                   ; remove outside quotes
                     "^\\(['\"`]\\)\\([[:ascii:]\\|[:nonascii:]]*\\)\\1"
                     "\\2"
                     (string-trim (or (nth 2 item) "")))))      ; trim whitespace
        (setq output (if (assoc key output)
                         (cons (list key value)
                               (assq-delete-all key output))
                       (cons (append (list key value))
                             output)))))
    (setq output (nreverse output))))

(provide 'dot-env-parser)

;;; dot-env.el ends here
