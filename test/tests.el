;;; tests.el --- Tests for dot-env -*- lexical-binding: t -*-

;; Author: Amo DelBello
;; Maintainer: Amo DelBello
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
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

;; This packages provides the tests for `ert'.
;; Note: you must run the tests from this file after buffer evaluation.
;; Otherwise the relative paths below will not work.

;;; Code:

(ert-deftest dot-env-config ()
  "Test dot-env-config reads a file and loads environment."
  (setq old-env-filepath dot-env-filepath)
  (setq dot-env-filepath ".env-normal")
  (dot-env-config)

  ;; (setq test (dot-env-get 'TEST_VAL "default"))
  ;; test
  (should (string-equal "does not exist" (dot-env-get 'OTHER "does not exist")))
  (should (string-equal "test-val" (dot-env-get 'TEST_VAL "default")))
  (should (string-equal "test-val2" (dot-env-get 'TEST_VAL2 "default")))

  ;; reload real config
  (setq dot-env-filepath old-env-filepath)
  (dot-env-config))

;;; tests.el ends here
