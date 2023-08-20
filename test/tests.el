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

;; This packages provides the tests for ert.
;; Note: you must run the tests from this file after buffer evaluation.
;; Otherwise the relative paths below will not work.

;;; Code:

(ert-deftest dot-env-file-does-not-exist ()
  "Test that non-existent file is handled properly"
  (setq old-env-filepath dot-env-filepath)
  (setq dot-env-filepath "does-not-exist")
  (setq output (dot-env-config))
  (should (s-starts-with-p "Failed to configure dotenv environment:" output))

  ;; reload real config
  (setq dot-env-filepath old-env-filepath)
  (dot-env-config))


(ert-deftest dot-env-config ()
  "Test dot-env-config reads a file and loads environment."
  (setq old-env-filepath dot-env-filepath)
  (setq dot-env-filepath ".env-normal")
  (dot-env-config)

  (should (string-equal "does not exist" (dot-env-get 'OTHER "does not exist")))
  (should (string-equal "test-val" (dot-env-get 'TEST_VAL "default")))
  (should (string-equal "test-val2" (dot-env-get 'TEST_VAL2 "default")))

  ;; reload real config
  (setq dot-env-filepath old-env-filepath)
  (dot-env-config))


(ert-deftest dot-env-config-with-quotes ()
  "Test dot-env-config reads a file containing quoted values and loads environment."
  (setq old-env-filepath dot-env-filepath)
  (setq dot-env-filepath ".env-quotes")
  (dot-env-config)

  (should (string-equal "value `with` backticks" (dot-env-get 'BACKTICKS)))
  (should (string-equal "value 'with' single quotes" (dot-env-get 'SINGLE)))
  (should (string-equal "value \"with\" double quotes" (dot-env-get 'DOUBLE)))
  (should (string-equal "value 'with' `mixed` quotes" (dot-env-get 'MIXED)))

  ;; reload real config
  (setq dot-env-filepath old-env-filepath)
  (dot-env-config))


(ert-deftest dot-env-config-with-multi-lines ()
  "Test dot-env-config reads a file containing multi-line values and loads environment."
  (setq old-env-filepath dot-env-filepath)
  (setq dot-env-filepath ".env-multi-line")
  (dot-env-config)

  (should (string-equal "one
one
one" (dot-env-get 'ONE)))
  (should (string-equal "two
two
two" (dot-env-get 'TWO)))
  (should (string-equal "three
three
three" (dot-env-get 'THREE)))

  ;; reload real config
  (setq dot-env-filepath old-env-filepath)
  (dot-env-config))


(ert-deftest dot-env-config-comments ()
  "Test dot-env-config reads a file with comments and loads environment."
  (setq old-env-filepath dot-env-filepath)
  (setq dot-env-filepath ".env-comments")
  (dot-env-config)

  (should (string-equal "" (dot-env-get 'ONE)))
  (should (string-equal "two" (dot-env-get 'TWO)))
  (should (string-equal "three #three" (dot-env-get 'THREE)))
  (should (string-equal "four #four
four" (dot-env-get 'FOUR)))

  ;; reload real config
  (setq dot-env-filepath old-env-filepath)
  (dot-env-config))


(ert-deftest dot-env-config-override ()
  "Test dot-env-config reads a file with comments and loads environment."
  (setq old-env-filepath dot-env-filepath)
  (setq dot-env-filepath ".env-normal")
  (dot-env-config)
  (setq dot-env-filepath ".env-override")
  (dot-env-config)

  (should (string-equal "overridden" (dot-env-get 'TEST_VAL)))
  (should (string-equal "test-val2" (dot-env-get 'TEST_VAL2)))

  ;; reload real config
  (setq dot-env-filepath old-env-filepath)
  (dot-env-config))


(ert-deftest dot-env-populate ()
  "Test dot-env-populate correctly populates environment"
  (setq old-env-filepath dot-env-filepath)
  (setq dot-env-filepath ".env-normal")
  (dot-env-config)

  (dot-env-populate '((NEW_VAL "new-val")))
  (should (string-equal "test-val" (dot-env-get 'TEST_VAL)))
  (should (string-equal "test-val2" (dot-env-get 'TEST_VAL2)))
  (should (string-equal "new-val" (dot-env-get 'NEW_VAL)))

  ;; reload real config
  (setq dot-env-filepath old-env-filepath)
  (dot-env-config))


(ert-deftest dot-env-populate-override ()
  "Test dot-env-populate correctly populates environment with overrides and debug"
  (setq old-env-filepath dot-env-filepath)
  (setq dot-env-filepath ".env-normal")
  (dot-env-config)

  ;; overrides with debug
  (dot-env-populate '((TEST_VAL "new-val")) t t)
  (should (string-equal "new-val" (dot-env-get 'TEST_VAL)))
  (should (string-equal "test-val2" (dot-env-get 'TEST_VAL2)))

  ;; does not override with debug
  (dot-env-populate '((TEST_VAL2 "new-val")) nil t)
  (should (string-equal "test-val2" (dot-env-get 'TEST_VAL2)))

  ;; reload real config
  (setq dot-env-filepath old-env-filepath)
  (dot-env-config))


(ert-deftest dot-env-get-encrypted ()
  "Test that file contents are not stored in the environment"
  (setq old-env-filepath dot-env-filepath)
  (setq old-env-encryption dot-env-file-is-encrypted)
  (setq dot-env-filepath ".env-normal")
  (setq dot-env-file-is-encrypted t)
  (dot-env-config)

  (should (string-equal "test-val" (dot-env-get 'TEST_VAL)))
  (should (string-equal "test-val2" (dot-env-get 'TEST_VAL2)))
  (should (string-equal "encrypted data, not stored in envrionment" dot-env-environment))

  ;; reload real config
  (setq dot-env-filepath old-env-filepath)
  (setq dot-env-file-is-encrypted old-env-encryption)
  (dot-env-config))


;;; tests.el ends here
