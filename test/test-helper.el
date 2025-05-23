;;; test-helper.el --- General test utilities for cognitive-complexity.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Marie Katrine Ekeberg

;; Author: Marie Katrine Ekeberg <mke@themkat.net>

;; This program is free software; you can redistribute it and/or modify
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
;;

;;; Code:
(defmacro cognitive-complexity-test (test-name file major-mode expected-result)
  "Create a cognitive-complexity test named TEST-NAME using FILE in MAJOR-MODE.
Expecting result EXPECTED-RESULT."
  (declare (indent 1))
  `(ert-deftest ,test-name ()
    (with-current-buffer (find-file-noselect ,file)
     (,major-mode)
     (let* ((cognitive-complexity-results (cognitive-complexity-buffer))
            (total-score (car cognitive-complexity-results))
            (expressions-and-scores (cognitive-complexity-test-utils-expression-scores cognitive-complexity-results)))
      (should (equal ,expected-result
               (cons total-score
                expressions-and-scores)))))))

(defun cognitive-complexity-test-utils-expression-scores (analyze-result)
  "Helper method to get a list of simple expression + score pairs to work with."
  (mapcar (lambda (it) (cons (treesit-node-type (car it)) (nth 2 it)))
          (cdr analyze-result)))

;;; test-helper.el ends here
