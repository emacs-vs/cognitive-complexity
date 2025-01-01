;;; typescript-test.el --- TypeScript language tests for cognitive-complexity.el  -*- lexical-binding: t; -*-

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
(require 'cognitive-complexity)

(cognitive-complexity-test typescript-simple
  "test/typescript/Simple.ts"
  typescript-ts-mode
  '(1
    ("class_declaration" . 0)
    ("method_definition" . 0)
    ("call_expression" . 0)
    ("method_definition" . 0)
    ("if_statement" . 1)))

;;; typescript-test.el ends here
