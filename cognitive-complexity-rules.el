;;; cognitive-complexity-rules.el --- Define scoring rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Shen, Jen-Chieh
;; Copyright (C) 2024  Abdelhak BOUGOUFFA

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Here we list of all scoring rules for all programming languages.
;;

;;; Code:

;;
;; (@* "Externals" )
;;

;; TODO(everyone): keep the forward declared alphabetically sorted

(declare-function cc-rules--class-declaration "cognitive-complexity.el")
(declare-function cc-rules--method-declaration "cognitive-complexity.el")
(declare-function cc-rules--logical-operators "cognitive-complexity.el")
(declare-function cc-rules--method-declaration-using-node-name "cognitive-complexity.el")
(declare-function cc-rules--operators "cognitive-complexity.el")
(declare-function cc-rules--recursion "cognitive-complexity.el")
(declare-function cc-rules--recursion-using-node-name "cognitive-complexity.el")
(declare-function cc-rules--elixir-call "cognitive-complexity.el")
(declare-function cc-rules--elisp-special-form "cognitive-complexity.el")
(declare-function cc-rules--elisp-list "cognitive-complexity.el")
(declare-function cc-rules--java-outer-loop "cognitive-complexity.el")
(declare-function cc-rules--kotlin-outer-loop "cognitive-complexity.el")
(declare-function cc-rules--kotlin-elvis-operator "cognitive-complexity.el")
(declare-function cc-rules--kotlin-function-declaration "cognitive-complexity.el")
(declare-function cc-rules--kotlin-recursion "cognitive-complexity.el")
(declare-function cc-rules--julia-macro-expression "cognitive-complexity.el")
(declare-function cc-rules--lua-binary-expressions "cognitive-complexity.el")
(declare-function cc-rules--ruby-binary "cognitive-complexity.el")
(declare-function cc-rules--rust-outer-loop "cognitive-complexity.el")
(declare-function cc-rules--scala-call-expression "cognitive-complexity.el")

;;
;; (@* "Rules" )
;;

;; TODO(everyone): keep the function alphabetically sorted

(defun cc-rules-bash ()
  "Return rules for Bash."
  `((function_definition . (lambda (node depth nested) (cc-rules--method-declaration-using-node-name node depth nested "word")))
    (if_statement        . (1 t))
    (while_statement     . (1 t))
    (for_statement       . (1 t))
    (command             . (lambda (node &rest _) (cc-rules--recursion-using-node-name node "command_name")))))

(defun cc-rules-c ()
  "Return rules for C."
  `((function_definition . (lambda (node depth nested)
                             ;; C has the identifier inside the function_declarator node, which is always inside a function_definition for valid C code
                             (let ((func-decl-node (car (cc--find-children-by-type node "function_declarator"))))
                              (cc-rules--method-declaration func-decl-node depth nested))))
    (lambda_expression   . (1 t))
    (preproc_ifdef       . (1 t))  ; macro
    (if_statement        . (1 t))
    (switch_statement    . (1 t))
    (while_statement     . (1 t))
    (for_statement       . (1 t))
    (do_statement        . (1 t))
    ("&&"                . cc-rules--logical-operators)
    ("||"                . cc-rules--logical-operators)
    (goto_statement      . (1 t))
    (call_expression     . cc-rules--recursion)))

(defun cc-rules-c++ ()
  "Return rules for C++."
  (append
   (cc-rules-c)
   `((class_declaration   . cc-rules--class-declaration)
     (catch_clause        . (1 t)))))

(defun cc-rules-csharp ()
  "Return rules for C#."
  `((class_declaration     . cc-rules--class-declaration)
    (method_declaration    . cc-rules--method-declaration)
    (lambda_expression     . (0 t))  ; don't score, but increase nested level
    (if_statement          . (1 t))
    (switch_statement      . (1 t))
    (while_statement       . (1 t))
    (for_statement         . (1 t))
    (foreach_statement     . (1 t))
    (do_statement          . (1 t))
    (catch_clause          . (1 t))
    (finally_clause        . (1 t))
    ("&&"                  . cc-rules--logical-operators)
    ("||"                  . cc-rules--logical-operators)
    (invocation_expression . cc-rules--recursion)))

(defun cc-rules-elixir ()
  "Return rules for Elixir."
  `((call . cc-rules--elixir-call)))

(defun cc-rules-elisp ()
  "Return rules for Emacs Lisp."
  `((function_definition . cc-rules--method-declaration)
    (special_form        . cc-rules--elisp-special-form)
    (list                . cc-rules--elisp-list)))

(defun cc-rules-go ()
  "Return rules for Go."
  `((function_declaration        . cc-rules--method-declaration)
    (method_declaration          . cc-rules--method-declaration)
    (func_literal                . (0 t))  ; don't score, but increase nested level
    (if_statement                . (1 t))
    (expression_switch_statement . (1 t))
    (for_statement               . (1 t))
    ("&&"                        . cc-rules--logical-operators)
    ("||"                        . cc-rules--logical-operators)
    (call_expression             . cc-rules--recursion)))

(defun cc-rules-java ()
  "Return rules for Java."
  `((class_declaration  . cc-rules--class-declaration)
    (method_declaration . cc-rules--method-declaration)
    (lambda_expression  . (0 t))  ; don't score, but increase nested level
    (if_statement       . (1 t))
    (switch_expression  . (1 t))
    (while_statement    . (1 t))
    (for_statement      . (1 t))
    (do_statement       . (1 t))
    (catch_clause       . (1 t))
    (finally_clause     . (1 t))
    ("&&"               . cc-rules--logical-operators)
    ("||"               . cc-rules--logical-operators)
    (break_statement    . cc-rules--java-outer-loop)
    (continue_statement . cc-rules--java-outer-loop)
    (method_invocation  . cc-rules--recursion)))

(defun cc-rules-javascript ()
  "Return rules for JavaScript."
  `((function_declaration . cc-rules--method-declaration)
    (function_expression  . (0 t))  ; traditional anonymous function
    (arrow_function       . (0 t))  ; don't score, but increase nested level
    (if_statement         . (1 t))
    (switch_statement     . (1 t))
    (while_statement      . (1 t))
    (for_statement        . (1 t))
    (do_statement         . (1 t))
    (catch_clause         . (1 t))
    (finally_clause       . (1 t))
    ("&&"                 . cc-rules--logical-operators)
    ("||"                 . cc-rules--logical-operators)
    (call_expression      . cc-rules--recursion)))

(defun cc-rules-julia ()
  "Return rules for Julia."
  `((function_definition . cc-rules--method-declaration)
    (if_statement        . (1 t))
    (while_statement     . (1 t))
    (for_statement       . (1 t))
    (catch_clause        . (1 t))
    (finally_clause      . (1 t))
    ("&&"                . cc-rules--logical-operators)
    ("||"                . cc-rules--logical-operators)
    (macro_expression    . cc-rules--julia-macro-expression)
    (call_expression     . cc-rules--recursion)))

(defun cc-rules-kotlin ()
  "Return rules for Kotlin."
  `((class_declaration    . cc-rules--class-declaration)
    (object_declaration   . cc-rules--class-declaration)
    (function_declaration . (lambda (node depth nested) (cc-rules--method-declaration-using-node-name node depth nested simple_identifier)))
    (lambda_literal       . (0 t))  ; don't score, but increase nested level
    (anonymous_function   . (0 t))  ; should in theory have same effect as lambda
    (if_expression        . (1 t))
    (when_expression      . (1 t))
    (while_statement      . (1 t))
    (do_while_statement   . (1 t))
    (for_statement        . (1 t))
    (catch_block          . (1 t))
    (finally_block        . (1 t))
    ("&&"                 . cc-rules--logical-operators)
    ("||"                 . cc-rules--logical-operators)
    ("?:"                 . (lambda (node &rest _) (cc-rules--operators node '("?:"))))
    (break                . cc-rules--kotlin-outer-loop)
    (continue             . cc-rules--kotlin-outer-loop)
    (call_expression      . (lambda (node &rest _) (cc-rules--recursion-using-node-name node "simple_identifier")))))

(defun cc-rules-lua ()
  "Return rules for Lua."
  `((function_declaration . cc-rules--method-declaration)
    (if_statement         . (1 t))
    (while_statement      . (1 t))
    (for_statement        . (1 t))
    (repeat_statement     . (1 t))
    (binary_expression    . cc-rules--lua-binary-expressions)
    (goto_statement       . (1 nil))
    (function_call        . cc-rules--recursion)))

(defun cc-rules-php ()
  "Return rules for PHP."
  `((class_declaration        . cc-rules--class-declaration)
    (function_definition      . cc-rules--method-declaration)
    (if_statement             . (1 t))
    (switch_statement         . (1 t))
    (while_statement          . (1 t))
    (for_statement            . (1 t))
    (do_statement             . (1 t))
    (catch_clause             . (1 t))
    (finally_clause           . (1 t))
    ("&&"                     . cc-rules--logical-operators)
    ("||"                     . cc-rules--logical-operators)
    (break_statement          . cc-rules--java-outer-loop)
    (continue_statement       . cc-rules--java-outer-loop)
    (goto_statement           . (1 nil))
    (function_call_expression . cc-rules--recursion)))

(defun cc-rules-python ()
  "Return rules for Python."
  `((class_definition    . cc-rules--class-declaration)
    (function_definition . cc-rules--method-declaration)
    (lambda              . (0 t))
    (if_statement        . (1 t))
    (while_statement     . (1 t))
    (for_statement       . (1 t))
    (except_clause       . (1 t))
    (finally_clause      . (1 t))
    (boolean_operator    . cc-rules--logical-operators)
    (raise_statement     . (lambda (&rest _) (cc-with-metrics '(1 nil) '(0 nil))))
    (call                . cc-rules--recursion)))

(defun cc-rules-ruby ()
  "Return rules for Ruby."
  `((class    . cc-rules--class-declaration)
    (method   . cc-rules--method-declaration)
    (lambda   . (0 t))  ; don't score, but increase nested level
    (if       . (1 t))
    (while    . (1 t))
    (for      . (1 t))
    (do_block . (1 t))
    (until    . (1 t))
    (binary   . cc-rules--ruby-binary)
    (throw    . (1 nil))
    (call     . cc-rules--recursion)))

(defun cc-rules-rust ()
  "Return rules for Rust."
  `((function_item       . cc-rules--method-declaration)
    (closure_expression  . (0 t))
    (if_expression       . (1 t))
    (match_expression    . (1 t))
    (while_expression    . (1 t))
    (for_expression      . (1 t))
    (loop_expression     . (1 t))
    (call_expression     . cc-rules--recursion)
    ("&&"                . cc-rules--logical-operators)
    ("||"                . cc-rules--logical-operators)
    (break_expression    . cc-rules--rust-outer-loop)
    (continue_expression . cc-rules--rust-outer-loop)
    (macro_invocation    . cc-rules--recursion)))

(defun cc-rules-scala ()
  "Return rules for Scala."
  `((class_definition     . cc-rules--class-declaration)
    (function_declaration . cc-rules--method-declaration)
    (function_definition  . cc-rules--method-declaration)
    ;; XXX: Scala lambda is very hard to parse, and it has a lot of nested
    ;; level... not quite sure how to improve this!
    (infix_expression     . cc-rules--scala-infix-expression)
    (if_expression        . (1 t))
    (match_expression     . (1 t))  ; switch statement
    (catch_clause         . (1 t))
    (finally_clause       . (1 t))
    (operator_identifier  . cc-rules--logical-operators)
    (call_expression      . cc-rules--scala-call-expression)))

(defun cc-rules-swift ()
  "Return rules for Swift."
  `((class_declaration      . cc-rules--class-declaration)
    (function_declaration   . cc-rules--method-declaration)
    (if_statement           . (1 t))
    (while_statement        . (1 t))
    (for_statement          . (1 t))
    (repeat_while_statement . (1 t))
    (catch_clause           . (1 t))
    ("&&"                   . cc-rules--logical-operators)
    ("||"                   . cc-rules--logical-operators)
    (tuple                  . cc-rules--recursion)))

(defun cc-rules-typescript ()
  "Return rules for TypeScript."
  (append
   (cc-rules-javascript)
   `((class_declaration . cc-rules--class-declaration)
     (method_definition . cc-rules--method-declaration))))


(provide 'cognitive-complexity-rules)
;;; cognitive-complexity-rules.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("cc-" . "cognitive-complexity-"))
;; End:
