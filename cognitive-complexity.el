;;; cognitive-complexity.el --- Minor mode to show the cognitive complexity of code  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Shen, Jen-Chieh
;; Copyright (C) 2024  Abdelhak BOUGOUFFA

;; Author: Abdelhak BOUGOUFFA
;; Maintainer: Abdelhak BOUGOUFFA
;; URL: https://github.com/abougouffa/cognitive-complexity
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience complexity

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

;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'rect)
(require 'treesit)

(require 'cognitive-complexity-rules)

(defgroup cognitive-complexity nil
  "Plugin shows complexity information."
  :prefix "cognitive-complexity-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/cognitive-complexity"))

(defcustom cc-metric 'cognitive
  "The metric types for complexity computation."
  :type '(choice
          (const :tag "Cognitive Complexity" cognitive)
          (const :tag "Cyclomatic Complexity" cyclomatic))
  :group 'cognitive-complexity)

(defcustom cc-rules
  `((c-mode             . ,(cc-rules-c))
    (c-ts-mode          . ,(cc-rules-c))
    (c++-mode           . ,(cc-rules-c++))
    (c++-ts-mode        . ,(cc-rules-c++))
    (csharp-mode        . ,(cc-rules-csharp))
    (csharp-ts-mode     . ,(cc-rules-csharp))
    (elixir-mode        . ,(cc-rules-elixir))
    (elixir-ts-mode     . ,(cc-rules-elixir))
    (emacs-lisp-mode    . ,(cc-rules-elisp))
    (go-mode            . ,(cc-rules-go))
    (go-ts-mode         . ,(cc-rules-go))
    (go-mod-ts-mode     . ,(cc-rules-go))
    (java-mode          . ,(cc-rules-java))
    (java-ts-mode       . ,(cc-rules-java))
    (javascript-mode    . ,(cc-rules-javascript))
    (js-ts-mode         . ,(cc-rules-javascript))
    (js2-mode           . ,(cc-rules-javascript))
    (js3-mode           . ,(cc-rules-javascript))
    (julia-mode         . ,(cc-rules-julia))
    (julia-ts-mode      . ,(cc-rules-julia))
    (kotlin-mode        . ,(cc-rules-kotlin))
    (kotlin-ts-mode     . ,(cc-rules-kotlin))
    (lua-mode           . ,(cc-rules-lua))
    (lua-ts-mode        . ,(cc-rules-lua))
    (php-mode           . ,(cc-rules-php))
    (python-mode        . ,(cc-rules-python))
    (python-ts-mode     . ,(cc-rules-python))
    (rjsx-mode          . ,(cc-rules-javascript))
    (ruby-mode          . ,(cc-rules-ruby))
    (ruby-ts-mode       . ,(cc-rules-ruby))
    (rust-mode          . ,(cc-rules-rust))
    (rust-ts-mode       . ,(cc-rules-rust))
    (rustic-mode        . ,(cc-rules-rust))
    (sh-mode            . ,(cc-rules-bash))
    (bash-ts-mode       . ,(cc-rules-bash))
    (scala-mode         . ,(cc-rules-scala))
    (scala-ts-mode      . ,(cc-rules-scala))
    (swift-mode         . ,(cc-rules-swift))
    (typescript-mode    . ,(cc-rules-typescript))
    (typescript-ts-mode . ,(cc-rules-typescript)))
  "An alist of (major-mode . (node-type . weight)).

WEIGHT is used to determine the final score."
  :type '(alist :key-type symbol
          :value-type (alist :key-type symbol :value-type function))
  :group 'cognitive-complexity)

(defcustom cc-100-percent-score 8.0
  "The score represnet 100 percent."
  :type 'float
  :group 'cognitive-complexity)

;;
;; (@* "Logger" )
;;

(defvar cc--show-log nil
  "Get more information from the program.")

(defun cc--log (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when cc--show-log
    (apply 'message fmt args)))

(defvar cc--recursion-identifier nil
  "Record recursion identifier for increment.")

(defvar cc--recursion-identifier-depth 0
  "Record recursion identifier depth to avoid recording recursion outside function.")

;;
;; (@* "Util" )
;;

(defmacro cc--with-treesit (&rest body)
  "Run BODY only if `tree-sitter-mode` is enabled."
  (declare (indent 0))
  `(if (and (treesit-available-p) (treesit-parser-list) t)
       (progn ,@body)
     (message "Ignoring block since `treesit' isn't available for this buffer!")))

(defmacro cc--with-current-visible-buffer (buffer-or-name &rest body)
  "Safely execute BODY when BUFFER-OR-NAME is displayed."
  (declare (indent 1))
  `(when (get-buffer-window ,buffer-or-name)
    (with-current-buffer ,buffer-or-name ,@body)))

(defun cc--count-matches (regexps str &optional start end)
  "Count matches of REGEXPS in STR, with optional START and END."
  (let ((count 0))
    (dolist (regexp (ensure-list regexps))
      (cl-incf count (save-match-data ; stolen from `s-count-matches'
                       (with-temp-buffer
                         (insert str)
                         (goto-char (point-min))
                         (count-matches regexp (or start 1) (or end (point-max)))))))
    count))

(defun cc--is-parent-p (node child)
  "Return non-nil if NODE is a parent of CHILD."
  (treesit-parent-until child (lambda (n) (and n (treesit-node-eq node n)))))

(defun cc--find-children-by-type (node &rest types)
  "Return a list of NODE's children of TYPES."
  (treesit-filter-child node (lambda (child) (not (member (treesit-node-type child) types)))))

(defun cc--find-parent-by-type (node &rest types)
  "Find the nearest NODE parent of TYPES."
  (treesit-parent-until node (lambda (n) (member (treesit-node-type n) types))))

;;
;; (@* "Core" )
;;

(defmacro cc-with-metrics (cogn-metric cycl-metric)
  "Execute conditions by variable `cc-metric'.
All arguments COGN-METRIC and CYCL-METRIC are followed by variable
`cc-metric'."
  (declare (indent 0))
  `(cl-case cc-metric
    (cognitive  ,cogn-metric)
    (cyclomatic ,cycl-metric)
    (t (user-error "Unknown complexity metric %s" cc-metric))))

(defun cc-percentage (score)
  "Calculate percentage from SCORE."
  (floor (* (/ score cc-100-percent-score) 100.0)))

(defun cc--rules (&optional mode)
  "Return rules from major (MODE)."
  (mapcar ; Convert symbols to strings, this will help stealing rules from `codemetrics'!
   (lambda (rule) (cons (let ((types (car rule))) (if (symbolp types) (symbol-name types) types))
                        (cdr rule)))
   (cdr (assq (or mode major-mode) cc-rules))))

(defun cc--children-depth-first-mapc (func node &optional depth)
  "Apply FUNC to all NODE's children in a depth-first manner.
Optional argument DEPTH is used for recursive depth calculation."
  (let ((depth (or depth 0)))
    (treesit-filter-child
     node
     (lambda (node)
       (cl-incf depth)
       (funcall func node depth)
       (cc--children-depth-first-mapc func node depth)
       (cl-decf depth)
       nil))))

(defun cc--accumulate (report)
  "Accumulate the score and add the information to the REPORT."
  (let ((score (car report))
        (data (cdr report))
        (new-data))
    (while data
      (let* ((current (pop data))
             (current-node (nth 0 current))
             (current-depth (nth 1 current))
             (accumulate-score 0)
             (break)
             (index 0))
        (while (and (not break)
                    (< index (length data)))
          (let* ((it         (nth index data))
                 (node       (nth 0 it))
                 (depth      (nth 1 it))
                 (node-score (nth 2 it)))
            (if (and (< current-depth depth)
                     (cc--is-parent-p current-node node))
                (cl-incf accumulate-score node-score)
              (setq break t)))
          (cl-incf index))
        (push (append current `(,accumulate-score)) new-data)))
    (cons score (reverse new-data))))

;;;###autoload
(defun cognitive-complexity-analyze (content &optional mode)
  "Analyze CONTENT in major (MODE), the code."
  (cc--with-treesit
   (let* ((mode (or mode major-mode))
          (rules (cc--rules mode))
          ;; Collection of nesting levels
          (nested-depths)
          (nested 0)
          (score 0)
          (data)
          ;; Helper for calculating nested value from our collection of nestings
          (calculate-nested-value (lambda (nested-depths) (max 0 (1- (length nested-depths)))))
          ;; Global Records
          (cc--recursion-identifier)
          (lang (treesit-language-at (point-min))))
     (with-current-buffer (get-buffer-create (format " *%s: cognitive-complexity*" (buffer-name)))
       (delete-region (point-min) (point-max))
       (insert content)
       (delay-mode-hooks (funcall mode))
       (treesit-parser-create lang)
       (cc--children-depth-first-mapc
        (lambda (node depth)
          ;; Handle recursion names and depth to avoid global calls
          ;;  counting as recursion (like calling a function outside
          ;;  a function in bash).
          (when (and cc--recursion-identifier
                     (<= depth cc--recursion-identifier-depth))
            (setq cc--recursion-identifier nil))
          ;; Decrement out if needed (if we have moved out of the last nesting)
          (setq nested-depths
                ;; Replacement of `-drop-while'
                (seq-subseq nested-depths (or (cl-position-if-not (lambda (nested) (<= depth nested)) nested-depths) 0))
                nested (funcall calculate-nested-value nested-depths))
          (when-let* ((types (treesit-node-type node))
                      (a-rule (assoc types rules))  ; cons
                      (rule (cdr a-rule)))
            (let* ((rules-data (if (functionp rule)
                                   (funcall rule node depth nested)
                                 rule))
                   (weight     (nth 0 rules-data))
                   (inc-nested (nth 1 rules-data)))
              (when inc-nested
                (let ((last-depth (or (nth 0 nested-depths)
                                      depth)))
                  (when (or (< last-depth depth)
                            (zerop (length nested-depths)))
                    (push depth nested-depths)
                    (setq nested (funcall calculate-nested-value nested-depths)))))
              (cc--log "depth: %s, nested-depths: %s, nested: %s"
                       depth nested-depths nested)
              (let ((node-score (if inc-nested (+ weight nested) weight)))
                (cc--log "%s" (cons types node-score))
                (push (list node depth node-score) data)
                ;; The first value is plus, second is times.
                (cl-incf score node-score)))))
        (treesit-buffer-root-node)))
     (cons score (reverse data)))))

;;;###autoload
(defun cognitive-complexity-region (&optional beg end)
  "Analyze the region, from BEG to END."
  (let ((beg (or beg (if (use-region-p) (region-beginning) (point-min))))
        (end (or end (if (use-region-p) (region-end) (point-max)))))
    (cc-analyze (buffer-substring beg end))))

;;;###autoload
(defun cognitive-complexity-buffer ()
  "Analyze current buffer."
  (cc-analyze (buffer-string)))

;;
;; (@* "Languages" )
;;

(defun cc-rules--class-declaration (_node depth _nested)
  "Define rule for `class' declaration.
For argument DEPTH, see function `cc-analyze'
for more information."
  (cc-with-metrics
   (if (< 1 depth)  ; if class inside class,
       '(1 nil)     ; we score 1, but don't increase nested level
     '(0 nil))
   '(1 nil)))

(defun cc-rules--method-declaration-using-node-name (node depth nested node-name)
  "Define rule for function/method declaration using NODE-NAME.
For arguments NODE, DEPTH, and NESTED, see function
`cc-analyze' for more information."
  ;; XXX: Record the recursion method name (identifier) identifier by node-name
  (when-let ((node (car (cc--find-children-by-type node node-name))))
    (setq cc--recursion-identifier (treesit-node-text node)
          cc--recursion-identifier-depth depth))
  (cc-with-metrics
   ;; These magic numbers are observed by TreeSitter AST.
   (if (or (<= 5 depth) (<= 3 nested))
       '(1 nil)
     '(0 nil))
   '(1 nil)))

(defun cc-rules--method-declaration (node depth nested)
  "Define general rule for `method' declaration for most languages.
For arguments NODE, DEPTH, and NESTED, see function
`cc-analyze' for more information."
  (cc-rules--method-declaration-using-node-name node depth nested "identifier"))

(defun cc-rules--operators (node operators)
  "Define rule for operators from OPERATORS argument.
For argument NODE, see function `cc-analyze'
for more information."
  (cc-with-metrics
   (let* ((parent (treesit-node-parent node))
          (parent-text (treesit-node-text parent))
          (sequence)
          (count (cc--count-matches operators parent-text)))
     (when (<= 2 count)
       (setq sequence t))
     (list (if sequence 1 0) nil))
   '(1 nil)))

(defun cc-rules--logical-operators (node &rest _)
  "Define rule for logical operators.
For argument NODE, see function `cc-analyze'
for more information."
  (cc-rules--operators node '("&&" "||")))

(defun cc-rules--outer-loop (node _depth _nested &optional children)
  "Define rule for outer loop (jump), `break' and `continue' statements.
Optional argument CHILDREN is the children count.
For argument NODE, see function `cc-analyze'
for more information."
  (cc-with-metrics
   (list (if (<= (treesit-node-child-count node) children) 0 1) nil)
   '(0 nil)))

(defun cc-rules--recursion-using-node-name (node node-name)
  "General recursion rule using NODE's NODE-NAME as the function name."
  (cc-with-metrics
   (if-let* ((identifier (car (cc--find-children-by-type node node-name)))
             (text (treesit-node-text identifier))
             ((equal text cc--recursion-identifier)))
       '(1 nil)
     '(0 nil))
   ;; do nothing
   '(0 nil)))

(defun cc-rules--recursion (node &rest _)
  "Handle recursion for most languages use `identifier' NODE as the keyword."
  (cc-rules--recursion-using-node-name node "identifier"))

(defun cc-rules--elixir-call (node depth nested)
  "Define rule for Elixir `call' declaration.
For argument NODE, DEPTH, and NESTED, see function
`cc-analyze' for more information."
  (cc-with-metrics
   (let* ((text (treesit-node-text node))
          (def (string-prefix-p "def " text))
          (defmodule (string-prefix-p "defmodule " text)))
     (cond (def
            (cc-rules--method-declaration node depth nested))
           (defmodule
            (cc-rules--class-declaration node depth nested))
           (t
            (cc-rules--recursion node depth nested))))
   '(1 nil)))

(defun cc--elisp-function-name (node)
  "Return elisp function/macro name by NODE."
  (when-let* ((func-node (cc--find-parent-by-type node "function_definition"))
              (first-node (treesit-node-child func-node 2)))
    (treesit-node-text first-node)))

(defun cc--elisp-statement-p (text)
  "Return non-nil if TEXT is elisp statement."
  (member text '("if" "when" "unless"
                 "if-let" "if-let*" "when-let" "when-let*"
                 "cond" "pcase" "case" "cl-case"
                 "dolist" "while" "loop" "cl-loop")))

(defun cc-rules--elisp-list (node &rest _)
  "Define rule for Emacs Lisp `list' node.
For argument NODE, see function `cc-analyze'
for more information."
  (let* ((symbol (car (cc--find-children-by-type node "symbol")))
         (text (ignore-errors (treesit-node-text symbol)))
         (func-name (cc--elisp-function-name node)))
    (cond ((cc--elisp-statement-p text)
           '(1 t))
          ((member text `(,func-name))  ; recursion
           '(1 nil))
          (t
           '(0 nil)))))

(defun cc-rules--elisp-special-form (node &rest _)
  "Define rule for Emacs Lisp `special_form' node.

For argument NODE, see function `cc-analyze'
for more information."
  (let* ((symbol (treesit-node-child node 1))
         (text (treesit-node-text symbol))
         (parent (treesit-node-parent node))
         (parent-text (treesit-node-text parent)))
    (cond ((cc--elisp-statement-p text)
           '(1 t))
          ((member text '("lambda"))
           '(0 t))
          ((member text '("and" "or"))
           (if-let* ((opts (cc--count-matches '("([ ]*and " "([ ]*or ") parent-text))
                     ((<= 2 opts)))
               '(1 nil)
             '(0 nil)))
          (t
           '(0 nil)))))

(defun cc-rules--java-outer-loop (node &rest _)
  "Java outer loop (jump), `break' and `continue' statements.
For argument NODE, see function `cc-analyze'
for more information."
  (cc-rules--outer-loop node nil nil 2))

(defun cc-rules--kotlin-outer-loop (node &rest _)
  "Java outer loop (jump), `break' and `continue' statements.
For argument NODE, see function `cc-analyze'
for more information."
  (cc-rules--outer-loop node nil nil 1))

(defun cc-rules--julia-macro-expression (node &rest _)
  "Define rule for Julia `macro' expression.
For argument NODE, see function `cc-analyze'
for more information."
  (cc-with-metrics
   (if-let* ((identifier (car (cc--find-children-by-type node "identifier")))
             (text (treesit-node-text identifier))
             ((string= text "goto")))
       '(1 nil)
     '(0 nil))
   '(0 nil)))

(defun cc-rules--lua-binary-expressions (node &rest _)
  "Lua binary expressions which include logical operators.
For argument NODE, see function `cc-analyze'
for more information."
  (cc-with-metrics
   (let* ((node-is-logical-operator
           (lambda (node)
             ;; binary_expressions contain 3 elements; two expressions and one middle string
             (member (treesit-node-text (treesit-node-child node 1)) '("and" "or"))))
          (matches (cc--find-children-by-type node "binary_expression"))
          (has-child-logical-operator (cl-some (lambda (x) (funcall node-is-logical-operator x)) matches))
          (self-is-logical-operator (funcall node-is-logical-operator node)))
     (list (if (and self-is-logical-operator has-child-logical-operator)
               1
             0)
           nil))
   '(1 nil)))

(defun cc-rules--ruby-binary (node &rest _)
  "Ruby binary expression.
For argument NODE, see function `cc-analyze'
for more information."
  (cc-with-metrics
   (let ((text (treesit-node-text node))
         (sequence nil))
     (when (<= 2 (cc--count-matches '("||" "&&") text))
       (setq sequence t))
     (list (if sequence 1 0) nil))
   '(1 nil)))

(defun cc-rules--rust-outer-loop (node &rest _)
  "Rust outer loop (jump), `break' and `continue' statements.
For argument NODE, see function `cc-analyze'
for more information."
  (cc-rules--outer-loop node nil nil 1))

(defun cc-rules--scala-call-expression (node &rest _)
  "Define rule for Scala `while', `for', `do', and function call.
For argument NODE, see function `cc-analyze'
for more information."
  (let ((text (treesit-node-text node)))
    (cond ((string-match-p "^while[ (]" text) ;; TODO: multiple spaces (!)
           '(1 t))
          ((string-match-p "^for[ (]" text)
           '(1 t))
          ((string-match-p "^do[ {]" text)
           '(1 t))
          (t (cc-rules--recursion node)))))

(defun cc-rules--scala-infix-expression (node &rest _)
  "Define rule for Scala `infix' expression.
For argument NODE, see function `cc-analyze'
for more information."
  (let ((text (treesit-node-text node)))
    (cond ((string-match-p "=>" text)
           '(0 t))  ; don't score, but increase nested level
          (t
           '(0 nil)))))

;;
;; (@* "Debug Mode" )
;;

;;;###autoload
(define-minor-mode cognitive-complexity-debug-mode
  "Turn on/off debug mode for `cognitive-complexity'."
  :group 'cognitive-complexity
  :init-value nil
  :lighter "Cognitive-Complexity Debug"
  (cc--with-treesit
   (cc--after-change)))

;;
;; (@* "Minor Mode" )
;;

(defun cc--enable ()
  "Start `cc-mode'."
  (add-hook 'after-change-functions #'cc--after-change nil t)
  (cc--after-change))

(defun cc--disable ()
  "End `cc-mode'."
  (remove-hook 'after-change-functions #'cc--after-change t)
  (cc--delete-ovs))

;;;###autoload
(define-minor-mode cognitive-complexity-mode
  "Display cognitive-complexity result in current buffer."
  :group 'cognitive-complexity
  :init-value nil
  :lighter "Cognitive-Complexity"
  (cc--with-treesit
   (if cc-mode (cc--enable) (cc--disable))))

;;
;; (@* "Display" )
;;

(defcustom cc-display 'method
  "Choose the scope you want it to display."
  :type '(choice (const :tag "method" method)
          (const :tag "class" class))
  :group 'cognitive-complexity)

(defcustom cc-delay 0.8
  "Delay time to display results in seconds."
  :type 'float
  :group 'cognitive-complexity)

(defvar-local cc--display-timer nil
  "Timer to render the result.")

(defvar-local cc--ovs nil
  "List of overlays.")

(defcustom cc-priority 100
  "Overlays' priority."
  :type 'integer
  :group 'cognitive-complexity)

(defface cc-default
  '((t :height 0.7 :foreground "#999999"))
  "Face added to cognitive-complexity display."
  :group 'cognitive-complexity)

(defface cc-average
  '((t :height 0.7 :foreground "#62b543"))
  "Face to apply when compelxity is average."
  :group 'cognitive-complexity)

(defface cc-high
  '((t :height 0.7 :foreground "#F4AF3D"))
  "Face to apply when compelxity is high."
  :group 'cognitive-complexity)

(defface cc-extreme
  '((t :height 0.7 :foreground "#E05555"))
  "Face to apply when compelxity is extreme."
  :group 'cognitive-complexity)

(defcustom cc-symbols
  `((0   . ,(concat (propertize "❖ " 'face 'cc-average)
             (propertize "very simple (%s%%)" 'face 'cc-default)))
    (25   . ,(concat (propertize "❖ " 'face 'cc-average)
              (propertize "simple enough (%s%%)" 'face 'cc-default)))
    (75  . ,(concat (propertize "❖ " 'face 'cc-high)
             (propertize "mildly complex (%s%%)" 'face 'cc-default)))
    (100 . ,(concat (propertize "❖ " 'face 'cc-extreme)
             (propertize "very complex (%s%%)" 'face 'cc-default))))
  "Alist of symbol messages, consist of (score . message)."
  :type '(cons integer string)
  :group 'cognitive-complexity)

(defun cc--complexity-symbol (percent)
  "Return format message by PERCENT."
  (if cc-debug-mode ""
    (let ((str))
      (cl-some (lambda (pair)
                 (let ((percentage (car pair))
                       (msg        (cdr pair)))
                   (when (<= percentage percent)
                     (setq str msg))))
               (reverse cc-symbols))
      str)))

(defun cc--display-nodes (&optional scope)
  "Return a list of node types for display SCOPE.
SCOPE defaults to `cc-display' when not specified."
  (setq scope (or scope cc-display))
  (cl-case scope
    (`method (cl-case major-mode
               (`elixir-mode '("call"))
               (t '("function_declaration" "function_definition" "function_item"
                    "method_declaration" "method_definition" "method"))))
    (`class (cl-case major-mode
              (`elixir-mode '("call"))
              (t '("class_declaration" "class"))))
    (t
     (user-error "Unknown display scope: %s" scope))))

(defun cc--display-this-node-p (scope node)
  "Return non-nil when the NODE is inside the display SCOPE."
  (or cc-debug-mode                ; scope is `all'
      (member (treesit-node-type node) scope)))

(defun cc--make-ov (pos)
  "Create an overlay at POS."
  (save-excursion
    (goto-char pos)
    (let* ((ov (make-overlay (line-beginning-position)
                             (line-beginning-position))))
      (overlay-put ov 'invisible t)
      (overlay-put ov 'priority cc-priority)
      (overlay-put ov 'cognitive-complexity t)
      (push ov cc--ovs)
      ov)))

(defun cc--delete-ovs ()
  "Clean up all overlays."
  (mapc #'delete-overlay cc--ovs))

(defun cc--display-start (buffer)
  "Display result in BUFFER."
  (cc--with-current-visible-buffer
   buffer  ; make sure buffer still exists
   (when cc-mode
     (cc--delete-ovs)               ; clean up before re-rendering
     (let* ((report (cc-buffer))
            (report (if cc-debug-mode
                        report
                      (cc--accumulate report)))
            (data (cdr report))              ; list of `node' and `score'
            (scope (cc--display-nodes)))
       (dolist (it data)
         (let ((node             (nth 0 it))
               (depth            (nth 1 it))
               (node-score       (nth 2 it))
               (accumulate-score (nth 3 it)))
           (when (cc--display-this-node-p scope node)
             (let* ((pos (treesit-node-start node))
                    (column (save-excursion (goto-char pos) (current-column)))
                    (ov (cc--make-ov pos))
                    (score-or-percent (if cc-debug-mode
                                          node-score
                                        (cc-percentage accumulate-score)))
                    (str (if cc-debug-mode
                             (format "%s, +%s" depth score-or-percent)
                           (format (cc--complexity-symbol score-or-percent)
                                   score-or-percent))))
               (when cc-debug-mode
                 (add-face-text-property 0 (length str) 'cc-default nil str))
               (setq str (concat (spaces-string column) str "\n"))
               (overlay-put ov 'after-string str)))))))))

(defun cc--after-change (&rest _)
  "Register to `after-change-functions' variable."
  (when (timerp cc--display-timer)
    (cancel-timer cc--display-timer))
  (setq cc--display-timer
        (run-with-idle-timer cc-delay nil
                             #'cc--display-start (current-buffer))))


(provide 'cognitive-complexity)
;;; cognitive-complexity.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("cc-" . "cognitive-complexity-"))
;; End:
