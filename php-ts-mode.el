;;; php-ts-mode.el --- tree-sitter support for PHP  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Vincenzo Pupillo

;; Author     : Vincenzo Pupillo <v.pupillo@gmail.com>
;; Maintainer : Vincenzo Pupillo <v.pupillo@gmail.com>
;; Created    : September 2023
;; Keywords   : php language tree-sitter

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'treesit)
(require 'c-ts-common) ;; For comment indent and filling.
(require 'html-ts-mode) ;; For embed html
(require 'css-mode) ;; for embed css into html
(require 'js) ;; for embed javascript html

(eval-when-compile
  (require 'cl-lib)
  (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-parser-language "treesit.c")
(declare-function treesit-parser-included-ranges "treesit.c")
(declare-function treesit-parser-list "treesit.c")

;;; Custom variables

(defcustom php-ts-mode-indent-offset 4
  "Number of spaces for each indentation step (default) in `php-ts-mode'."
  :version "30.1"
  :type 'integer
  :safe 'integerp
  :group 'php)

(defun php-ts-mode--indent-style-setter (sym val)
  "Custom setter for `php-ts-mode-set-style'.

Apart from setting the default value of SYM to VAL, also change
the value of SYM in `php-ts-mode' buffers to VAL.

SYM should be `php-ts-mode-indent-style', and VAL should be a style
symbol."
  (set-default sym val)
  (named-let my-loop ((res nil)
                      (buffers (buffer-list)))
    (if (null buffers)
        (mapc (lambda (b)
                (with-current-buffer b
                  (php-ts-mode-set-style val)))
              res)
      (let ((buffer (car buffers)))
        (with-current-buffer buffer
          (if (derived-mode-p 'php-ts-mode)
              (my-loop (append res (list buffer)) (cdr buffers))
            (my-loop res (cdr buffers))))))))

(defcustom php-ts-mode-indent-style 'default
  "Style used for indentation.

The selected style could be one of:
`Default' - use a reasonable default style for PHP.
`PSR-2/PSR-12' - use PSR standards (PSR-2, PSR-12).
`PEAR' - use coding styles preferred for PEAR code and modules.
`Drupal' - use coding styles preferred for working with Drupal projects.
`WordPress' - use coding styles preferred for working with WordPress projects.
`Symfony2' - use coding styles preferred for working with Symfony2 projects.

If one of the supplied styles doesn't suffice a function could be
set instead.  This function is expected return a list that
follows the form of `treesit-simple-indent-rules'."
  :version "30.1"
  :type '(choice (const :tag "Default" default)
                 (const :tag "PSR-2/PSR-12" psr2)
                 (const :tag "PEAR" pear)
                 (const :tag "Drupal" drupal)
                 (const :tag "WordPress" wordpress)
                 (const :tag "Symfony2" symfony2)
                 (function :tag "A function for user customized style" ignore))
  :set #'php-ts-mode--indent-style-setter
  :group 'php)

(defcustom php-ts-mode-disable-inject nil
  "If true disable html/css/javascript injection."
  :version "30.1"
  :type '(boolean)
  :group 'php)

;;; Utils

(defun php-ts-mode--get-indent-style ()
  "Helper function to set indentation style.
MODE can be `pear', `default', `psr2', `drupal', `wordpress', `symfony2'."
  (let ((style
         (if (functionp php-ts-mode-indent-style)
             (funcall php-ts-mode-indent-style)
           (pcase php-ts-mode-indent-style
             ('default (alist-get 'default (php-ts-mode--indent-styles)))
             ('pear (alist-get 'pear (php-ts-mode--indent-styles)))
             ('drupal (alist-get 'drupal (php-ts-mode--indent-styles)))
             ('wordpress (alist-get 'wordpress (php-ts-mode--indent-styles)))
	     ('symfony2 (alist-get 'symfony2 (php-ts-mode--indent-styles)))
	     ('psr2 (alist-get 'psr2 (php-ts-mode--indent-styles)))))))
    `((php ,@style))))

(defun php-ts-mode--prompt-for-style ()
  "Prompt for an indent style and return the symbol for it."
  (intern
   (completing-read
    "Style: "
    (mapcar #'car (php-ts-mode--indent-styles))
    nil t nil nil "default")))

(defun php-ts-mode-set-global-style (style)
  "Set the indent style of PHP modes globally to STYLE.

This changes the current indent style of every PHP buffer and
the default PHP indent style for `php-ts-mode'
in this Emacs session."
  (interactive (list (php-ts-mode--prompt-for-style)))
  (php-ts-mode--indent-style-setter 'php-ts-mode-indent-style style))

(defun php-ts-mode-set-style (style)
  "Set the PHP indent style of the current buffer to STYLE.

To set the default indent style globally, use
`php-ts-mode-set-global-style'."
  (interactive (list (php-ts-mode--prompt-for-style)))
  (if (not (derived-mode-p 'php-ts-mode))
      (user-error "The current buffer is not in `php-ts-mode'")
    (setq-local php-ts-mode-indent-style style)
    (setq treesit-simple-indent-rules
          (treesit--indent-rules-optimize
           (php-ts-mode--get-indent-style)))))

(defun php-ts-mode--get-parser-ranges ()
  "Return the ranges covered by the parsers.

'php-ts-mode' use 4 parsers, this function returns, for the
current buffer, the ranges covered by each parser"
  (let ((ranges))
    (if (not (treesit-parser-list))
	(message "At least one parser must be initialized"))
    (cl-loop
     for parser in (treesit-parser-list)
     do (push (list parser (treesit-parser-included-ranges parser)) ranges)
     finally return ranges)))

;;; Syntax table

(defvar php-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; Taken from the cc-langs version
    (modify-syntax-entry ?_  "_"     table)
    (modify-syntax-entry ?\\ "\\"    table)
    (modify-syntax-entry ?+  "."     table)
    (modify-syntax-entry ?-  "."     table)
    (modify-syntax-entry ?=  "."     table)
    (modify-syntax-entry ?%  "."     table)
    (modify-syntax-entry ?<  "."     table)
    (modify-syntax-entry ?>  "."     table)
    (modify-syntax-entry ?&  "."     table)
    (modify-syntax-entry ?|  "."     table)
    (modify-syntax-entry ?\' "\""    table)
    (modify-syntax-entry ?\240 "."   table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"  table)
    (modify-syntax-entry ?\^m "> b" table)
    ;; php specific syntax
    (modify-syntax-entry ?`  "\""  table)
    (modify-syntax-entry ?\" "\""  table)
    (modify-syntax-entry ?#  "< b" table)
    (modify-syntax-entry ?$  "_"   table)
    table)
  "Syntax table for `php-ts-mode'.")

;;; Indent
;; TODO: only "default" works (sort of) for now
(defun php-ts-mode--indent-styles ()
  "Indent rules supported by `php-ts-mode'."
  (let ((common
         `(;; Slam all top level nodes to the left margin
	   ((parent-is "program") column-0 0)
	   ((parent-is "php_tag") column-0 0)
	   ((query "(ERROR (ERROR)) @indent") column-0 0) ;; FIXME: it's useful?
	   ((node-is ")") parent-bol 0)
           ((node-is "]") parent-bol 0)
           ((node-is "else") parent-bol 0)
           ((node-is "case") parent-bol php-ts-mode-indent-offset)
	   ((and (parent-is "comment") c-ts-common-looking-at-star)
            c-ts-common-comment-start-after-first-star -1)
           (c-ts-common-comment-2nd-line-matcher
            c-ts-common-comment-2nd-line-anchor 1)
           ((parent-is "comment") prev-adaptive-prefix 0)

           ((parent-is "function_definition") parent-bol 0)
           ((parent-is "conditional_expression") first-sibling 0)
           ((parent-is "assignment_expression") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "array_creation_expression") parent-bol php-ts-mode-indent-offset)
           ((parent-is "parenthesized_expression") first-sibling 1)
	   ((parent-is "arguments") parent-bol php-ts-mode-indent-offset)
           ((parent-is "formal_parameters") first-sibling 1)
           ((parent-is "binary_expression") parent 0)
           ((query "(for_statement (assignment_expression left: (_)) @indent)") parent-bol 5)
           ((query "(for_statement (binary_expression left: (_)) @indent)") parent-bol 5)
           ((query "(for_statement (update_expression (_)) @indent)") parent-bol 5)
           ((query "(function_call_expression arguments: (_) @indent)") parent php-ts-mode-indent-offset)
           ((parent-is "function_call_expression") parent 0)
	   ((node-is "}") standalone-parent 0)
           ((parent-is "declaration_list") parent-bol php-ts-mode-indent-offset)
           ((parent-is "initializer_list") parent-bol php-ts-mode-indent-offset)

	   ;; Statement in {} blocks.
	   ((or (match nil "compound_statement" nil 1 1)
                (match null "compound_statement"))
            standalone-parent php-ts-mode-indent-offset)
           ((parent-is "compound_statement") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "match_block") parent-bol php-ts-mode-indent-offset)

	   ;; These rules are for cases where the body is bracketless.
	   ((parent-is "case_statement") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "if_statement") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "else") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "for_statement")  parent-bol php-ts-mode-indent-offset)
	   ((parent-is "while_statement") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "do_statement") parent-bol php-ts-mode-indent-offset)
	   )))
    `((default ,@common)
      (drupal ,@common)
      (psr2 ,@common)
      (pear ,@common)
      (wordpress ,@common))))

;; TODO: questo firse non serve
(defun php-ts-mode--top-level-label-matcher (node &rest _)
  "A matcher that matches a top-level label.
NODE should be a labeled_statement."
  (let ((func (treesit-parent-until
               node (lambda (n)
                      (equal (treesit-node-type n)
                             "compound_statement")))))
    (and (equal (treesit-node-type node)
                "labeled_statement")
         (not (treesit-node-top-level func "compound_statement")))))

(defvar php-ts-mode-indent-block-type-regexp
  (rx (or "compound_statement"
	  "declaration_list"
          "enum_declaration_list"))
  "Regexp matching types of block nodes (i.e., {} blocks).")

;;; Font-lock
(defvar php-ts-mode--keywords
  '("abstract" "and" "array" "as" "break" "callable" "case" "catch"
    "class" "clone" "const" "continue" "declare" "default" "do" "echo"
    "else" "elseif" "enddeclare" "endfor" "endforeach" "endif"
    "endswitch" "endwhile" "enum" "extends" "final" "finally" "fn"
    "for" "foreach" "from" "function" "global" "goto" "if" "implements"
    "include" "include_once" "instanceof" "insteadof" "interface"
    "list" "match" "namespace" "new" "null" "or" "print" "private"
    "protected" "public" "readonly" "require" "require_once" "return"
    "static" "switch" "throw" "trait" "try" "unset" "use" "while" "xor"
    "yield")
  "PHP keywords for tree-sitter font-locking.")

(defvar php-ts-mode--operators
  '("**=" "*=" "/=" "%=" "+=" "-=" ".=" "<<=" ">>=" "&=" "^=" "|="
    "??=" "||" "&&" "|" "^" "&" "==" "!=" "<>" "===" "!==" "<" ">" "<="
    ">=" "<=>" "<<" ">>" "+" "-" "." "*" "**" "/" "%")
  "PHP operators for tree-sitter font-locking.")

(defvar php-ts-mode--magic-constants
  (rx string-start
      (or "__LINE__" "__FILE__" "__DIR__" "__FUNCTION__" "__CLASS__"
	  "__TRAIT__" "__METHOD__" "__NAMESPACE__" "PHP_EOL")
      string-end)
  "Magical predefined constants that is expanded at compile time.
See https://www.php.net/manual/en/language.constants.magic.php")

(defun php-ts-mode--font-lock-settings ()
  "Tree-sitter font-lock settings."
  (treesit-font-lock-rules

   :language 'php
   :feature 'keyword
   :override t
   `([,@php-ts-mode--keywords] @font-lock-keyword-face)
   
   :language 'php
   :feature 'comment
   '((comment) @font-lock-comment-face
     (comment) @contextual)

   :language 'php
   :feature 'constant
   '((boolean) @font-lock-constant-face
     (float) @font-lock-constant-face
     (integer) @font-lock-constant-face
     (null) @font-lock-constant-face
     ((name) @font-lock-builtin-face
      (:match "__[A-Z]*__" @font-lock-builtin-face))
     (const_declaration
      (const_element (name) @font-lock-constant-face)))

   :language 'php
   :feature 'name
   `((expression_statement (name) @font-lock-keyword-face (:equal "exit" @font-lock-keyword-face)))

   :language 'php
   :feature 'delimiter
   `((["," ":" ";" "\\"]) @font-lock-delimiter-face)
   
   :language 'php
   :feature 'operator
   `([,@php-ts-mode--operators] @font-lock-operator-face)

   :language 'php
   :feature 'string
   '((encapsed_string) @font-lock-string-face
     (string_value) @font-lock-string-face
     (string) @font-lock-string-face)
   
   :language 'php
   :feature 'literal
   '((heredoc identifier: (heredoc_start) @font-lock-string-face)
     (heredoc end_tag: (heredoc_end) @font-lock-string-face)
     (heredoc (_) @font-lock-costant-face)
     (heredoc_body (string_value) @font-lock-string-face)
     (nowdoc) @font-lock-string-face
     (shell_command_expression) @font-lock-string-face)

   :language 'php
   :feature 'type
   :override t
   '((union_type) @font-lock-type-face
     (bottom_type) @font-lock-type-face
     (intersection_type) @font-lock-type-face
     (primitive_type) @font-lock-type-face
     (cast_type) @font-lock-type-face
     (named_type) @font-lock-type-face)

   :language 'php
   :feature 'definition
   '((php_tag) @font-lock-preprocessor-face
     ("?>") @font-lock-preprocessor-face
     ;; Highlights identifiers in declarations.
     (class_declaration
      name: (_) @font-lock-type-face)
     (interface_declaration
      name: (_) @font-lock-type-face)
     (trait_declaration
      name: (_) @font-lock-type-face)
     (property_declaration
      (visibility_modifier) @font-lock-keyword-face)
     (enum_declaration
      name: (_) @font-lock-type-face)
     (function_definition
      name: (_) @font-lock-function-name-face)
     (method_declaration
      name: (_) @font-lock-function-name-face)
     ;; (object_creation_expression
     ;;  [(name) (qualified_name)] @font-lock-type-face)
     (object_creation_expression
      (name) @font-lock-type-face)
     (namespace_name_as_prefix (namespace_name (name)) @font-lock-type-face)
     (namespace_use_clause (name) @font-lock-property-use-face)
     (namespace_aliasing_clause (name) @font-lock-type-face)
     (namespace_name (name) @font-lock-type-face)
     (use_declaration (name) @font-lock-property-use-face))
   
   :language 'php
   :feature  'function-name
   '(;;     (array_creation_expression "array") @font-lock-function-name-face
     (function_call_expression
      function: (_) @font-lock-function-call-face)
     ;; (function_call_expression
     ;;  function: (qualified_name (name)) @font-lock-function-name-face)
     (scoped_call_expression
      name: (_) @font-lock-function-name-face)
     (scoped_call_expression
      scope: (_) @font-lock-constant-face)
     (member_call_expression
      name: (_) @font-lock-function-name-face)
     (class_constant_access_expression) @font-lock-constant-face
     (nullsafe_member_call_expression
      name: (_) @font-lock-constant-face))

   ;; TODO: bisogna aggiungere una regola per matchare i filtri delle funzioni tipo filter_*
   :language 'php
   :feature 'argument
   '((argument
      name: (_) @font-lock-constant-face))
   
   :language 'php
   :feature 'variable-name
   :override t
   `((variable_name "$" (name) @font-lock-variable-name-face)
     ((variable_name "$" (name) @font-lock-constant-face)
      (:equal "this" @font-lock-constant-face))
     (dynamic_variable_name (variable_name (name)) @font-lock-variable-name-face)
     (member_access_expression
      name: (_) @font-lock-variable-name-face)
     (scoped_property_access_expression
      scope: (name) @font-lock-constant-face))
   
   :language 'php
   :feature 'escape-sequence
   :override t
   '((heredoc_body (escape_sequence) @font-lock-escape-face)
     (encapsed_string (escape_sequence) @font-lock-escape-face))

   :language 'php
   :feature 'base-clause
   :override t
   '((base_clause (name) @font-lock-type-face)
;;     (qualified_name (name) @font-lock-function-name-face)
     (qualified_name (name) @font-lock-constant-face))

   :language 'php
   :feature 'property
   '((enum_case
      name: (_) @font-lock-type-face))

   :language 'php
   :feature 'attribute
   '((((attribute (_) @attribute_name) @font-lock-preprocessor-face) (:match "Deprecated" @attribute_name))
     (attribute_group (attribute (name) @font-lock-constant-face)))

   ;;The parentheses must stand after the attribute, because the attribute list starts with '#['
   :language 'php
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'php
   :feature 'error
   :override t
   '((ERROR) @php-ts-mode--fontify-error)))

;;; Font-lock helpers

(defun php-ts-mode--declarator-identifier (node)
  "Return the identifier of the declarator node NODE."
  (pcase (treesit-node-type node)
    ;; Recurse.
    ((or "attributed_declarator" "parenthesized_declarator")
     (php-ts-mode--declarator-identifier (treesit-node-child node 0 t)))
    ((or "pointer_declarator" "reference_declarator")
     (php-ts-mode--declarator-identifier (treesit-node-child node -1)))
    ((or "function_declarator" "array_declarator" "init_declarator")
     (php-ts-mode--declarator-identifier
      (treesit-node-child-by-field-name node "declarator")))
    ("qualified_identifier"
     (php-ts-mode--declarator-identifier
      (treesit-node-child-by-field-name node "name")))
    ;; Terminal case.
    ((or "identifier" "field_identifier")
     node)))

(defun php-ts-mode--fontify-declarator (node override start end &rest _args)
  "Fontify a declarator (whatever under the \"declarator\" field).
For NODE, OVERRIDE, START, END, and ARGS, see
`treesit-font-lock-rules'."
  (let* ((identifier (php-ts-mode--declarator-identifier node))
         (qualified-root
          (treesit-parent-while (treesit-node-parent identifier)
                                (lambda (node)
                                  (equal (treesit-node-type node)
                                         "qualified_identifier"))))
         (face (pcase (treesit-node-type (treesit-node-parent
                                          (or qualified-root
                                              identifier)))
                 ;; ("function_declarator" 'font-lock-function-name-face)
		 ("function_definition" 'font-lock-function-name-face)
                 (_ 'font-lock-variable-name-face))))
    (treesit-fontify-with-override
     (treesit-node-start identifier) (treesit-node-end identifier)
     face override start end)))

(defun php-ts-mode--fontify-error (node override start end &rest _)
  "Fontify the error nodes.
For NODE, OVERRIDE, START, and END, see
`treesit-font-lock-rules'."
  (treesit-fontify-with-override
   (treesit-node-start node) (treesit-node-end node)
   'font-lock-warning-face
   override start end))

(defun php-ts-mode--html-language-at-point (point)
  "Return the language at POINT assuming the point is within a HTML region."
  (let* ((node-at-point (treesit-node-at point 'html))
         (parent-node (treesit-node-parent node-at-point))
	 (node-query (format "(%s (%s))" (treesit-node-type parent-node) (treesit-node-type node-at-point))))
    (cond
     ((string-equal "(style_element (raw_text))" node-query) 'css)
     ((string-equal "(script_element (raw_text))" node-query) 'javascript)
     (t 'html))))


;; FIXME: php-ts-mode--language-at-point or php-ts-mode--language-at-point-2 ? Which one is better ?
(defun php-ts-mode--language-at-point (point)
  "Return the language at POINT, used to determine which tree sitter parser to use."
  (let* ((node-at-point (treesit-node-at point 'php))
         (parent-node (treesit-node-parent node-at-point))
	 (node-query (format "(%s (%s))" (treesit-node-type parent-node) (treesit-node-type node-at-point))))
    (if (not (member node-query '("(program (text))"
				  "(text_interpolation (text))"
				  ;; "(program (text_interpolation \"?>\"))"
				  )))
	'php
      (php-ts-mode--html-language-at-point point))))

(defun php-ts-mode--language-at-point-2 (point)
  "Return the language at POINT."
  (let* ((range nil)
         (language-in-range
          (cl-loop
           for parser in (treesit-parser-list)
           do (setq range
                    (cl-loop
                     for range in (treesit-parser-included-ranges parser)
                     if (and (>= point (car range)) (<= point (cdr range)))
                     return parser))
           if range
           return (treesit-parser-language parser))))
    (if (null language-in-range)
        (when-let ((parser (car (treesit-parser-list))))
          (treesit-parser-language parser))
      language-in-range)))


;;; Imenu

(defun php-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (when (member (treesit-node-type node) '(
					   "class_declaration"
					   "function_definition"
					   "method_declaration"
					   "trait_declaration"
					   "interface_declaration"
					   "enum_declaration"))
    (treesit-node-text
     (treesit-node-child-by-field-name node "name") t)))


(defun php-ts-mode--variable-name (node)
  "Return the variable name of NODE.
Return nil if there is no name or if NODE is not a variable_name node."
  (if (string-equal "variable_name" (treesit-node-type (treesit-node-parent node)))
      (treesit-node-text node t)))

;;; Defun navigation

(defun php-ts-mode--indent-defun ()
  "Indent the current top-level declaration syntactically.

`treesit-defun-type-regexp' defines what constructs to indent."
  (interactive "*")
  (when-let ((orig-point (point-marker))
             (node (treesit-defun-at-point)))
    (indent-region (treesit-node-start node)
                   (treesit-node-end node))
    (goto-char orig-point)))

;;; Filling
(defun php-ts-mode--fill-paragraph (&optional arg)
  "Fillling function for `php-ts-mode'.
ARG is passed to `fill-paragraph'.
Derived from `c-ts-common--fill-paragraph', support '#' comment."
  (interactive "*P")
  (save-restriction
    (widen)
    (let ((node (treesit-node-at (point))))
      (when (string-match-p "comment"
                            (treesit-node-type node))
        (if (save-excursion
              (goto-char (treesit-node-start node))
              (looking-at (rx (or "//" "#"))))
            (fill-comment-paragraph arg)
          (c-ts-common--fill-block-comment arg)))
      ;; Return t so `fill-paragraph' doesn't attempt to fill by
      ;; itself.
      t)))

(defun php-ts-mode-comment-setup ()
  "Set up local variables for PHP comment.
Derived from `c-ts-common-comment-setup'."
  (c-ts-common-comment-setup)
  (setq-local comment-multi-line t)
  (setq-local comment-style 'extra-line)
  (setq-local comment-line-break-function #'c-indent-new-comment-line)
  (setq-local fill-paragraph-function #'php-ts-mode--fill-paragraph))

;;; Defun navigation

(defun php-ts-mode--defun-valid-p (node)
  "Return non-nil if NODE is a valid defun node.
Ie, NODE is not nested."
  (not (and (member (treesit-node-type node)
                    '("variable_name"
		      "enum_declaration"
                      "union_declaration"
                      "declaration"))
            ;; If NODE's type is one of the above, make sure it is
            ;; top-level.
            (treesit-node-top-level
             node (rx (or "variable_name"
			  "function_definition"
                          "enum_declaration"
                          "union_declaration"
                          "declaration"))))))

(defvar php-ts-mode--custom-html-font-lock-settings
  (treesit-font-lock-rules
   :language 'html
   :override t
   :feature 'comment
   `((comment) @font-lock-comment-face
     (fragment (text) @font-lock-comment-face))
     ;; (text) @font-lock-comment-face)
   :language 'html
   :override t
   :feature 'keyword
   `("doctype" @font-lock-keyword-face)
   :language 'html
   :override t
   :feature 'definition
   `((tag_name) @font-lock-function-name-face)
   :language 'html
   :override t
   :feature 'string
   `((quoted_attribute_value) @font-lock-string-face)
   :language 'html
   :override t
   :feature 'property
   `((attribute_name) @font-lock-variable-name-face))
  "Tree-sitter font-lock settings for `php-html-ts-mode'.")
;;; Modes

(defvar-keymap php-ts-mode-map
  :doc "Keymap for the PHP language with tree-sitter"
  :parent prog-mode-map
  "C-c C-q" #'php-ts-mode--indent-defun)

;;;###autoload
;; TODO: ste due funzioni vanno fuse
(define-derived-mode php-ts-mode prog-mode "PHP"
  "Major mode for editing PHP, powered by tree-sitter.

\\{php-ts-mode-map}"
  :group 'php
  :syntax-table php-ts-mode--syntax-table

  ;; (unless (treesit-ready-p 'html)
  ;;   (warn "Tree-sitter for Html isn't available, it's required by php-ts-mode"))

  ;; (unless (treesit-ready-p 'css)
  ;;   (warn "Tree-sitter for CSS isn't available, it's required by php-ts-mode"))

  ;; (unless (treesit-ready-p 'javascript)
  ;;   (warn "Tree-sitter for Javascript isn't available, it's required by php-ts-mode"))
  
  (unless (treesit-ready-p 'php)
    (error "Tree-sitter for PHP isn't available"))

  
  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (regexp-opt '("comment"
			    "variable_name"
			    "function_definition"
                            "enum_declaration"
                            "class_declaration"
			    "interface_declaration"
			    "trait_declaration"
			    "method_declaration"
			    )))
  
  (setq-local treesit-defun-name-function #'php-ts-mode--defun-name)

  (setq-local treesit-sentence-type-regexp
              ;; compound_statement makes us jump over too big units
              ;; of code, so skip that one, and include the other
              ;; statements.
              (regexp-opt '("statement"
			    "declaration"
                            "expression_statement"
                            "if_statement"
                            "switch_statement"
                            "do_statement"
                            "while_statement"
                            "for_statement"
                            "return_statement"
                            "break_statement"
                            "continue_statement"
                            "case_statement")))

  (setq-local treesit-sexp-type-regexp
              (regexp-opt '("definition"
                            "qualifier"
                            "type"
                            "assignment"
			    "expression"
                            "literal"
                            "string")))
  
  ;; Nodes like struct/enum/union_specifier can appear in
  ;; function_definitions, so we need to find the top-level node.
  (setq-local treesit-defun-prefer-top-level t)

  ;; Indent.
  (when (eq php-ts-mode-indent-style 'default)
    (setq-local indent-tabs-mode t)) ;; TODO: check if needed for PHP

  ;; Comment
  (php-ts-mode-comment-setup)

  (setq-local c-ts-common-indent-offset 'php-ts-mode-indent-offset)
  (setq-local treesit-simple-indent-rules (php-ts-mode--get-indent-style))
  
  ;; Electric
  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings
              '(("Enum" "enum_declaration" nil nil)
		("Variable" "variable_name" nil php-ts-mode--variable-name)
		("Class" "class_declaration" nil nil)
                ("Function" "function_definition" nil nil)
                ("Method" "method_declaration" nil nil)))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings (php-ts-mode--font-lock-settings))
  ;; (setq-local font-lock-defaults nil) ;; only for derived mode
  (setq-local treesit-font-lock-feature-list
              '(( comment definition)
                ( keyword string type name)
                ( assignment constant escape-sequence base-clause literal function-name variable-name)
                ( argument bracket delimiter error operator property attribute)))

  ;; FIXME: php is pretty ok net of treesit-php-mode problems, however there
  ;; are rules from the embedded languages that alter its behavior, dammit!
  
  ;; Embed html, if possible
  (when (not php-ts-mode-disable-inject)
    (if (and (treesit-ready-p 'html) (treesit-ready-p 'javascript) (treesit-ready-p 'css))
	(progn
	  (setq-local html-ts-parser (treesit-parser-create 'html)
		      css-ts-parser (treesit-parser-create 'css)
		      javascript-ts-parser (treesit-parser-create 'javascript))
	  
	  ;; workaround for treesitter bug, see
	  ;; https://lists.gnu.org/archive/html/emacs-devel/2023-09/msg01020.html
	  (defun test-php--clean-up-parser-range (&rest _)
	    (dolist (parser (mapcan (lambda (lang)
				      (treesit-parser-list nil lang))
				    '(html css javascript)))
	      (when (null (treesit-parser-included-ranges parser))
		(treesit-parser-set-included-ranges
		 parser `((,(point-min) . ,(point-min)))))))
	  
	  (setq-local treesit-range-settings
		      (treesit-range-rules
		       :embed 'html
		       :host 'php
		       '((program (text) @cap)
			 (text_interpolation (text) @cap))

		       :embed 'javascript
		       :host 'html
		       '((script_element
			  (start_tag (tag_name))
			  (raw_text) @cap))

		       :embed 'css
		       :host 'html
		       '((style_element
			  (start_tag (tag_name))
			  (raw_text) @cap))

		       ;; workaround for a treesitter bug
		      #'test-php--clean-up-parser-range))
	  
	  (setq-local treesit-font-lock-settings
		      (append treesit-font-lock-settings
			      php-ts-mode--custom-html-font-lock-settings
			      js--treesit-font-lock-settings
			      css--treesit-settings))
	  
	  ;; come sopra per l'indentazione
	  (setq-local treesit-simple-indent-rules
		      (append treesit-simple-indent-rules
			      html-ts-mode--indent-rules
			      js--treesit-indent-rules
			      css--treesit-indent-rules))
	  
	  (setq-local treesit-language-at-point-function #'php-ts-mode--language-at-point)

	  (setq-local treesit-font-lock-feature-list
		      '(( comment definition
			  ;; CSS
			  query selector
			  ;; HTML
			  text)
			( keyword string type name)
			( assignment constant escape-sequence base-clause literal function-name variable-name
			  ;; Javascript
			  jsx number pattern string-interpolation
			  )
			( argument bracket delimiter error function operator property variable attribute)))
	  )
      (warn "Tree-sitter for Html (with javascript and css) isn't available. Html and/or Javascript and/or CSS syntax support isn't available to php-ts-mode")))
  ;; should be the last one
  (setq-local php-ts-parser (treesit-parser-create 'php))
  (treesit-font-lock-recompute-features)
  (treesit-major-mode-setup))

(if (treesit-ready-p 'php)
    (progn
      (add-to-list 'auto-mode-alist '("\\.\\(?:php[s345]?\\|phtml\\)\\'" . php-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.\\(?:php\\.inc\\|stub\\)\\'" . php-ts-mode))
      (add-to-list 'auto-mode-alist '("/\\.php_cs\\(?:\\.dist\\)?\\'" . php-ts-mode))))

(provide 'php-ts-mode)
;;; php-ts-mode.el ends here
