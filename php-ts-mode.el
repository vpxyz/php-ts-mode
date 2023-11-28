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
;; This package provides `php-ts-mode' which is a major mode
;; for editing PHP files with embedded html, javascript, css and phpdoc.
;; Tree Sitter is used to parse each of these languages, if parsers are available.
;; phpdoc or html/javascript/css can be disabled if you don't need them.
;;
;; This package is compatible with an was tested against those tree-sitter grammars:
;; * https://github.com/tree-sitter/tree-sitter-php
;; * https://github.com/tree-sitter/tree-sitter-html
;; * https://github.com/tree-sitter/tree-sitter-javascript
;; * https://github.com/tree-sitter/tree-sitter-css
;; * https://github.com/claytonrcarter/tree-sitter-phpdoc
;;
;; Features
;;
;; * Indent
;; * IMenu
;; * Navigation
;; * Which-function
;; * Tree-sitter parser installation helper

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


;;; Install treesitter language parsers
(defvar php-ts-mode--language-source-alist
  '((php . ("https://github.com/tree-sitter/tree-sitter-php"))
    (html . ("https://github.com/tree-sitter/tree-sitter-html"))
    (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
    (css . ("https://github.com/tree-sitter/tree-sitter-css"))
    (phpdoc . ("https://github.com/claytonrcarter/tree-sitter-phpdoc")))
  "Treesitter language parser alist required by php-ts-mode.

You can customize this variable if you want to stick to a specific
commit and/or use different parsers.")

(defun php-ts-mode-install-parser ()
  "Install all the required treesitter parser."
  (interactive)
  (let ((treesit-language-source-alist php-ts-mode--language-source-alist))
    (dolist (item php-ts-mode--language-source-alist)
      (treesit-install-language-grammar (car item)))))

;;; Custom variables

(defcustom php-ts-mode-indent-offset 4
  "Number of spaces for each indentation step (default) in `php-ts-mode'."
  :version "30.1"
  :type 'integer
  :safe 'integerp
  :group 'php-ts)

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

(defcustom php-ts-mode-indent-style 'psr2
  "Style used for indentation.

The selected style could be one of:
`PSR-2/PSR-12' - use PSR standards (PSR-2, PSR-12), thi is the default.
`PEAR' - use coding styles preferred for PEAR code and modules.
`Drupal' - use coding styles preferred for working with Drupal projects.
`WordPress' - use coding styles preferred for working with WordPress projects.
`Symfony' - use coding styles preferred for working with Symfony projects.
`Zend' - use coding styles preferred for working with Zend projects.

If one of the supplied styles doesn't suffice a function could be
set instead.  This function is expected return a list that
follows the form of `treesit-simple-indent-rules'."
  :version "30.1"
  :type '(choice (const :tag "PSR-2/PSR-12" psr2)
		 (const :tag "PEAR" pear)
		 (const :tag "Drupal" drupal)
		 (const :tag "WordPress" wordpress)
		 (const :tag "Symfony" symfony)
		 (const :tag "Zend" zend)
		 (function :tag "A function for user customized style" ignore))
  :set #'php-ts-mode--indent-style-setter
  :safe 'c-ts-indent-style-safep
  :group 'php-ts)

(defcustom php-ts-mode-disable-inject nil
  "If true disable html/css/javascript injection."
  :version "30.1"
  :type '(boolean)
  :group 'php-ts)

;;; Utils

(defun php-ts-mode--get-indent-style ()
  "Helper function to set indentation style.
MODE can be `psr2', `pear', `drupal', `wordpress', `symfony', `zend'."
  (let ((style
	 (if (functionp php-ts-mode-indent-style)
	     (funcall php-ts-mode-indent-style)
	   (cl-case php-ts-mode-indent-style
	     (psr2 (alist-get 'psr2 (php-ts-mode--indent-styles)))
	     (pear (alist-get 'pear (php-ts-mode--indent-styles)))
	     (drupal (alist-get 'drupal (php-ts-mode--indent-styles)))
	     (wordpress (alist-get 'wordpress (php-ts-mode--indent-styles)))
	     (symfony (alist-get 'symfony (php-ts-mode--indent-styles)))
	     (zend (alist-get 'zend (php-ts-mode--indent-styles)))
	     (t (alist-get 'psr2 (php-ts-mode--indent-styles)))))))
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

(defun php-ts-mode--set-indent-offset (style)
  (if (eq style 'drupal)
      (setq php-ts-mode-indent-offset 2)
    (setq php-ts-mode-indent-offset 4))
  ;; (pcase style
  ;;   ('drupal (setq php-ts-mode-indent-offset 2))
  ;;   (_ (setq php-ts-mode-indent-offset 4)))
  ;;(setq-local c-ts-common-indent-offset 'php-ts-mode-indent-offset)
  )

(defun php-ts-mode-set-style (style)
  "Set the PHP indent style of the current buffer to STYLE.

To set the default indent style globally, use
`php-ts-mode-set-global-style'."
  (interactive (list (php-ts-mode--prompt-for-style)))
  (if (not (derived-mode-p 'php-ts-mode))
      (user-error "The current buffer is not in `php-ts-mode'")
    (php-ts-mode--set-indent-offset style)
    (setq-local php-ts-mode-indent-style style)
    (setq treesit-simple-indent-rules
	  (treesit--indent-rules-optimize
	   (php-ts-mode--get-indent-style)))))

(defun php-ts-mode--get-parser-ranges ()
  "Return the ranges covered by the parsers.

`php-ts-mode' use 4 parsers, this function returns, for the
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
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?$  "_"   table)
    table)
  "Syntax table for `php-ts-mode'.")

;;; Indent

(defun php-ts-mode--indent-styles ()
  "Indent rules supported by `php-ts-mode'."
  (let ((common
	 `(((or (node-is "program")
		(node-is "php_tag"))
	    ;;parent-bol 0)
	    parent 0)
	   ;; column-0 0)
	   ;; ((parent-is "program") column-0 0)
	   ;; ((parent-is "php_tag") column-0 0)
	   ((query "(ERROR (ERROR)) @indent") column-0 0)
	   ((node-is ")") parent-bol 0)
	   ((node-is "]") parent-bol 0)
	   ((node-is "else") parent-bol 0)
	   ((node-is "case") parent-bol php-ts-mode-indent-offset)
	   ((node-is "default") parent-bol php-ts-mode-indent-offset)
	   ;; `c-ts-common-looking-at-star' has to come before
	   ;; `c-ts-common-comment-2nd-line-matcher'.
	   ((and (parent-is "comment") c-ts-common-looking-at-star)
	    c-ts-common-comment-start-after-first-star -1)
	   (c-ts-common-comment-2nd-line-matcher
	    c-ts-common-comment-2nd-line-anchor
	    1)
	   ((parent-is "comment") prev-adaptive-prefix 0)

	   ((parent-is "method_declaration") parent-bol 0)
	   ((parent-is "function_definition") parent-bol 0)
	   ;;((parent-is "function_call_expression") first-sibling 0) ;; spostato nella parte specifiche per i linguaggi
	   ((parent-is "member_call_expression") first-sibling php-ts-mode-indent-offset)
	   ((parent-is "conditional_expression") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "assignment_expression") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "array_creation_expression") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "parenthesized_expression") first-sibling 1)
	   ((parent-is "binary_expression") parent 0)
	   ((or (parent-is "arguments")
		(parent-is "formal_parameters"))
	    parent-bol php-ts-mode-indent-offset)
	   ((query "(for_statement (assignment_expression left: (_)) @indent)") parent-bol php-ts-mode-indent-offset)
	   ((query "(for_statement (binary_expression left: (_)) @indent)") parent-bol php-ts-mode-indent-offset)
	   ((query "(for_statement (update_expression (_)) @indent)") parent-bol php-ts-mode-indent-offset)
	   ((query "(function_call_expression arguments: (_) @indent)") parent php-ts-mode-indent-offset)
	   ((query "(member_call_expression arguments: (_) @indent)") parent php-ts-mode-indent-offset)

	   ;; Closing bracket. Must stay here, the rule order matter.
	   ((node-is "}") standalone-parent 0)
	   ((parent-is "declaration_list") parent-bol php-ts-mode-indent-offset)
	   ;;((parent-is "attribute") parent-bol 0)
	   ((parent-is "initializer_list") parent-bol php-ts-mode-indent-offset)

	   ;; Statement in {} blocks.
	   ((or (match nil "compound_statement" nil 1 1)
		(match null "compound_statement"))
	    standalone-parent php-ts-mode-indent-offset)
	   ((parent-is "compound_statement") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "match_block") parent-bol php-ts-mode-indent-offset)

	   ;; These rules are for cases where the body is bracketless.
	   ((or (parent-is "switch_statement")
		(parent-is "case_statement")
		(parent-is "if_statement")
		(parent-is "else")
		(parent-is "for_statement")
		(parent-is "while_statement")
		(parent-is "do_statement"))
	    parent-bol php-ts-mode-indent-offset)
	   )))
    `((psr2
       ((parent-is "function_call_expression") parent-bol php-ts-mode-indent-offset)
       ((parent-is "array_creation_expression") parent 1)
       ,@common)
      (pear
       ((or (node-is "case_statement")
	    (node-is "default_statement"))
	parent-bol 0)
       ;;((parent-is "function_call_expression") parent-bol php-ts-mode-indent-offset)
       ((parent-is "array_creation_expression") parent-bol php-ts-mode-indent-offset)
       ,@common)
      (drupal
       ((parent-is "function_call_expression") parent-bol php-ts-mode-indent-offset)
       ;;((query "(member_call_expression arguments: (_) @indent)") first-sibling php-ts-mode-indent-offset)
       ;;((node-is "member_call_expression") parent-bol php-ts-mode-indent-offset)
       ((parent-is "default_statement") parent-bol php-ts-mode-indent-offset)
       ,@common)
      (symfony ,@common)
      (wordpress ,@common)
      (zend
       ((or (parent-is "arguments")
	    (parent-is "formal_parameters"))
	first-sibling 1)
       ((parent-is "function_call_expression") first-sibling 0)
       ,@common))))

(defvar php-ts-mode--phpdoc-indent-styles
  '((phpdoc
     ((and (parent-is "document") c-ts-common-looking-at-star)
      c-ts-common-comment-start-after-first-star -1)
     (c-ts-common-comment-2nd-line-matcher
      c-ts-common-comment-2nd-line-anchor
      1)
     ((parent-is "document") prev-adaptive-prefix 0)
;;     ((node-is "document") column-0 php-ts-mode-indent-offset)
     ))
  "Tree-sitter indentation rules for for `phpdoc'.")

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

;; TODO: anche questo non sembra essere utile
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
    "??"  "??=" "||" "&&" "|" "^" "&" "==" "!=" "<>" "===" "!==" "<"
    ">" "<=" ">=" "<=>" "<<" ">>" "+" "-" "." "*" "**" "/"
    "%" "->" "?->")
  "PHP operators for tree-sitter font-locking.")

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
     ((name) @font-lock-constant-face
      (:match "^_?[A-Z][A-Z%d_]*$" @font-lock-constant-face))
     ((name) @font-lock-builtin-face
      (:match "^__[A-Z]*__" @font-lock-builtin-face))
     (const_declaration
      (const_element (name) @font-lock-constant-face))
     (relative_scope "self") @font-lock-builtin-face)

   :language 'php
   :feature 'name
   `((goto_statement (name) @font-lock-constant-face)
     (named_label_statement (name) @font-lock-constant-face)
     (expression_statement (name) @font-lock-keyword-face
			   (:equal "exit" @font-lock-keyword-face)))

   :language 'php
   ;;:override t
   :feature 'delimiter
   `((["," ":" ";" "\\"]) @font-lock-delimiter-face)

   :language 'php
   :feature 'operator
   `([,@php-ts-mode--operators] @font-lock-operator-face)

   :language 'php
   :feature 'variable-name
   :override t
   `(((name) @font-lock-keyword-face (:equal "this" @font-lock-keyword-face))
     (variable_name (name) @font-lock-variable-name-face)
     (dynamic_variable_name (name) @font-lock-variable-name-face)
     (member_access_expression
      name: (_) @font-lock-variable-name-face)
     (scoped_property_access_expression
      scope: (name) @font-lock-constant-face))

   :language 'php
   :feature 'string
   ;;:override t
   `(("\"") @font-lock-string-face
     (encapsed_string) @font-lock-string-face
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
     (class_interface_clause (name) @font-lock-type-face)
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
     ("=>") @font-lock-keyword-face
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
   :override t
   '(;;     (array_creation_expression "array") @font-lock-function-name-face
     (function_call_expression
      function: (_) @font-lock-function-call-face)
     (scoped_call_expression
      name: (_) @font-lock-function-name-face)
     (scoped_call_expression
      scope: (_) @font-lock-constant-face)
     (member_call_expression
      name: (_) @font-lock-function-name-face)
     (class_constant_access_expression (name) @font-lock-constant-face)
     (nullsafe_member_call_expression
      name: (_) @font-lock-constant-face))

   :language 'php
   :feature 'argument
   '((argument
      name: (_) @font-lock-constant-face))

   :language 'php
   :feature 'escape-sequence
   :override t
   '((heredoc_body (escape_sequence) @font-lock-escape-face)
     (encapsed_string (escape_sequence) @font-lock-escape-face))

   :language 'php
   :feature 'base-clause
   :override t
   '((base_clause (name) @font-lock-type-face)
     (use_as_clause (name) @font-lock-property-use-face)
     (qualified_name (name) @font-lock-constant-face))

   :language 'php
   :feature 'property
   '((enum_case
      name: (_) @font-lock-type-face))

   :language 'php
   :feature 'attribute
   '((((attribute (_) @attribute_name) @font-lock-preprocessor-face)
      (:equal "Deprecated" @attribute_name))
     (attribute_group (attribute (name) @font-lock-constant-face)))

   :language 'php
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'php
   :feature 'error
   :override t
   '((ERROR) @php-ts-mode--fontify-error)))

(defun php-ts-mode--phpdoc-font-lock-settings ()
  "Tree-sitter font-lock settings for phpdoc."
  (treesit-font-lock-rules
   ;; :language 'phpdoc
   ;; :feature 'document
   ;; :override 'prepend
   ;; '((document (_) @font-lock-doc-face))
   ;; ;;'(((document) @font-lock-doc-face))

   :language 'phpdoc
   :feature 'type
   :override t
   '((type_list
      [(array_type) (primitive_type) (named_type) (optional_type)] @font-lock-type-face))

   :language 'phpdoc
   :feature 'attribute
   :override t
   '((tag_name) @font-lock-constant-face
     (tag
      [(version) (email_address)] @font-lock-doc-markup-face))

   :language 'phpdoc
   :feature 'variable
   :override t
   '((variable_name (name) @font-lock-variable-name-face))

   ;; TODO: disabled. The phpdoc parser does not gracefully handle unknown tags
   ;; :language 'phpdoc
   ;; :feature 'error
   ;; :override t
   ;; '((ERROR) @php-ts-mode--fontify-error)
   ))

;;; Font-lock helpers

(defun php-ts-mode--fontify-error (node override start end &rest _)
  "Fontify the error nodes.
For NODE, OVERRIDE, START, and END, see
`treesit-font-lock-rules'."
  (treesit-fontify-with-override
   (treesit-node-start node) (treesit-node-end node)
   'font-lock-warning-face
   override start end))

;; TODO: this function was taken from elixir-ts-mode, why not put it in treesit.el ?
(defun php-ts-mode--language-at-point (point)
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

(defun php-ts-mode--parent-object (node)
  "Return the name of the object that own NODE."
  (treesit-parent-until
   node
   (lambda (n)
     (member (treesit-node-type n)
	     '("class_declaration"
	       "trait_declaration"
	       "interface_declaration"
	       "enum_declaration"
	       "function_definition"
	       "method_declaration")))))

(defun php-ts-mode--defun-name-separator (node)
  (if (member (treesit-node-type node) '("function_definition" "method_declaration"))
      "()::"
    "\\")
  ;; (pcase (treesit-node-type node)
  ;;   ((or "function_definition" "method_declaration")
  ;;    "()::")
  ;;   (_ "\\"))
  )

(defun php-ts-mode--defun-object-name (node node-text)
  (let* ((parent-node (php-ts-mode--parent-object node))
	 (parent-node-text
	  (treesit-node-text
	   (treesit-node-child-by-field-name parent-node "name") t))
	 (parent-node-separator (php-ts-mode--defun-name-separator parent-node))
	 (node-separator (php-ts-mode--defun-name-separator node)))
    (if parent-node
	(progn
	  (setq parent-node-text
		(php-ts-mode--defun-object-name
		 parent-node
		 parent-node-text))
	  (concat parent-node-text parent-node-separator node-text))
      node-text)))

(defun php-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (let ((child (treesit-node-child-by-field-name node "name")))
    (cl-case (intern (treesit-node-type node))
      (class_declaration (treesit-node-text child t))
      (trait_declaration (treesit-node-text child t))
      (interface_declaration (treesit-node-text child t))
      (enum_declaration (treesit-node-text child t))
      (function_definition (treesit-node-text child t))

      (method_declaration
       (php-ts-mode--defun-object-name node (treesit-node-text child t)))

      (variable_name
       (php-ts-mode--defun-object-name node (treesit-node-text node t))))))


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
(defun php-ts-mode-comment-setup ()
  "Set up local variables for PHP comment.
Derived from `c-ts-common-comment-setup'."
  (c-ts-common-comment-setup)
  (setq-local comment-style 'extra-line)
  ;;(setq-local comment-start "// ")
  (setq-local comment-start-skip
              (eval-when-compile
                (rx (group (or (: "#" (not (any "[")))
                               (: "/" (+ "/"))
                               (: "/*")))
                    (* (syntax whitespace)))))
  (setq-local comment-continue "* ")
  ;;(setq-local comment-end "")
  (setq-local c-ts-common--comment-regexp (rx (or "comment" "document"))))

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

;;; Injected tree-sitter helper

(defvar php-ts-mode--custom-html-font-lock-settings
  (treesit-font-lock-rules
   :language 'html
   :override t
   :feature 'comment
   `((comment) @font-lock-comment-face
     (fragment (text) @font-lock-comment-face))

   :language 'html
   :override t
   :feature 'keyword
   `("doctype" @font-lock-keyword-face)

   :language 'html
   :override t
   :feature 'definition
   `((tag_name) @font-lock-function-name-face)

   :language 'html
   :override 'keep
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
  "C-c C-q" #'php-ts-mode--indent-defun
  "C-c ." #'php-ts-mode-set-style
  "C-c C-c" #'comment-region
  "C-c C-k" #'c-ts-mode-toggle-comment-style)

;;;###autoload
;; TODO: ste due funzioni vanno fuse
(define-derived-mode php-ts-mode prog-mode "PHP"
  "Major mode for editing PHP, powered by tree-sitter.

\\{php-ts-mode-map}"
  :group 'php
  :syntax-table php-ts-mode--syntax-table

  (unless (treesit-ready-p 'php)
    (error "Tree-sitter for PHP isn't available"))

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
	      (regexp-opt '("function_definition"
			    "method_declaration"
			    "class_declaration"
			    "interface_declaration"
			    "trait_declaration"
			    "enum_declaration")))

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

  (setq-local treesit-thing-settings
	      `((php
		 (defun ,treesit-defun-type-regexp)
		 ;;(sexp ,'(treesit-sexp-type-regexp))
		 (sexp (not ,(rx (or "{" "}" "[" "]" "(" ")" ","))))
		 (sentence  ,treesit-sentence-type-regexp)
		 (text ,(regexp-opt '("comment" "text"))))))

  ;; Nodes like struct/enum/union_specifier can appear in
  ;; function_definitions, so we need to find the top-level node.
  (setq-local treesit-defun-prefer-top-level t)

  ;; Indent.
  ;; TODO: check php-mode.el:714
  (when (eq php-ts-mode-indent-style 'default)
    (setq-local indent-tabs-mode nil)) ;; TODO: check if needed for PHP

  (setq-local c-ts-common-indent-offset 'php-ts-mode-indent-offset)
  (setq-local treesit-simple-indent-rules (php-ts-mode--get-indent-style))

  ;; (when (not (eq php-ts-indent-style 'default))
  ;;   (setq-local fill-column 78))

  ;; Comment
  (php-ts-mode-comment-setup)

  ;; Electric
  (setq-local electric-indent-chars
	      (append "{}():;," electric-indent-chars))

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings
	      '(("Enum" "\\`enum_declaration\\'" nil nil)
		("Variable" "\\`variable_name\\'" nil nil)
		("Class" "\\`class_declaration\\'" nil nil)
		("Function" "\\`function_definition\\'" nil nil)
		("Method" "\\`method_declaration\\'" nil nil)
		("Trait" "\\`trait_declaration\\'" nil nil)))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings (php-ts-mode--font-lock-settings))

  (setq-local treesit-font-lock-feature-list
	      '(( comment definition spell)
		( keyword string type name)
		( attribute assignment constant escape-sequence
		  base-clause literal function-name variable-name)
		( argument bracket delimiter error operator property)))

  ;; Embed html, if possible
  (when (not php-ts-mode-disable-inject)
    (if (and (treesit-ready-p 'html) (treesit-ready-p 'javascript) (treesit-ready-p 'css))
	(progn
	  (setq-local html-ts-parser (treesit-parser-create 'html)
		      css-ts-parser (treesit-parser-create 'css)
		      javascript-ts-parser (treesit-parser-create 'javascript))

	  (setq-local treesit-range-settings
		      (treesit-range-rules
		       :embed 'html
		       :host 'php
		       '((program (text) @cap)
			 (text_interpolation (text) @cap))

		       :embed 'javascript
		       :host 'html
		       :offset '(1 . -1)
		       '((script_element
			  (start_tag (tag_name))
			  (raw_text) @cap))

		       :embed 'css
		       :host 'html
		       :offset '(1 . -1)
		       '((style_element
			  (start_tag (tag_name))
			  (raw_text) @cap))))

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
		      '(( comment definition spell
			  ;; CSS
			  query selector
			  ;; HTML
			  text
			  ;; PHPDOC
			  document )
			( keyword string type name)
			( attribute assignment constant escape-sequence
			  base-clause literal function-name variable-name variable
			  ;; Javascript
			  jsx number pattern string-interpolation
			  )
			( argument bracket delimiter error function operator property))))
      (warn "Tree-sitter for Html (with javascript and css) isn't available. Html and/or Javascript and/or CSS syntax support isn't available to php-ts-mode. You could run `php-ts-mode-install-parser' to install the required parsers.")))

  ;;Embed phpdoc, if possible
  (if (treesit-ready-p 'phpdoc)
      (progn
	(setq-local phpdoc-ts-parser (treesit-parser-create 'phpdoc))
	(setq-local treesit-range-settings
		    (append treesit-range-settings
			    (treesit-range-rules
			     :embed 'phpdoc
			     :host 'php
			     :local t
			     `(((comment) @phpdoc (:match "^/\\*\\*" @phpdoc)))
			     )))

	(setq-local treesit-font-lock-settings
		    (append treesit-font-lock-settings
			    (php-ts-mode--phpdoc-font-lock-settings)))
	(setq-local treesit-simple-indent-rules
		    (append treesit-simple-indent-rules
			    php-ts-mode--phpdoc-indent-styles)))
    (warn "Tree-sitter for PHPDOC isn't available. Phpdoc syntax support isn't available to php-ts-mode.You could run `php-ts-mode-install-parser' to install the required parsers."))

  ;; Which-function.
  (setq-local which-func-functions (treesit-defun-at-point))

  ;; Align.
  ;; TODO: it's usefull?
  (setq-local align-indent-before-aligning t)

  ;; should be the last one
  (setq-local php-ts-parser (treesit-parser-create 'php))
  (treesit-font-lock-recompute-features)
  (treesit-major-mode-setup))

(if (treesit-ready-p 'php)
    (progn
      (add-to-list 'auto-mode-alist '("\\.\\(?:php[s345]?\\|phtml\\)\\'" . php-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.\\(?:php\\|inc\\|stub\\)\\'" . php-ts-mode))
      (add-to-list 'auto-mode-alist '("/\\.php_cs\\(?:\\.dist\\)?\\'" . php-ts-mode))))

(provide 'php-ts-mode)
;;; php-ts-mode.el ends here
