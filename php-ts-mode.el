;;; php-ts-mode.el --- Major mode for editing PHP files using tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Vincenzo Pupillo

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
;; html/javascript/css parsers can be disabled if you don't need them.
;;
;; This package is compatible with and was tested against those tree-sitter grammars:
;; * https://github.com/tree-sitter/tree-sitter-php
;; * https://github.com/tree-sitter/tree-sitter-html
;; * https://github.com/tree-sitter/tree-sitter-javascript
;; * https://github.com/tree-sitter/tree-sitter-css
;;
;; Features
;;
;; * Indent
;; * IMenu
;; * Navigation
;; * Which-function
;; * Tree-sitter parser installation helper
;; * PHP built-in server support
;; * Shell interaction: execute PHP code in a inferior PHP process

;;; Code:

(require 'treesit)
(require 'c-ts-common) ;; For comment indent and filling.
(require 'html-ts-mode) ;; For embed html
(require 'css-mode) ;; for embed css into html
(require 'js) ;; for embed javascript html
(require 'comint)

(eval-when-compile
  (require 'cl-lib)
  (require 'rx)
  (require 'subr-x))

(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-string "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-parser-add-notifier "treesit.c")
(declare-function treesit-parser-buffer "treesit.c")
(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-parser-included-ranges "treesit.c")
(declare-function treesit-parser-list "treesit.c")
(declare-function treesit-parser-language "treesit.c")

;;; Install treesitter language parsers
(defvar php-ts-mode--language-source-alist
  '((php . ("https://github.com/tree-sitter/tree-sitter-php"))
    (html . ("https://github.com/tree-sitter/tree-sitter-html"))
    (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
    (css . ("https://github.com/tree-sitter/tree-sitter-css")))
  "Treesitter language parser alist required by `php-ts-mode'.

You can customize this variable if you want to stick to a specific
commit and/or use different parsers.")

(defun php-ts-mode-install-parsers ()
  "Install all the required treesitter parser."
  (interactive)
  (let ((treesit-language-source-alist php-ts-mode--language-source-alist))
    (dolist (item php-ts-mode--language-source-alist)
      (treesit-install-language-grammar (car item)))))

;;; Custom variables

(defcustom php-ts-mode-indent-offset 4
  "Number of spaces for each indentation step (default) in `php-ts-mode'."
  :tag "PHP indent offset"
  :version "30.1"
  :type 'integer
  :safe 'integerp
  :group 'php-ts)

(defcustom php-ts-mode-js-css-indent-offset html-ts-mode-indent-offset
  "JavaScript and CSS indent spaces related to the <script> and <style> html tags.
By default, the value is the same as `html-ts-mode-indent-offset'"
  :tag "PHP javascript or css indent offset"
  :version "30.1"
  :type 'integer
  :safe 'integerp
  :group 'php-ts)

(defcustom php-ts-mode-php-executable (or (executable-find "php") "/usr/bin/php")
  "The location of PHP executable."
  :tag "PHP Executable"
  :version "30.1"
  :type 'string
  :safe 'stringp
  :group 'php-ts)

(defcustom php-ts-mode-php-config nil
  "The location of php.ini file. If nil php use the default one."
  :tag "PHP Init file"
  :version "30.1"
  :type 'string
  :safe 'stringp
  :group 'php-ts)

(defcustom php-ts-mode-ws-hostname "localhost"
  "The hostname that will be served by the PHP built-in webserver.
If nil, then `php-ts-mode-run-php-webserver' will ask you for the hostname.
See `https://www.php.net/manual/en/features.commandline.webserver.php'."
  :tag "PHP built-in web server hostname"
  :version "30.1"
  :type 'string
  :safe 'stringp
  :group 'php-ts)

(defcustom php-ts-mode-ws-port 3000
  "The port on which the PHP built-in webserver will listen.
If nil, then `php-ts-mode-run-php-webserver' will ask you for the port number."
  :tag "PHP built-in web server port"
  :version "30.1"
  :type 'integer
  :safe 'integerp
  :group 'php-ts)

(defcustom php-ts-mode-ws-document-root nil
  "The root of the documents that the PHP built-in webserver will serve.
If nil, then `php-ts-mode-run-php-webserver' will ask you for the document root."
  :tag "PHP built-in web server document root"
  :version "30.1"
  :type 'string
  :safe 'stringp
  :group 'php-ts)

(defcustom php-ts-mode-ws-router nil
  "The router script that will be executed by the PHP built-in webserver.
Useful if it's not the usual index.php."
  :tag "PHP built-in web server router"
  :version "30.1"
  :type 'string
  :safe 'stringp
  :group 'php-ts)

(defcustom php-ts-mode-ws-workers nil
  "The number of workers the PHP built-in webserver will fork.
In order to test code, for e.g. , that requires multiple concurrent requests."
  :tag "PHP built-in number of workers"
  :version "30.1"
  :type 'integer
  :safe 'integerp
  :group 'php-ts)

(defcustom php-ts-mode-inferior-buffer "*PHP*"
  "Name of the inferior PHP buffer."
  :tag "PHP inferior process buffer name"
  :type 'string
  :safe 'stringp
  :group 'php-ts
  :version "30.1")

(defcustom php-ts-mode-inferior-history nil
  "File used to save command history of the inferior PHP process."
  :tag "PHP inferior process history file."
  :type '(choice (const :tag "None" nil) file)
  :safe 'string-or-null-p
  :group 'php-ts
  :version "30.1")

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
  :tag "PHP indent style"
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
  "If true disable syntax hightlight of html/css/javascript injected languages."
  :tag "Disable syntax highlight of html/css/javascript"
  :version "30.1"
  :type 'boolean
  :safe 'booleanp
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

(defun php-ts-mode--set-indent-property (style)
  "Set the offset, tab, etc. according to STYLE."
  (cl-case style
    (psr2 (setq php-ts-mode-indent-offset 4
		tab-width 4
		indent-tabs-mode nil))
    (pear (setq php-ts-mode-indent-offset 4
		tab-width 4
		indent-tabs-mode nil))
    (drupal (setq php-ts-mode-indent-offset 2
		  tab-width 2
		  indent-tabs-mode nil))
    (wordpress (setq php-ts-mode-indent-offset 2
		     tab-width 2
		     indent-tabs-mode t))
    (symfony (setq php-ts-mode-indent-offset 4
		   tab-width 4
		   indent-tabs-mode nil))
    (zend (setq php-ts-mode-indent-offset 4
		tab-width 4
		indent-tabs-mode nil))))

(defun php-ts-mode-set-style (style)
  "Set the PHP indent style of the current buffer to STYLE.

To set the default indent style globally, use
`php-ts-mode-set-global-style'."
  (interactive (list (php-ts-mode--prompt-for-style)))
  (if (not (derived-mode-p 'php-ts-mode))
      (user-error "The current buffer is not in `php-ts-mode'")
    (setq-local php-ts-mode-indent-style style)
    (php-ts-mode--set-indent-property style)
    (setq treesit-simple-indent-rules
	  (treesit--indent-rules-optimize
	   (php-ts-mode--get-indent-style)))))

(defun php-ts-mode--get-parser-ranges ()
  "Return the ranges covered by the parsers.

`php-ts-mode' use 4 parsers, this function returns, for the
current buffer, the ranges covered by each parser.
Usefull for debugging."
  (let ((ranges)
	(parsers (treesit-parser-list nil nil t)))
    (if (not parsers)
	(message "At least one parser must be initialized"))
    (cl-loop
     for parser in parsers
     do (push (list parser (treesit-parser-included-ranges parser)) ranges)
     finally return ranges)))

;;; Syntax table

(defvar php-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; Taken from the cc-langs version
    (modify-syntax-entry ?\\ "\\"     table)
    (modify-syntax-entry ?+  "."      table)
    (modify-syntax-entry ?-  "."      table)
    (modify-syntax-entry ?=  "."      table)
    (modify-syntax-entry ?%  "."      table)
    (modify-syntax-entry ?<  "."      table)
    (modify-syntax-entry ?>  "."      table)
    (modify-syntax-entry ?&  "."      table)
    (modify-syntax-entry ?|  "."      table)
    (modify-syntax-entry ?\' "\""     table)
    (modify-syntax-entry ?\240 "."    table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)
    ;; php specific syntax
    (modify-syntax-entry ?_  "w"      table)
    (modify-syntax-entry ?`  "\""     table)
    (modify-syntax-entry ?\" "\""     table)
    (modify-syntax-entry ?\r "> b"    table)
    (modify-syntax-entry ?#  "< b"    table)
    (modify-syntax-entry ?$  "_"      table)
    table)
  "Syntax table for `php-ts-mode'.")

;;; Indent

;; taken from c-ts-mode
(defun php-ts-mode--else-heuristic (node parent bol &rest _)
  "Heuristic matcher for when \"else\" is followed by a closing bracket.
NODE, PARENT, and BOL are the same as in other matchers."
  (and (null node)
       (save-excursion
	 (forward-line -1)
	 (looking-at (eval-when-compile (rx (* whitespace) "else" (* whitespace) eol))))
       (let ((next-node (treesit-node-first-child-for-pos parent bol)))
	 (equal (treesit-node-type next-node) "}"))))

;; taken from c-ts-mode
(defun php-ts-mode--first-sibling (node parent &rest _)
  "Matches when NODE is the \"first sibling\".
\"First sibling\" is defined as: the first child node of PARENT
such that it's on its own line.  NODE is the node to match and
PARENT is its parent."
  (let ((prev-sibling (treesit-node-prev-sibling node t)))
    (or (null prev-sibling)
	(save-excursion
	  (goto-char (treesit-node-start prev-sibling))
	  (<= (line-beginning-position)
	      (treesit-node-start parent)
	      (line-end-position))))))

(defun php-ts-mode--js-css-tag-bol (node parent &rest _)
  "Find the first non-space caracters of html tags <script> or <style>.
If NODE is null return `line-beginning-position'. PARENT is ignored."
  (if (null node)
      (line-beginning-position)
    (save-excursion
      (goto-char (treesit-node-start node))
      (re-search-backward "<script>\\|<style>" nil t))))

(defun php-ts-mode--parent-eol (node parent &rest _)
  "Find the last non-space caracters of the PARENT of the current NODE."
  (save-excursion
    (goto-char (treesit-node-start parent))
    (line-end-position)))

(defun php-ts-mode--indent-styles ()
  "Indent rules supported by `php-ts-mode'."
  (let ((common
	 `(((or (node-is "program")
		(node-is "php_tag"))
	    parent-bol 0)

	   (php-ts-mode--else-heuristic prev-line php-ts-mode-indent-offset)

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
	   ((node-is "class_interface_clause") parent-bol php-ts-mode-indent-offset)
	   ;;((query "(class_interface_clause (name) @indent)") parent-bol php-ts-mode-indent-offset)
	   ((query "(class_interface_clause (name) @indent)") php-ts-mode--parent-eol 1)
	   ((query "(class_interface_clause (qualified_name) @indent)") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "class_declaration") parent-bol 0)
	   ((parent-is "function_definition") parent-bol 0)
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
	   ;;((parent-is "declaration_list") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "declaration_list") column-0 php-ts-mode-indent-offset)
	   ;;((parent-is "attribute") parent-bol 0)
	   ((parent-is "initializer_list") parent-bol php-ts-mode-indent-offset)

	   ;; Statement in {} blocks.
	   ;; ((or (match nil "compound_statement" nil 1 1)
	   ;;	(match null "compound_statement"))
	   ;;  standalone-parent php-ts-mode-indent-offset)
	   ((or (and (parent-is "compound_statement")
		     ;; If the previous sibling(s) are not on their
		     ;; own line, indent as if this node is the first
		     ;; sibling (Bug#67357)
		     php-ts-mode--first-sibling)
		(match null "compound_statement"))
	    standalone-parent php-ts-mode-indent-offset)
	   ((parent-is "compound_statement") parent-bol php-ts-mode-indent-offset)
	   ;; Opening bracket.
	   ;;((node-is "compound_statement") standalone-parent php-ts-mode-indent-offset)
	   ;; Opening bracket without closing bracket
	   ;; this is a workaround, treesit-php-mode mark the { as (ERROR "{")
	   ;;((parent-is "{") parent-bol php-ts-mode-indent-offset)

	   ((parent-is "match_block") parent-bol php-ts-mode-indent-offset)

	   ;; These rules are for cases where the body is bracketless.
	   ((query "(do_statement \"while\" @indent)") parent-bol 0)
	   ((or (parent-is "switch_statement")
		(parent-is "case_statement")
		(parent-is "if_statement")
		(parent-is "else_clause")
		(parent-is "for_statement")
		(parent-is "while_statement")
		(parent-is "do_statement"))
	    parent-bol php-ts-mode-indent-offset)
	   )))
    `((psr2
       ((parent-is "function_call_expression") parent-bol php-ts-mode-indent-offset)
       ;;       ((parent-is "array_creation_expression") parent 1) ;; questa perchè l'ho messa qui?
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
       ((parent-is "binary_expression") first-sibling 0)
       ,@common))))

;;; Font-lock
(defconst php-ts-mode--keywords
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

(defconst php-ts-mode--operators
  '("**=" "*=" "/=" "%=" "+=" "-=" ".=" "<<=" ">>=" "&=" "^=" "|="
    "??"  "??=" "||" "&&" "|" "^" "&" "==" "!=" "<>" "===" "!==" "<"
    ">" "<=" ">=" "<=>" "<<" ">>" "+" "-" "." "*" "**" "/"
    "%" "->" "?->")
  "PHP operators for tree-sitter font-locking.")

(defconst php-ts-mode--predefined-constant
  '(;; predefined constant
    "PHP_VERSION" "PHP_MAJOR_VERSION" "PHP_MINOR_VERSION"
    "PHP_RELEASE_VERSION" "PHP_VERSION_ID" "PHP_EXTRA_VERSION"
    "ZEND_THREAD_SAFE" "ZEND_DEBUG_BUILD" "PHP_ZTS" "PHP_DEBUG"
    "PHP_MAXPATHLEN" "PHP_OS" "PHP_OS_FAMILY" "PHP_SAPI" "PHP_EOL"
    "PHP_INT_MAX" "PHP_INT_MIN" "PHP_INT_SIZE" "PHP_FLOAT_DIG"
    "PHP_FLOAT_EPSILON" "PHP_FLOAT_MIN" "PHP_FLOAT_MAX"
    "PHP_WINDOWS_EVENT_CTRL_C" "PHP_WINDOWS_EVENT_CTRL_BREAK"
    "DEFAULT_INCLUDE_PATH" "PEAR_INSTALL_DIR" "PEAR_EXTENSION_DIR"
    "PHP_EXTENSION_DIR" "PHP_PREFIX" "PHP_BINDIR" "PHP_BINARY"
    "PHP_MANDIR" "PHP_LIBDIR" "PHP_DATADIR" "PHP_SYSCONFDIR"
    "PHP_LOCALSTATEDIR" "PHP_CONFIG_FILE_PATH" "PHP_CONFIG_FILE_SCAN_DIR"
    "PHP_SHLIB_SUFFIX" "PHP_FD_SETSIZE" "E_ERROR" "E_WARNING" "E_PARSE"
    "E_NOTICE" "E_CORE_ERROR" "E_CORE_WARNING" "E_COMPILE_ERROR"
    "E_COMPILE_WARNING" "E_USER_ERROR" "E_USER_WARNING"
    "E_USER_NOTICE" "E_USER_NOTICE" "E_DEPRECATED" "E_USER_DEPRECATED"
    "E_ALL" "E_STRICT"
    ;; magic constant
    "__COMPILER_HALT_OFFSET__" "__CLASS__" "__DIR__" "__FILE__"
    "__FUNCTION__" "__LINE__" "__METHOD__" "__NAMESPACE__" "__TRAIT__")
  "PHP predefined constant.")

(defun php-ts-mode--font-lock-settings ()
  "Tree-sitter font-lock settings."
  (treesit-font-lock-rules

   :language 'php
   :feature 'keyword
   :override t
   `([,@php-ts-mode--keywords] @font-lock-keyword-face)

   :language 'php
   :feature 'comment
   :override t
   ;; '((comment) @font-lock-comment-face
   ;;   (comment) @contextual)
   ;; '((comment) @php-ts-mode--font-lock-comment
   ;;   (comment) @contextual)
   '((comment) @php-ts-mode--font-lock-comment
     (comment) @contextual)

   :language 'php
   :feature 'constant
   `((boolean) @font-lock-constant-face
     (float) @font-lock-constant-face
     (integer) @font-lock-constant-face
     (null) @font-lock-constant-face
     ;; predefined constant or built in constant
     ((name) @font-lock-builtin-face
      (:match ,(rx-to-string
		`(: bos (or ,@php-ts-mode--predefined-constant) eos))
	      @font-lock-builtin-face))
     ;; user defined constant
     ((name) @font-lock-constant-face
      (:match "_?[A-Z][0-9A-Z_]+" @font-lock-constant-face))
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
      scope: (name) @font-lock-constant-face)
     (error_suppression_expression (name) @font-lock-variable-name-face))

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
     ;;(intersection_type) @font-lock-type-face
     (primitive_type) @font-lock-type-face
     (cast_type) @font-lock-type-face
     (named_type) @font-lock-type-face
     (optional_type) @font-lock-type-face)

   :language 'php
   :feature 'definition
   :override t
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
   :feature 'function-scope
   :override t
   '((relative_scope) @font-lock-constant-face
     (scoped_call_expression
      scope: (name) @font-lock-constant-face)
     (class_constant_access_expression (name) @font-lock-constant-face))

   :language 'php
   :feature  'function-call
   :override t
   '((function_call_expression
      function: (_) @font-lock-function-call-face)
     (scoped_call_expression
      name: (_) @font-lock-function-name-face)
     (member_call_expression
      name: (_) @font-lock-function-name-face)
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
   ;;'((ERROR) @font-lock-warning-face)
   '((ERROR) @php-ts-mode--fontify-error)))

;;; Font-lock helpers

;; phpdoc font-lock adapted from php-mode
(defconst php-ts-mode--phpdoc-types
  '("string" "integer" "int" "boolean" "bool" "float"
    "double" "object" "mixed" "array" "resource"
    "void" "null" "false" "true" "self" "static"
    "callable" "iterable" "number"
    ;; PHPStan and Psalm types
    "array-key" "associative-array" "callable-array" "callable-object"
    "callable-string" "class-string" "empty" "enum-string" "list"
    "literal-string" "negative-int" "non-positive-int" "non-negative-int"
    "never" "never-return" "never-returns" "no-return" "non-empty-array"
    "non-empty-list" "non-empty-string" "non-falsy-string"
    "numeric" "numeric-string" "positive-int" "scalar"
    "trait-string" "truthy-string" "key-of" "value-of")
  "A list of type and pseudotype names that can be used in PHPDoc.")

(defconst php-ts-mode--phpdoc-tags
  '("assert" "assert-if-true" "assert-if-false" "if-this-is" "ignore"
    "category" "todo" "copyright" "subpackage" "example" "license"
    "source" "use" "uses" "method" "deprecated" "since" "version"
    "package" "param" "property" "property-read" "property-write"
    "return" "throws" "var" "self-out" "this-out" "param-out" "template"
    "template-covariant" "template-extends" "template-implements"
    "template-use" "type" "extends" "require-extends" "implemtents"
    "require-implements" "author" "inheritdoc" "inheritDoc" "api"
    "filesource" "internal")
  "A list of tags specifying type names.")

(defun php-ts-mode--font-lock-comment (node override start end &rest _)
  "Fontify the comments and phpdoc comment.

    For NODE, OVERRIDE, START, and END, see `treesit-font-lock-rules'."
  (save-excursion
    (let ((node-start (treesit-node-start node))
	  (node-end (treesit-node-end node)))
      (goto-char node-start)
      (cond ((looking-at-p "/\\*\\*")
	     ;; first of all fontify the doc comments
	     (treesit-fontify-with-override node-start node-end
					    'font-lock-doc-face
					    override start end)
	     ;; (goto-char node-start)
	     ;; ;;(message "firt while")
	     ;; "{@foo ...}" markup.
	     (while (re-search-forward "{@[-[:alpha:]]+\\s-*\\([^}]*\\)}" node-end t)
	       ;;(message " mb-0 = %d, mb-end-0 = %d" (match-beginning 0)  (match-end 0))
	       (treesit-fontify-with-override (match-beginning 0) (match-end 0)
					      'font-lock-doc-markup-face
					      override start end))
	     (goto-char node-start)
	     ;;(message "second while")
	     ;; variable
	     (while (re-search-forward (eval-when-compile (rx (group "$") (group (in "A-Za-z_") (* (in "0-9A-Za-z_"))))) node-end t)
	       ;;(message " mb-1 = %d, mb-end-1 = %d" (match-beginning 1)  (match-end 1))
	       (treesit-fontify-with-override (match-beginning 1) (match-end 1)
					      'font-lock-operator-face
					      override start end)
	       ;;(message " mb-1 = %d, mb-end-1 = %d" (match-beginning 2)  (match-end 2))
	       (treesit-fontify-with-override (match-beginning 2) (match-end 2)
					      'font-lock-variable-name-face
					      override start end))
	     (goto-char node-start)
	     ;; $this
	     (while (re-search-forward "\\(\\$\\)\\(this\\)\\>" node-end t)
	       ;;(message " mb-1 = %d, mb-end-1 = %d" (match-beginning 1)  (match-end 1))
	       (treesit-fontify-with-override (match-beginning 1) (match-end 1)
					      'font-lock-operator-face
					      override start end)
	       ;;(message " mb-2 = %d, mb-end-2 = %d" (match-beginning 2)  (match-end 2))
	       (treesit-fontify-with-override (match-beginning 2) (match-end 2)
					      'font-lock-keyword-face
					      override start end))
	     (goto-char node-start)
	     ;;(message "third while")
	     ;; php-doc etc. tags
	     (while (re-search-forward (concat "\\s-@" (eval-when-compile (rx (? (or "phan" "phpstan" "psalm") "-")))
					       (regexp-opt php-ts-mode--phpdoc-tags)
					       "\\s-+"
					       "\\("
					       (eval-when-compile (rx (+ (? "?") (? "\\") (+ (in "0-9A-Z_a-z")) (? "[]") (? "|") (? ".") (? "-"))))
					       "\\)+")
				       node-end t)
	       ;;(message " mb-0 = %d, mb-end-0 = %d" (match-beginning 0)  (match-end 0))
	       (treesit-fontify-with-override (match-beginning 0) (match-end 0)
					      'font-lock-doc-markup-face
					      override start end)
	       (treesit-fontify-with-override (match-beginning 1) (match-end 1)
					      'font-lock-constant-face
					      override start end))
	     (goto-char node-start)
	     ;;(message "forth while")
	     ;; php-doc types
	     (while (re-search-forward (concat "\\(?:|\\|\\?\\|\\s-\\)\\(" (regexp-opt php-ts-mode--phpdoc-types 'words) "\\)")
				       node-end t)
	       ;;(message " mb-1 = %d, mb-end-1 = %d" (match-beginning 1)  (match-end 1))
	       (treesit-fontify-with-override (match-beginning 1) (match-end 1)
					      'font-lock-type-face
					      override start end))
	     ;;(message "fifth while")
	     (goto-char node-start)
	     ;; "@foo ... or @foo(...) ..." markup.
	     (while (re-search-forward "\\(@[[:alpha:]][-[:alpha:]\\]*\\)\\(([a-zA-0]+=[a-zA-0]+)\\)*" node-end t)
	       ;;(message "test mb-1 = %d, mb-end-1 = %d" (match-beginning 1)  (match-end 1))
	       (treesit-fontify-with-override (match-beginning 1) (match-end 1)
					      'font-lock-doc-markup-face
					      override start end)
	       (when (match-beginning 2)
		 (treesit-fontify-with-override (match-beginning 2) (match-end 2)
						'font-lock-property-use-face
						override start end))
	       )
	     ;;(goto-char node-start)
	     ;;	;; HTML tags.
	     ;; (while (re-search-forward (concat "</?\\sw"
	     ;;					       "\\("
	     ;;					       (concat "\\sw\\|\\s \\|[=\n\r*.:]\\|"
	     ;;						       "\"[^\"]*\"\\|'[^']*'")
	     ;;					       "\\)*>") node-end t)
	     ;;   (treesit-fontify-with-override (match-beginning 0) (match-end 0)
	     ;;					      'font-lock-doc-markup-face
	     ;;					      override start end))
	     (goto-char node-start)
	     ;; email
	     (while (re-search-forward "<[a-zA-0_.]+@[a-zA-0_.]+>" node-end t)
	       (treesit-fontify-with-override (match-beginning 0) (match-end 0)
					      'font-lock-doc-markup-face
					      override start end))

	     )
	    ((or (looking-at-p "/\\*") (looking-at-p "//") (looking-at-p "#"))
	     ;; fontify others comment
	     (treesit-fontify-with-override node-start node-end
					    'font-lock-comment-face
					    override start end))))))

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
  (let* ((node (treesit-node-at point 'html))
	 (parent (treesit-node-parent node))
	 (node-query (format "(%s (%s))" (treesit-node-type parent) (treesit-node-type node))))
    ;;(message "node-query = %s" node-query)
    (cond
     ((string-equal "(script_element (raw_text))" node-query) 'javascript)
     ((string-equal "(style_element (raw_text))" node-query) 'css)
     (t 'html))))

(defun php-ts-mode--language-at-point (point)
  "Return the language at POINT."
  (let* ((node (treesit-node-at point 'php))
	 (parent (treesit-node-parent node))
	 (node-query (format "(%s (%s))" (treesit-node-type parent) (treesit-node-type node))))
    (if (not (member node-query '("(program (text))"
				  "(text_interpolation (text))")))
	'php
      (php-ts-mode--html-language-at-point point))))

;;; Imenu

(defun php-ts-mode--parent-object (node)
  "Return the name of the object that own NODE."
  (treesit-parent-until
   node
   (lambda (n)
     (member (treesit-node-type n)
	     '("class_declaration"
	       "enum_declaration"
	       "function_definition"
	       "interface_declaration"
	       "method_declaration"
	       "namespace_definition"
	       "trait_declaration")))))

(defun php-ts-mode--defun-name-separator (node)
  "Return a separator to connect object name, based on NODE type."
  (let ((node-type (treesit-node-type node)))
    (cond ((member node-type '("function_definition" "method_declaration"))
	   "()::")
	  ((member node-type '("class_declaration" "enum_declaration" "trait_declaration"))
	   "::")
	  (t "\\"))))

(defun php-ts-mode--defun-object-name (node node-text)
  "Compose the full name of a NODE that is a PHP variable, method, class etc.

If the NODE has a parent recursively compose the parents name whith NODE-TEXT,
othewire return NODE-TEXT."
  (let* ((parent-node (php-ts-mode--parent-object node))
	 (parent-node-text
	  (treesit-node-text
	   (treesit-node-child-by-field-name parent-node "name") t))
	 (parent-node-separator (php-ts-mode--defun-name-separator parent-node)))
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
      (namespace_definition (treesit-node-text child t))
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
  (setq-local c-ts-common--comment-regexp "comment")
  (setq-local comment-start "// ")
  (setq-local comment-style 'extra-line)
  (setq-local comment-continue "* ")
  (setq-local comment-start-skip
	      (eval-when-compile
		(rx (group (or (: "#" (not (any "[")))
			       (: "/" (+ "/"))
			       (: "/*"))
			   (* (syntax whitespace))))))
  (setq-local comment-end ""))

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
	     node (eval-when-compile
		    (rx (or "variable_name"
			    "function_definition"
			    "enum_declaration"
			    "union_declaration"
			    "declaration")))))))

;;; Injected tree-sitter helper
(defconst php-ts-mode--custom-html-font-lock-settings
  (treesit-font-lock-rules
   :language 'html
   :override t
   :feature 'comment
   `((comment) @font-lock-comment-face
     ;; handle shebang path and others type of comment
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
   :override 'append
   :feature 'string
   `((quoted_attribute_value) @font-lock-string-face)

   :language 'html
   :override t
   :feature 'property
   `((attribute_name) @font-lock-variable-name-face))

  ;; (append (treesit-font-lock-rules
  ;;	   :language 'html
  ;;	   :override t
  ;;	   :feature 'comment
  ;;	   `((comment) @font-lock-comment-face
  ;;	     (fragment (text) @font-lock-comment-face))
  ;;	   :language 'html
  ;;	   :override 'append
  ;;	   :feature 'string
  ;;	   `((quoted_attribute_value) @font-lock-string-face))
  ;;	  html-ts-mode--font-lock-settings)
  "Tree-sitter font-lock settings for `php-html-ts-mode'.")

;;; Modes

(defun php-ts-mode-set-comment-style ()
  "Set a different comment style."
  (interactive)
  (setq-local comment-start
	      (completing-read
	       "Choose comment style: "
	       '("// " "/* " "# ") nil t nil nil "// "))
  (if (string= comment-start "/* ")
      (setq-local comment-end " */")
    (setq-local comment-add nil))
  (setq mode-name (concat "PHP" (string-trim-right comment-start)))
  (force-mode-line-update))

(defvar-keymap php-ts-mode-map
  :doc "Keymap for `php-ts-mode' buffers."
  :parent prog-mode-map
  "C-c C-q" #'php-ts-mode--indent-defun
  "C-c ." #'php-ts-mode-set-style
  "C-c C-c" #'comment-region
  "C-c C-k" #'php-ts-mode-set-comment-style)

(easy-menu-define php-ts-mode-menu php-ts-mode-map
  "Menu bar entry for `php-ts-mode'."
  `("Php"
    ["Comment Out Region" comment-region
     :enable mark-active
     :help "Comment out the region between the mark and point"]
    ["Uncomment Region" (comment-region (region-beginning)
					(region-end) '(4))
     :enable mark-active
     :help "Uncomment the region between the mark and point"]
    ["Indent Top-level Expression" php-ts-mode--indent-defun
     :help "Indent/reindent top-level function, class, etc."]
    ["Indent Line or Region" indent-for-tab-command
     :help "Indent current line or region, or insert a tab"]
    ["Forward Expression" forward-sexp
     :help "Move forward across one balanced expression"]
    ["Backward Expression" backward-sexp
     :help "Move back across one balanced expression"]
    ("Style..."
     ["Set Indentation Style..." php-ts-mode-set-style
      :help "Set C/C++ indentation style for current buffer"]
     ["Show Current Indentation Style"(message "Indentation Style: %s"
					       php-ts-mode-indent-style)
      :help "Show the name of the C/C++ indentation style for current buffer"]
     ["Set Comment Style" php-ts-mode-set-comment-style
      :help "Choose PHP comment style between block and line comments"])
    "--"
    ["Start interpreter" run-php
     :help "Run inferior PHP process in a separate buffer"]
    ["Show interpreter buffer" php-ts-mode-show-process-buffer]
    ["Hide interpreter buffer" php-ts-mode-hide-process-buffer]
    ["Kill interpreter process" php-ts-mode-kill-process]
    ["Evaluate buffer" php-ts-mode-send-buffer]
    ["Evaluate file" php-ts-mode-send-file]
    ["Evaluate region" php-ts-mode-send-region]
    "--"
    ["Start built-in webserver" php-ts-mode-run-php-webserver
     :help "Run the built-in PHP webserver"]
    "--"
    ["Customize" (lambda () (interactive) (customize-group "php-ts"))]))

;;;###autoload
(define-derived-mode php-ts-mode prog-mode "PHP"
  "Major mode for editing PHP, powered by tree-sitter.

\\{php-ts-mode-map}"
  :group 'php-ts
  :syntax-table php-ts-mode--syntax-table

  (unless (treesit-ready-p 'php) (error "Tree-sitter for PHP isn't
    available.  You can install the parsers with M-x
    php-ts-mode-install-parsers"))

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
	      (regexp-opt '("class_declaration"
			    "enum_declaration"
			    "function_definition"
			    "interface_declaration"
			    "method_declaration"
			    "namespace_definition"
			    "trait_declaration")))

  (setq-local treesit-defun-name-function #'php-ts-mode--defun-name)

  (setq-local treesit-thing-settings
	      `((php
		 (defun ,treesit-defun-type-regexp)
		 ;; (sexp ,  (regexp-opt
		 ;;	   '("definition"
		 ;;	     "qualifier"
		 ;;	     "type"
		 ;;	     "assignment"
		 ;;	     "expression"
		 ;;	     "literal"
		 ;;	     "string")))
		 (sexp (not ,(eval-when-compile
			       (rx (or "{" "}" "[" "]" "(" ")" ",")))))
		 (sentence  ,(regexp-opt
			      '("break_statement"
				"case_statement"
				"continue_statement"
				"declaration"
				"default_statement"
				"do_statement"
				"expression_statement"
				"for_statement"
				"if_statement"
				"return_statement"
				"switch_statement"
				"while_statement"
				"statement")))
		 (text ,(regexp-opt '("comment" "text"))))))

  ;; Nodes like struct/enum/union_specifier can appear in
  ;; function_definitions, so we need to find the top-level node.
  (setq-local treesit-defun-prefer-top-level t)

  ;; Indent.
  (setq-local indent-tabs-mode nil) ;; for Wordpress will be t

  (setq-local c-ts-common-indent-offset 'php-ts-mode-indent-offset)
  (setq-local treesit-simple-indent-rules (php-ts-mode--get-indent-style))

  ;; Comment
  (php-ts-mode-comment-setup)

  ;; PHP vars are case-sensitive
  (setq-local case-fold-search t)

  ;; Electric
  (setq-local electric-indent-chars
	      (append "{}():;," electric-indent-chars))

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings
	      '(("Class" "\\`class_declaration\\'" nil nil)
		("Enum" "\\`enum_declaration\\'" nil nil)
		("Function" "\\`function_definition\\'" nil nil)
		("Interface" "\\`interface_declaration\\'" nil nil)
		("Method" "\\`method_declaration\\'" nil nil)
		("Namespace" "\\`namespace_definition\\'" nil nil)
		("Trait" "\\`trait_declaration\\'" nil nil)
		("Variable" "\\`variable_name\\'" nil nil)))

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
	  (treesit-parser-create 'html)
	  (treesit-parser-create 'css)
	  (treesit-parser-create 'javascript)

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
			      `((javascript ((parent-is "program")
					     php-ts-mode--js-css-tag-bol
					     php-ts-mode-js-css-indent-offset)
					    ,@(cdr (car js--treesit-indent-rules))))
			      `((css ((parent-is "stylesheet")
				      php-ts-mode--js-css-tag-bol
				      php-ts-mode-js-css-indent-offset)
				     ,@(cdr (car css--treesit-indent-rules))))))

	  (setq-local treesit-language-at-point-function #'php-ts-mode--language-at-point)

	  (setq-local treesit-font-lock-feature-list
		      '(( comment definition spell
			  ;; CSS
			  query selector
			  ;; HTML
			  text)
			( keyword string type name)
			( attribute assignment constant escape-sequence function-scope
			  base-clause literal variable-name variable
			  ;; Javascript
			  jsx number pattern string-interpolation)
			(;; Javascript
			 function
			 argument bracket delimiter error function-call operator property))))
      (warn "Tree-sitter for Html (with javascript and css) isn't
      available. HTML and/or Javascript and/or CSS syntax support
      isn't available to php-ts-mode. You could run
      `php-ts-mode-install-parsers' to install the required parsers.")))

  ;; Which-function.
  (setq-local which-func-functions (treesit-defun-at-point))

  ;; Align.
  ;; TODO: it's usefull?
  (setq-local align-indent-before-aligning t)

  ;; should be the last one
  (setq-local php-ts-parser (treesit-parser-create 'php))
  (treesit-font-lock-recompute-features)
  (treesit-major-mode-setup))

;;;###autoload
(defun php-ts-mode-run-php-webserver (&optional port hostname document-root router num-of-workers)
  "Run the PHP Built-in web-server on a specified PORT.

`PORT': Port number of built-in web server, default `php-ts-mode-ws-port'.
`HOSTNAME': Hostname or IP address of Built-in web server,
default `php-ts-mode-ws-hostname'.
`DOCUMENT-ROOT': Path to Document root, default is the current directory.
`ROUTER': Path of the router PHP script,
see `https://www.php.net/manual/en/features.commandline.webserver.php'
`NUM-OF-WORKERS': Before run the web server set the
PHP_CLI_SERVER_WORKERS env variable in order to test code that
requires multiple concurrent requests to the built-in webserver."
  (interactive)
  (let* ((port (cond (port port)
		     (php-ts-mode-ws-port php-ts-mode-ws-port)
		     (t (read-number "Port: " 3000))))
	 (hostname (cond (hostname hostname)
			 (php-ts-mode-ws-hostname php-ts-mode-ws-hostname)
			 (t (read-string "Hostname: " "localhost"))))
	 (document-root (cond (document-root document-root)
			      (php-ts-mode-ws-document-root php-ts-mode-ws-document-root)
			      (t (read-string "Document-Root: " (file-name-directory (buffer-file-name))))))
	 (host (format "%s:%d" hostname port))
	 (name (format "PHP web server on: %s" host))
	 (buf-name (format "*%s*" name))
	 (args (delq
		nil
		(list "-S" host
		      "-t" document-root
		      (cond (router router)
			    (php-ts-mode-ws-router php-ts-mode-ws-router)
			    (t nil))
		      ))))
    (cond (num-of-workers (setenv "PHP_CLI_SERVER_WORKERS" num-of-workers))
	  (php-ts-mode-ws-workers (setenv "PHP_CLI_SERVER_WORKERS" php-ts-mode-ws-workers)))
    (if (get-buffer buf-name)
	(message "Switch to already running web server")
      (message "Run PHP built-in web server with args %s" (string-join args " ")))
    (apply #'make-comint name php-ts-mode-php-executable nil args)
    (funcall
     (if (called-interactively-p 'interactive) #'display-buffer #'get-buffer)
     buf-name)))

(define-derived-mode inferior-php-ts-mode comint-mode "Inferior PHP"
  "Major mode for PHP inferior process."
  :group 'php-ts
  (setq-local mode-line-process '(":%s")
	      ;;comint-input-ignoredups t
	      comint-highlight-input nil
	      comint-input-ring-file-name php-ts-mode-inferior-history
	      ;;comint-process-echoes t
	      comint-prompt-read-only t
	      comint-use-prompt-regexp t
	      comint-prompt-regexp "^php > "
	      comint-output-filter-functions '(ansi-color-process-output)
	      scroll-conservatively 1)
  (comint-read-input-ring t))

;;;###autoload
(defun run-php ()
  "Runs a PHP interpreter as a subprocess of Emacs, with PHP
I/O through an Emacs buffer.  Variables `php-ts-mode-php-executable'
and `php-ts-mode-php-config' control which PHP interpreter is run."
  (interactive)
  (let ((buffer (get-buffer-create php-ts-mode-inferior-buffer)))
    (unless (comint-check-proc buffer)
      (with-current-buffer buffer
	(inferior-php-ts-mode-startup)
	(inferior-php-ts-mode)))
    (when buffer
      (pop-to-buffer buffer))))

(defvar php-ts-mode--inferior-php-process nil
  "The PHP inferior process associated to `php-ts-mode-inferior-buffer'.")

(defun inferior-php-ts-mode-startup ()
  "Start an inferior PHP process."
  (setq-local php-ts-mode--inferior-php-process
	      (apply #'make-comint-in-buffer
		     (string-replace "*" "" php-ts-mode-inferior-buffer)
		     php-ts-mode-inferior-buffer
		     php-ts-mode-php-executable
		     nil
		     (delq
		      nil
		      (list
		       (when php-ts-mode-php-config
			 (format "-c %s" php-ts-mode-php-config))
		       "-a"))))
  (add-hook 'comint-preoutput-filter-functions
	    (lambda (string)
	      (if (member string '("php > " "php { " "/*  > "))
		  string
		(concat
		 (string-trim-right
		  (replace-regexp-in-string 
		   "\\(?:php [\">{] \\)+\\|\\(?:/\\*  > \\)+"
		   ""
		   string))
		 ;; Re-add the prompt for the next line.
		 "\nphp > ")))
	    nil t)
  (when php-ts-mode-inferior-history
    (set-process-sentinel
     php-ts-mode--inferior-php-process
     'php-ts-mode-inferior--write-history)))

(defun php-ts-mode-inferior--write-history (process _)
  "Write history file for inferior PHP PROCESS."
  ;; Depending on how the process is killed the buffer may not be
  ;; around anymore; e.g. `kill-buffer'.
  (when-let* ((buffer (process-buffer process))
	      ((buffer-live-p (process-buffer process))))
    (with-current-buffer buffer (comint-write-input-ring))))

(defun php-ts-mode-send-region (beg end)
  "Send region between BEG and END to the inferior PHP process."
  (interactive "r")
  (when-let* ((buffer-live-p php-ts-mode--inferior-php-process))
    (comint-send-region php-ts-mode--inferior-php-process beg end)))

(defun php-ts-mode-send-buffer ()
  "Send current buffer to the inferior PHP process."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward "<?php" nil t)
    (php-ts-mode-send-region (point) (point-max))))

(defun php-ts-mode-send-file (file)
  "Send contents of FILE to the inferior PHP process."
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents-literally file)
    (search-forward "<?php" nil t)
    (php-ts-mode-send-region (point) (point-max))))

(defun php-ts-mode-show-process-buffer ()
  "Show the inferior PHP process buffer."
  (interactive)
  (display-buffer php-ts-mode-inferior-buffer))

(defun php-ts-mode-hide-process-buffer ()
  "Hide the inferior PHP process buffer."
  (interactive)
  (delete-windows-on php-ts-mode-inferior-buffer))

(defun php-ts-mode-kill-process ()
  "Kill the inferior PHP process."
  (interactive)
  (with-current-buffer php-ts-mode-inferior-buffer
    (kill-buffer-and-window)))

(when (treesit-ready-p 'php)
      (add-to-list
       'auto-mode-alist '("\\.\\(?:php[s345]?\\|phtml\\)\\'" . php-ts-mode))
      (add-to-list
       'auto-mode-alist '("\\.\\(?:php\\|inc\\|stub\\)\\'" . php-ts-mode))
      (add-to-list
       'auto-mode-alist '("/\\.php_cs\\(?:\\.dist\\)?\\'" . php-ts-mode)))

(provide 'php-ts-mode)

;;; php-ts-mode.el ends here
