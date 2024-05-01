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
;; * https://github.com/claytonrcarter/tree-sitter-phpdoc
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
    (phpdoc . ("https://github.com/claytonrcarter/tree-sitter-phpdoc"))
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

(defgroup php-ts-mode nil
  "Major mode for editing PHP files."
  :prefix "php-ts-mode-"
  :group 'languages)

(defcustom php-ts-mode-indent-offset 4
  "Number of spaces for each indentation step (default) in `php-ts-mode'."
  :tag "PHP indent offset"
  :version "30.1"
  :type 'integer
  :safe 'integerp)

(defcustom php-ts-mode-js-css-indent-offset html-ts-mode-indent-offset
  "JavaScript and CSS indent spaces related to the <script> and <style> html tags.
By default, the value is the same as `html-ts-mode-indent-offset'"
  :tag "PHP javascript or css indent offset"
  :version "30.1"
  :type 'integer
  :safe 'integerp)

(defcustom php-ts-mode-php-executable (or (executable-find "php") "/usr/bin/php")
  "The location of PHP executable."
  :tag "PHP Executable"
  :version "30.1"
  :type 'string
  :safe 'stringp)

(defcustom php-ts-mode-php-config nil
  "The location of php.ini file. If nil php use the default one."
  :tag "PHP Init file"
  :version "30.1"
  :type 'string
  :safe 'stringp)

(defcustom php-ts-mode-ws-hostname "localhost"
  "The hostname that will be served by the PHP built-in webserver.
If nil, then `php-ts-mode-run-php-webserver' will ask you for the hostname.
See `https://www.php.net/manual/en/features.commandline.webserver.php'."
  :tag "PHP built-in web server hostname"
  :version "30.1"
  :type 'string
  :safe 'stringp)

(defcustom php-ts-mode-ws-port 3000
  "The port on which the PHP built-in webserver will listen.
If nil, then `php-ts-mode-run-php-webserver' will ask you for the port number."
  :tag "PHP built-in web server port"
  :version "30.1"
  :type 'integer
  :safe 'integerp)

(defcustom php-ts-mode-ws-document-root nil
  "The root of the documents that the PHP built-in webserver will serve.
If nil, then `php-ts-mode-run-php-webserver' will ask you for the document root."
  :tag "PHP built-in web server document root"
  :version "30.1"
  :type 'string
  :safe 'stringp)

(defcustom php-ts-mode-ws-router "index.php"
  "The router script that will be executed by the PHP built-in webserver.
Customize it if it is not the usual index.php.
If nil, then `php-ts-mode-run-php-webserver' will ask you for the router script."
  :tag "PHP built-in web server router"
  :version "30.1"
  :type 'string
  :safe 'stringp)

(defcustom php-ts-mode-ws-workers nil
  "The number of workers the PHP built-in webserver will fork.
In order to test code, for e.g. , that requires multiple concurrent requests."
  :tag "PHP built-in number of workers"
  :version "30.1"
  :type 'integer
  :safe 'integerp)

(defcustom php-ts-mode-inferior-buffer "*PHP*"
  "Name of the inferior PHP buffer."
  :tag "PHP inferior process buffer name"
  :version "30.1"
  :type 'string
  :safe 'stringp)

(defcustom php-ts-mode-inferior-history nil
  "File used to save command history of the inferior PHP process."
  :tag "PHP inferior process history file."
  :version "30.1"
  :type '(choice (const :tag "None" nil) file)
  :safe 'string-or-null-p)

(defvar php-ts-mode--inferior-prompt "php >"
  "Prompt used by PHP inferior process.")

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
  :safe 'c-ts-indent-style-safep)


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
    (wordpress (setq php-ts-mode-indent-offset 4
		     tab-width 4
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
  (cond
   ((not (derived-mode-p 'php-ts-mode)) (user-error "The current buffer is not in `php-ts-mode'"))
   ((equal php-ts-mode-indent-style style) (message "The style is already %s" style));; nothing to do
   (t (progn
	(setq-local php-ts-mode-indent-style style)
	(php-ts-mode--set-indent-property style)
	(let ((rules (assq-delete-all 'php treesit-simple-indent-rules))
	      (new-style (car (treesit--indent-rules-optimize
			       (php-ts-mode--get-indent-style)))))
	  (setq treesit-simple-indent-rules (cons new-style rules))
	  (message "Switch to %s style" style))))))

(defun php-ts-mode--get-parser-ranges ()
  "Return the ranges covered by the parsers.
`php-ts-mode' use 5 parsers, this function returns, for the
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
	 (looking-at (rx (* whitespace) "else" (* whitespace) eol)))
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

(defun php-ts-mode--parent-html-bol (node parent bol &rest _)
  "Find the first non-space characters of the html tags before NODE."
  (save-excursion
    (let ((html-node (treesit-search-forward node "text" t)))
      (if html-node
	  (let ((end-html (treesit-node-end html-node)))
	    (goto-char end-html)
	    (backward-word)
	    (back-to-indentation)
	    (point))
	;; forse è meglio usare bol, leggi la documentazione!!!
	(treesit-node-start parent)))))

(defun php-ts-mode--parent-html-heuristic (node parent bol &rest _)
  "Returns the position based on the html indentation.
Returns 0 if the NODE is after the </html>, otherwise returns the
indentation point of the last word before the NODE, plus the offset of
the indentation. If there is no html, it returns the beginning of the parent.
It can be used when you want to indent php code inside the html with
the offset and outside the html at 0"
  (let ((html-node (treesit-search-forward node "text" t)))
    (if html-node
	(let ((end-html (treesit-node-end html-node)))
	  (save-excursion
	    (goto-char end-html)
	    (backward-word)
	    (back-to-indentation)
	    (if (search-forward "</html>" end-html t 1)
		0
	      (+ (point) php-ts-mode-indent-offset))))
      ;; forse è meglio usare bol, leggi la documentazione!!!
      (treesit-node-start parent))))

(defun php-ts-mode--array-element-heuristic (node parent bol &rest _)
  "Return of the position of the first element of the array."
  ;; got to the line where is the parent
  (let ((parent-start
	 (treesit-node-start parent))
	(parent-first-child-start
	 (treesit-node-start (treesit-node-child parent 2))))
    (if (equal
	 (line-number-at-pos parent-start)
	 (line-number-at-pos parent-first-child-start))
	;; if array_creation_expression and the first
	;; array_element_initializer are on the same same line
	parent-first-child-start
      ;; else return parent-bol plus the offset
      (save-excursion
	(goto-char (treesit-node-start parent))
	(back-to-indentation)
	(+ (point) php-ts-mode-indent-offset)))))

(defun php-ts-mode--first-sibling-align (node parent bol &rest _)
  "Return the starting position of the first child of a sibling."
  (let ((first-sibling-start
	 (treesit-node-start (treesit-node-child parent 0)))
	(first-sibling-child-start
	 (treesit-node-start (treesit-node-child parent 1))))
    (if (equal
	 (line-number-at-pos first-sibling-start)
	 (line-number-at-pos first-sibling-child-start))
	;; if are on the same line return the child start
	first-sibling-child-start
      first-sibling-start)))

;; TODO: prova a vedere come viene usata c-ts-common-statement-offset
;; in java-ts-mode

(defun php-ts-mode--indent-styles ()
  "Indent rules supported by `php-ts-mode'."
  (let ((common
	 `((php-ts-mode--else-heuristic prev-line php-ts-mode-indent-offset)

	   ((query "(ERROR (ERROR)) @indent") column-0 0)

	   ((node-is ")") parent-bol 0)
	   ((node-is "]") parent-bol 0)
	   ((node-is "else_clause") parent-bol 0)
	   ((node-is "case_statement") parent-bol php-ts-mode-indent-offset)
	   ((node-is "default_statement") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "default_statement") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "expression_statement") parent-bol php-ts-mode-indent-offset)
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
	   ((query "(class_interface_clause (name) @indent)") php-ts-mode--parent-eol 1)
	   ((query "(class_interface_clause (qualified_name) @indent)") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "class_declaration") parent-bol 0)
	   ((parent-is "namespace_use_group") parent-bol php-ts-mode-indent-offset)
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
	   ((or (and (parent-is "compound_statement")
		     ;; If the previous sibling(s) are not on their
		     ;; own line, indent as if this node is the first
		     ;; sibling (Bug#67357)
		     php-ts-mode--first-sibling)
		(match null "compound_statement"))
	    standalone-parent php-ts-mode-indent-offset)
	   ((parent-is "compound_statement") parent-bol php-ts-mode-indent-offset)
	   ;; Opening bracket.
	   ((node-is "compound_statement") standalone-parent php-ts-mode-indent-offset)
	   ;; Opening bracket without closing bracket

	   ((parent-is "match_block") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "switch_block") parent-bol 0)
	   
	   ;; These rules are for cases where the body is bracketless.
	   ((query "(do_statement \"while\" @indent)") parent-bol 0)
	   ((or (parent-is "switch_statement")
		(parent-is "case_statement")
		(parent-is "if_statement")
		(parent-is "else_clause")
		(parent-is "for_statement")
		(parent-is "while_statement")
		(parent-is "do_statement"))
	    parent-bol php-ts-mode-indent-offset))))
    `((psr2
       ((parent-is "program") parent-bol 0)
       ((parent-is "text_interpolation") column-0 0)
       ((parent-is "function_call_expression") parent-bol php-ts-mode-indent-offset)
       ,@common)
      (pear
       ((parent-is "program") php-ts-mode--parent-html-heuristic 0)
       ((parent-is "text_interpolation") php-ts-mode--parent-html-heuristic 0)
       ((or (node-is "case_statement")
	    (node-is "default_statement"))
	parent-bol 0)
       ((parent-is "binary_expression") parent-bol php-ts-mode-indent-offset)
       ,@common)
      (drupal
       ((parent-is "program") php-ts-mode--parent-html-heuristic 0)
       ((parent-is "text_interpolation") php-ts-mode--parent-html-bol 0)
       ((parent-is "if_statement") parent-bol 0)
       ((parent-is "binary_expression") parent-bol php-ts-mode-indent-offset)
       ((parent-is "function_call_expression") parent-bol php-ts-mode-indent-offset)
       ,@common)
      (symfony
       ((parent-is "function_call_expression") parent-bol php-ts-mode-indent-offset)
       ,@common)
      (wordpress
       ((parent-is "program") php-ts-mode--parent-html-bol 0)
       ((parent-is "text_interpolation") php-ts-mode--parent-html-bol 0)
       ,@common)
      (zend
       ((parent-is "class_interface_clause") php-ts-mode--first-sibling-align 0)
       ((parent-is "function_call_expression") first-sibling 0)
       ((parent-is "array_creation_expression") php-ts-mode--array-element-heuristic 0)
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
   '((comment) @font-lock-comment-face)

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
   '((heredoc identifier: (heredoc_start) @font-lock-constant-face)
     (heredoc end_tag: (heredoc_end) @font-lock-constant-face)
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
   '((ERROR) @php-ts-mode--fontify-error)
   ))

(defvar php-ts-mode--phpdoc-indent-rules
  '((phpdoc
     ((and (parent-is "document") c-ts-common-looking-at-star)
      c-ts-common-comment-start-after-first-star -1)
     (c-ts-common-comment-2nd-line-matcher
      c-ts-common-comment-2nd-line-anchor
      1)))
  "Tree-sitter indentation rules for for `phpdoc'.")


;;; Font-lock helpers

(defconst php-ts-mode--custom-html-font-lock-settings
  (treesit-font-lock-rules
   :language 'html
   :override t
   :feature 'comment
   `((comment) @font-lock-comment-face
     ;; handle shebang path and others type of comment
     (document (text) @font-lock-comment-face))

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
  "Tree-sitter font-lock settings for `php-html-ts-mode'.")

(defvar php-ts-mode--phpdoc-font-lock-settings
  (treesit-font-lock-rules
   :language 'phpdoc
   :feature 'document
   :override t
   '((document) @font-lock-doc-face)

   :language 'phpdoc
   :feature 'type
   :override t
   '((union_type
      [(array_type) (primitive_type) (named_type) (optional_type)] @font-lock-type-face)
     ([(array_type) (primitive_type) (named_type) (optional_type)] @font-lock-type-face)
     (fqsen (name) @font-lock-function-name-face))

   :language 'phpdoc
   :feature 'attribute
   :override t
   `((tag_name) @font-lock-constant-face
     (tag
      [(version) (email_address)] @font-lock-doc-markup-face)
     (tag (author_name) @font-lock-property-name-face))

   :language 'phpdoc
   :feature 'variable
   :override t
   '((variable_name (name) @font-lock-variable-name-face)))
  "Tree-sitter font-lock settings for phpdoc.")

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
	 (node-query (format "(%s (%s))"
			     (treesit-node-type parent)
			     (treesit-node-type node))))
    (cond
     ((string-equal "(script_element (raw_text))" node-query) 'javascript)
     ((string-equal "(style_element (raw_text))" node-query) 'css)
     (t 'html))))

(defun php-ts-mode--language-at-point (point)
  "Return the language at POINT."
  (let* ((node (treesit-node-at point 'php))
	 (node-type (treesit-node-type node))
	 (parent (treesit-node-parent node))
	 (node-query (format "(%s (%s))" (treesit-node-type parent) node-type)))
    (save-excursion
      (goto-char (treesit-node-start node))
      (cond ((and (string= "comment" node-type) (looking-at-p "/\\*\\*")) 'phpdoc)
	    ((not (member node-query '("(program (text))"
				       "(text_interpolation (text))")))
	     'php)
	    (t (php-ts-mode--html-language-at-point point))))))

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
       (php-ts-mode--defun-object-name node (treesit-node-text node t)))
      (const_element
       (php-ts-mode--defun-object-name
	node
	(treesit-node-text (treesit-node-child node 0) t))))))


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

(defun php-ts-mode--defun-valid-p (node)
  "Return non-nil if NODE is a valid defun node.
Ie, NODE is not nested."
  (not (and (member (treesit-node-type node)
		    '("variable_name"
		      "const_element"
		      "enum_declaration"
		      "union_declaration"
		      "declaration"))
	    ;; If NODE's type is one of the above, make sure it is
	    ;; top-level.
	    (treesit-node-top-level
	     node (rx (or "variable_name"
			  "const_element"
			  "function_definition"
			  "enum_declaration"
			  "union_declaration"
			  "declaration"))))))


;;; Filling

(defun php-ts-mode--comment-indent-new-line (&optional soft)
  "Break line at point and indent, continuing comment if within one.
This is like `c-ts-common-comment-indent-new-line', but handle the
less common PHP-style # comment.  SOFT works the same as in
`comment-indent-new-line'."
  (if (save-excursion
	;; Line start with # or ## or ###...
	(beginning-of-line)
	(looking-at (rx "#" (* "#") (* " "))))
      (let ((prefix (match-string 0)))
	(if soft (insert-and-inherit ?\n) (newline 1))
	(delete-region (line-beginning-position) (point))
	(insert prefix))
    ;; other style of comments
    (c-ts-common-comment-indent-new-line soft)))

(defun php-ts-mode-comment-setup ()
  "Set up local variables for PHP comment.
Derived from `c-ts-common-comment-setup'."
  (c-ts-common-comment-setup)
  (setq-local c-ts-common--comment-regexp "comment"
	      comment-line-break-function #'php-ts-mode--comment-indent-new-line
	      comment-style 'extra-line
	      comment-start-skip (rx (or (seq "#" (not (any "[")))
					 (seq "/" (+ "/"))
					 (seq "/" (+ "*")))
				     (* (syntax whitespace)))))


;;; Modes

(defun php-ts-mode-set-comment-style ()
  "Set a different comment style."
  (interactive)
  (setq-local comment-start
	      (completing-read
	       "Choose comment style: "
	       '("/**" "//" "/*" "#") nil t nil nil "// "))
  (cond
   ((equal comment-start "/*") (setq-local comment-end "*/"))
   ((equal comment-start "//") (setq-local comment-end ""))
   ((equal comment-start "#") (setq-local comment-end ""))
   ((equal comment-start "/**") (setq-local comment-end "*/")))
  (setq mode-name (concat "PHP" (string-trim-right comment-start)))
  (force-mode-line-update))

(defvar-keymap php-ts-mode-map
  :doc "Keymap for `php-ts-mode' buffers."
  :parent prog-mode-map
  "C-c C-q" #'php-ts-mode--indent-defun
  "C-c ."   #'php-ts-mode-set-style
  "C-c C-k" #'php-ts-mode-set-comment-style
  "C-c C-n" #'run-php
  "C-c C-c" #'php-ts-mode-send-buffer
  "C-c C-l" #'php-ts-mode-send-file
  "C-c C-r" #'php-ts-mode-send-region)

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

(defvar php-ts-mode--feature-list
  '((comment definition spell
     ;; CSS
     query selector
     ;; HTML
     text
     ;; PHPDOC
     document
     phpdoc-error)
    (keyword string type name)
    (attribute assignment constant escape-sequence function-scope
     base-clause literal variable-name variable
     ;; Javascript
     jsx number pattern string-interpolation)
    (argument bracket delimiter error function-call operator property
     ;; Javascript
     function)))

;;;###autoload
(define-derived-mode php-ts-mode prog-mode "PHP"
  "Major mode for editing PHP, powered by tree-sitter.

\\{php-ts-mode-map}"
  :syntax-table php-ts-mode--syntax-table

  (unless (and
	   (treesit-ready-p 'php)
	   (treesit-ready-p 'phpdoc)
	   (treesit-ready-p 'html)
	   (treesit-ready-p 'javascript)
	   (treesit-ready-p 'css))
    (error "Tree-sitter for PHP isn't
    available.  You can install the parsers with M-x
    php-ts-mode-install-parsers"))

  ;; phpdoc is a local parser, don't create a parser fot it
  (treesit-parser-create 'html)
  (treesit-parser-create 'css)
  (treesit-parser-create 'javascript)

  ;; define the injected parser ranges
  (setq-local treesit-range-settings
	      (treesit-range-rules
	       :embed 'phpdoc
	       :host 'php
	       :local t
	       '(((comment) @cap
		  (:match "/\\*\\*" @cap)))

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

  (setq-local treesit-language-at-point-function #'php-ts-mode--language-at-point)

  ;; Only for debug
  ;;(setq-local treesit--font-lock-verbose t)
  ;;(setq-local treesit--indent-verbose t)

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
		 (sexp (not ,(rx (or "{" "}" "[" "]" "(" ")" ","))))
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
  (setq-local treesit-simple-indent-rules
	      (append treesit-simple-indent-rules
		      php-ts-mode--phpdoc-indent-rules
		      html-ts-mode--indent-rules
		      ;; Extended rules for js and css, to
		      ;; indent appropriately when injected
		      ;; into html
		      `((javascript ((parent-is "program")
				     php-ts-mode--js-css-tag-bol
				     php-ts-mode-js-css-indent-offset)
				    ,@(cdr (car js--treesit-indent-rules))))
		      `((css ((parent-is "stylesheet")
			      php-ts-mode--js-css-tag-bol
			      php-ts-mode-js-css-indent-offset)
			     ,@(cdr (car css--treesit-indent-rules))))))

  ;; Comment
  (php-ts-mode-comment-setup)

  ;; PHP vars are case-sensitive
  (setq-local case-fold-search t)

  ;; Electric
  (setq-local electric-indent-chars
	      (append "{}():;," electric-indent-chars))

  ;; Imenu/Which-function/Outline
  (setq-local treesit-simple-imenu-settings
	      '(("Class" "\\`class_declaration\\'" nil nil)
		("Enum" "\\`enum_declaration\\'" nil nil)
		("Function" "\\`function_definition\\'" nil nil)
		("Interface" "\\`interface_declaration\\'" nil nil)
		("Method" "\\`method_declaration\\'" nil nil)
		("Namespace" "\\`namespace_definition\\'" nil nil)
		("Trait" "\\`trait_declaration\\'" nil nil)
		("Variable" "\\`variable_name\\'" nil nil)
		("Constant" "\\`const_element\\'" nil nil)))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings (php-ts-mode--font-lock-settings))
  (treesit-add-font-lock-rules php-ts-mode--custom-html-font-lock-settings)
  (treesit-add-font-lock-rules js--treesit-font-lock-settings)
  (treesit-add-font-lock-rules css--treesit-settings)
  (treesit-add-font-lock-rules php-ts-mode--phpdoc-font-lock-settings)

  (setq-local treesit-font-lock-feature-list php-ts-mode--feature-list)

  ;; Align.
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
`DOCUMENT-ROOT': Path to Document root, default `php-ts-mode-ws-document-root'.
If a default value is null, the value is prompted.
`ROUTER': Path of the router PHP script,
see `https://www.php.net/manual/en/features.commandline.webserver.php'
`NUM-OF-WORKERS': Before run the web server set the
PHP_CLI_SERVER_WORKERS env variable in order to test code that
requires multiple concurrent requests to the built-in webserver.
When called with \\[universal-argument] it requires `PORT', `HOSTNAME' and `DOCUMENT-ROOT'."
  (interactive (when current-prefix-arg (php-ts-mode--webserver-read-args)))
  (let* ((port (or
		port
		php-ts-mode-ws-port
		(php-ts-mode--webserver-read-args 'port)))
	 (hostname (or
		    hostname
		    php-ts-mode-ws-hostname
		    (php-ts-mode--webserver-read-args 'hostname)))
	 (document-root (or
			 document-root
			 php-ts-mode-ws-document-root
			 (php-ts-mode--webserver-read-args 'document-root)))
	 (router (or
		  router
		  php-ts-mode-ws-router
		  (php-ts-mode--webserver-read-args 'router-script)))
	 (host (format "%s:%d" hostname port))
	 (name (format "PHP web server on: %s" host))
	 (buf-name (format "*%s*" name))
	 (args (delq
		nil
		(list "-S" host
		      "-t" document-root
		      router))))
    (cond (num-of-workers (setenv "PHP_CLI_SERVER_WORKERS" num-of-workers))
	  (php-ts-mode-ws-workers (setenv "PHP_CLI_SERVER_WORKERS" php-ts-mode-ws-workers)))
    (if (get-buffer buf-name)
	(message "Switch to already running web server")
      (message "Run PHP built-in web server with args %s" (string-join args " ")))
    (apply #'make-comint name php-ts-mode-php-executable nil args)
    (funcall
     (if (called-interactively-p 'interactive) #'display-buffer #'get-buffer)
     buf-name)))

(defun php-ts-mode--webserver-read-args (&optional type)
  "Helper for php-ts-mode-run-php-webserver.
The optional TYPE can be 'port, 'hostname, 'document-root or 'router-script,
otherwise it requires all of them."
  (let ((ask-port (lambda ()
		    (read-number "Port: " 3000)))
	(ask-hostname (lambda ()
			(read-string "Hostname: " "localhost")))
	(ask-document-root (lambda ()
			     (read-string "Document root: "
					  (file-name-directory (buffer-file-name)))))
	(ask-router-script (lambda ()
			     (read-string "Router script: "
					  (file-name-directory (buffer-file-name))))))
    (cl-case type
      (port (funcall ask-port))
      (hostname (funcall ask-hostname))
      (document-root (funcall ask-document-root))
      (router-script (funcall ask-router-script))
      (t (list
	  (funcall ask-port)
	  (funcall ask-hostname)
	  (funcall ask-document-root)
	  (funcall ask-router-script))))))

(define-derived-mode inferior-php-ts-mode comint-mode "Inferior PHP"
  "Major mode for PHP inferior process."
  (setq-local scroll-conservatively 1
	      comint-input-ring-file-name php-ts-mode-inferior-history
	      comint-input-ignoredups t
	      comint-prompt-read-only t
	      comint-use-prompt-regexp t
	      comint-prompt-regexp (concat "^" php-ts-mode--inferior-prompt " "))
  (comint-read-input-ring t))


;;;###autoload
(defun run-php (cmd config)
  "Runs a PHP interpreter as a subprocess of Emacs, with PHP
I/O through an Emacs buffer.  Variables `php-ts-mode-php-executable'
and `php-ts-mode-php-config' control which PHP interpreter is run."
  (interactive (list
		(if current-prefix-arg
		    (read-string "Run PHP: " php-ts-mode-php-executable)
		  php-ts-mode-php-executable)
		(if current-prefix-arg
		    (read-string "With config: " php-ts-mode-php-config)
		  php-ts-mode-php-config)))
  (let ((buffer (get-buffer-create php-ts-mode-inferior-buffer)))
    (unless (comint-check-proc buffer)
      (with-current-buffer buffer
	(inferior-php-ts-mode-startup cmd config)
	(inferior-php-ts-mode)))
    (when buffer
      (pop-to-buffer buffer))))

(defvar php-ts-mode--inferior-php-process nil
  "The PHP inferior process associated to `php-ts-mode-inferior-buffer'.")

(defun inferior-php-ts-mode-startup (cmd config)
  "Start an inferior PHP process."
  (setq-local php-ts-mode--inferior-php-process
	      (apply #'make-comint-in-buffer
		     (string-replace "*" "" php-ts-mode-inferior-buffer)
		     php-ts-mode-inferior-buffer
		     cmd
		     nil
		     (delq
		      nil
		      (list
		       (when config
			 (format "-c %s" config))
		       "-a"))))
  (add-hook 'comint-preoutput-filter-functions
	    (lambda (string)
	      (let ((prompt (concat php-ts-mode--inferior-prompt " ")))
		(if (member
		     string
		     (list prompt "php { " "php ( " "/* > " "Interactive shell\n\n"))
		    string
		  (let (;; Filter out prompts characters that accumulate when sending
			;; regions to the inferior process.
			(clean-string
			 (replace-regexp-in-string
			  (rx-to-string `(or
					  (+ "php >" (opt space))
					  (+ "php {" (opt space))
					  (+ "php (" (opt space))
					  (+ "/*" (1+ space) (1+ ">") (opt space))))
			  "" string)))
		    ;; Re-add the prompt for the next line, if isn't empty.
		    (if (string= clean-string "")
			""
		      (concat clean-string prompt))))))
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
  (when (buffer-live-p php-ts-mode--inferior-php-process)
    (php-ts-mode-show-process-buffer)
    (comint-send-string php-ts-mode--inferior-php-process "\n")
    (comint-send-string
     php-ts-mode--inferior-php-process
     (buffer-substring-no-properties beg end))
    (comint-send-string php-ts-mode--inferior-php-process "\n")))

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

(derived-mode-add-parents 'php-ts-mode '(php-mode))

(when (treesit-ready-p 'php)
  (add-to-list
   'auto-mode-alist '("\\.\\(?:php[s345]?\\|phtml\\)\\'" . php-ts-mode))
  (add-to-list
   'auto-mode-alist '("\\.\\(?:php\\|inc\\|stub\\)\\'" . php-ts-mode))
  (add-to-list
   'auto-mode-alist '("/\\.php_cs\\(?:\\.dist\\)?\\'" . php-ts-mode))
  (add-to-list
   'interpreter-mode-alist
   (cons "php\\(?:-?[34578]\\(?:\\.[0-9]+\\)*\\)?" 'php-ts-mode)))

(provide 'php-ts-mode)
;;; php-ts-mode.el ends here
