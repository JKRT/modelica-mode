;;; modelica-mode.el --- major mode for editing Modelica files -*- lexical-binding: t; -*-
;; Copyright (C) 2019       John Tinnerholm
;; Copyright (C) 2019       Free Software Foundation, Inc
;; Copyright (C  2019       Espen Skoglund <esk@gnu.org>
;; Copyright (C) 2010       Dietmar Winkler
;; Copyright (C) 1997--2001 Ruediger Franke
;; Copyright (C) 1997--2001 Free Software Foundation, Inc.

;; Keywords: languages, tools, modelica
;; Author:   John Tinnerholm <jtinnerholm@gmail.com>

;; This code has been written for use with Emacs and shares its licensing.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Based in Modelica mode previously provided by Dietmar Winkler and Ruediger Franke
;; For indention reuses many procedures for Pascal-mode provided by Espen Skoglund
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun modelica--package-directory ()
  "Return the directory containing modelica-mode sources."
  (or (and load-file-name (file-name-directory load-file-name))
      (and buffer-file-name (file-name-directory buffer-file-name))
      default-directory))

(defun modelica--ensure-parser-loaded ()
  "Load the parser integration for modelica-mode."
  (or (featurep 'modelica-mode_)
      (let ((dir (modelica--package-directory)))
        (when (and dir (file-directory-p dir))
          (add-to-list 'load-path dir))
        (require 'modelica-mode_ nil t))))

(condition-case nil
    (modelica--ensure-parser-loaded)
  (error nil))

(declare-function modelica-mode-clear-parse-cache "modelica-mode_" (&optional buffer))
(declare-function modelica-mode-parse-buffer "modelica-mode_" (&optional buffer))

(defconst modelica-mode-version "2.0.0")

(defgroup modelica-mode nil
  "Customization group for Modelica major mode."
  :group 'languages
  :prefix "modelica-")

;;; Native compilation support (.eln)
(defconst modelica--native-compilation-sources
  '("modelica-mode.el"
    "modelica-mode_.el"
    "absyn-parser.el"
    "absyn-ast.el")
  "Modelica-mode source files compiled for native `.eln' generation.")

(defvar modelica--native-compilation-triggered nil
  "Internal flag used to avoid repeated auto native compilation.")

(defconst modelica--native-compilation-cache-dir
  (file-name-as-directory (expand-file-name "eln-cache" user-emacs-directory))
  "Directory used for modelica-mode native `.eln' output.")

(defun modelica--native-compilation-cache-dir-ready-p ()
  "Ensure and return non-nil when the `.eln' cache directory is writable."
  (when (and (boundp 'native-comp-eln-load-path)
             (stringp modelica--native-compilation-cache-dir))
    (ignore-errors
      (make-directory modelica--native-compilation-cache-dir t)
      (unless (member modelica--native-compilation-cache-dir
                      native-comp-eln-load-path)
        (setq native-comp-eln-load-path
              (append (list modelica--native-compilation-cache-dir)
                      native-comp-eln-load-path)))
      (and (file-directory-p modelica--native-compilation-cache-dir)
           (file-writable-p modelica--native-compilation-cache-dir)))))

(defcustom modelica-enable-native-compilation t
  "Enable native compilation for modelica-mode sources using `.eln'.
When non-nil and native compilation is available, the package files are
compiled asynchronously on first entry into modelica-mode."
  :type 'boolean
  :safe #'booleanp
  :group 'modelica-mode)

(defcustom modelica-native-compile-on-save nil
  "Recompile `modelica-mode' native files after saving package sources.
This has no effect when `modelica-enable-native-compilation' is nil."
  :type 'boolean
  :safe #'booleanp
  :group 'modelica-mode)

(defun modelica-native-compilation-available-p ()
  "Return non-nil when Emacs native compilation is available for this session."
  (and (fboundp 'native-compile)
       (fboundp 'native-compile-async)
       (modelica--native-compilation-cache-dir-ready-p)
       (let ((ok nil))
         (dolist (dir native-comp-eln-load-path ok)
           (when (and dir
                      (file-directory-p dir)
                      (file-writable-p dir))
             (setq ok t))))))

(defun modelica--native-package-dir ()
  (or (and load-file-name (file-name-directory load-file-name))
      default-directory))

(defun modelica--native-compilation-files ()
  "Return absolute `.el' paths for modelica-mode parser+mode sources."
  (let ((dir (modelica--native-package-dir)))
    (delq nil
          (mapcar (lambda (f)
                    (let ((path (expand-file-name f dir)))
                      (and (file-readable-p path) path)))
                  modelica--native-compilation-sources))))

(defun modelica-native-compile (&optional async)
  "Compile modelica-mode source files to `.eln'.

When ASYNC is nil (default), compiles asynchronously with
`native-compile-async'. When ASYNC is non-nil, runs synchronous
`native-compile'."
  (interactive "P")
  (unless (modelica-native-compilation-available-p)
    (user-error "Native compilation unavailable"))
  (let ((files (modelica--native-compilation-files))
        (fn (if async #'native-compile #'native-compile-async)))
    (if (null files)
        (user-error "No modelica-mode source files found for native compilation")
      (dolist (file files) (funcall fn file)))))

(defun modelica--native-compilation-maybe ()
  (when (and modelica-enable-native-compilation
             (not modelica--native-compilation-triggered)
             (modelica-native-compilation-available-p))
    (setq modelica--native-compilation-triggered t)
    (ignore-errors (modelica-native-compile))))

(defun modelica--native-compilation-after-save ()
  (when (and modelica-enable-native-compilation
             modelica-native-compile-on-save
             buffer-file-name
             (not (file-remote-p buffer-file-name)))
    (let ((file (file-truename buffer-file-name))
          (files (mapcar #'file-truename (modelica--native-compilation-files))))
      (when (member file files)
        (modelica-native-compile)))))

;;; History
;;    see ChangeLog
;;; constants
(defcustom modelica-indent-level 2
  "Indentation depth for block bodies."
  :type 'integer
  :safe #'integerp
  :group 'modelica-mode)

(defcustom modelica-case-indent 2
  "Indentation depth for case statement bodies."
  :type 'integer
  :safe #'integerp
  :group 'modelica-mode)

(defcustom modelica-auto-newline nil
  "Whether to insert a newline after semicolons in specific contexts."
  :type 'boolean
  :safe #'booleanp
  :group 'modelica-mode)

(defcustom modelica-tab-always-indent t
  "Whether `TAB' should always reindent the current line."
  :type 'boolean
  :safe #'booleanp
  :group 'modelica-mode)

(defcustom modelica-auto-endcomments t
  "Whether to insert block end comments for certain block endings."
  :type 'boolean
  :safe #'booleanp
  :group 'modelica-mode)

(defcustom modelica-auto-lineup '(all)
  "Contexts where alignment of declarations is done automatically."
  :type '(repeat symbol)
  :safe #'listp
  :group 'modelica-mode)

(defcustom modelica-indent-nested-classes t
  "Whether nested classes are additionally indented."
  :type 'boolean
  :safe #'booleanp
  :group 'modelica-mode)

(defcustom modelica-type-keywords
  '("array" "String" "Boolean" "Integer" "Real" "string" "record" "uniontype")
  "Modelica type keywords used by completion and editing helpers."
  :type '(repeat string)
  :safe #'listp
  :group 'modelica-mode)

(defcustom modelica-start-keywords
  '("algorithm" "equation" "function" "end" "connector" "block" "while" "for"
    "initial equation" "initial algorithm" "local" "match" "matchcontinue" "reset"
    "print")
  "Modelica keywords typically used to start statements."
  :type '(repeat string)
  :safe #'listp
  :group 'modelica-mode)

(defcustom modelica-separator-keywords
  '("else" "then")
  "Modelica separator keywords used by completion."
  :type '(repeat string)
  :safe #'listp
  :group 'modelica-mode)


(defconst modelica-class-modifier-keyword
  "\\(encapsulated\\|final\\|inner\\|outer\\|partial\\|re\\(declare\\|placeable\\)\\)[ \t\n\r]+"
  "*Keyword regexp optionally found before a class keyword.")

(defconst modelica-class-keyword
  "\\(block\\|c\\(lass\\|onnector\\)\\|function\\|model\\|package\\|record\\|uniontype\\|type\\)[ \t\n\r]+"
  "*Keyword regexp preceding a Modelica class declaration or definition.")


(defvar modelica-keywords
  '("algorithm" "and" "annotation" "block" "break" "class" "connect" "connector" "constant" "constrainedby"
    "der" "discrete" "each" "else" "elseif" "elsewhen" "encapsulated" "end" "enumeration" "equation"
    "expandable" "extends" "external" "false" "final" "flow" "for" "function" "if" "import" "impure" "in" "initial"
    "inner" "input" "loop" "model" "not" "operator" "or" "outer" "output" "package" "parameter" "partial"
    "protected" "public" "pure" "record" "redeclare" "replaceable" "return" "stream" "then" "true" "type" "when"
    "while" "within"
    ;;The following keywords are for the MetaModelica extension
    "match" "matchcontinue" "list" "array" "as" "case" "then" "local" "record" "uniontype"))

;;;
;;; Regular expressions used to calculate indent, etc.
;;;
;;For completion help
(defconst modelica-symbol-re      "\\<[a-zA-Z_][a-zA-Z_0-9.]*\\>")
;Things that starts blocks
(defconst modelica-beg-block-re   "\\<\\(algorithm\\|case\\|connector\\|equation\\|function\\|class\\|local\\|match\\|matchcontinue\\|model\\|record\\|uniontype\\|block\\)\\>")
(defconst modelica-end-block-re   "\\<\\(end\\)\\>")
;;Should not be indented
(defconst modelica-declaration-re "\\<\\(constant\\|connector\\|class\\|block\\)\\>")
;;Packages are special
(defconst modelica-progbeg-re     "\\<package\\>")
;;Treat functions in their own way :)
(defconst modelica-defun-re       "\\<\\(function\\|class\\|package\\|uniontype\\|record\\|block\\|connector\\)\\>")
;;Things to treat as subblocks
(defconst modelica-sub-block-re   "\\<\\(if\\|else\\|for\\|while\\|when\\|case\\)\\>")
;;Things that shall have no extra indention. algorithm,  equation initial
(defconst modelica-noindent-re    "\\<\\(algorithm\\|end\\|public\\|protected\\|else if\\|else\\|equation\\|initial\\)\\>")
;;Things that start a new statement
(defconst modelica-nosemi-re      "\\<\\(begin\\|then\\|loop\\|else\\)\\>")
;For special functionality
(defconst modelica-autoindent-lines-re "\\<\\(import\\)\\>")

;;; Interface to font-lock
(defvar modelica-font-lock-keywords
  (list
   (list (concat "\\<"
                 (regexp-opt
                  '("import" "within" "extends"
                    "for" "while" "in" "loop" "when"
                    "if" "then" "else" "elseif" "elsewhen"
                    "and" "not" "or" "case"))"\\>")
         0 'font-lock-keyword-face)
   (list (concat "\\<"
                 (regexp-opt
                  '("algorithm" "equation" "public" "protected" "match" "matchcontinue" "local" "guard" "as") t)
                 "\\>")
         0 'font-lock-keyword-face)
   (list (concat "\\<"
                 (regexp-opt
                  '("redeclare" "final" "partial" "replaceable"
                    "inner" "outer" "encapsulated"
                    "discrete" "parameter" "constant"
                    "flow" "input" "output" "external"
                    "block" "class" "connector" "function" "model"
                    "package" "record" "type"
                    "end" "uniontype") t) "\\>")
         0 'font-lock-type-face)
   (list (concat "\\<"
                 "\\(a\\(bs\\|nalysisType\\)\\|c\\(ardinality\\|eil\\|"
                 "hange\\|ross\\)\\|d\\(e\\(lay\\|r\\)\\|i\\(agonal\\|"
                 "v\\)\\)\\|edge\\|f\\(ill\\|loor\\)\\|i\\(dentity\\|"
                 "n\\(itial\\|teger\\)\\)\\|linspace\\|m\\(a\\(trix\\|"
                 "x\\)\\|in\\|od\\)\\|n\\(dims\\|oEvent\\)\\|o\\(nes\\|"
                 "uterProduct\\)\\|pr\\(e\\|o\\(duct\\|mote\\)\\)\\|"
                 "re\\(init\\|m\\)\\|s\\(amle\\|calar\\|i\\(gn\\|"
                 "ze\\)\\|kew\\|qrt\\|um\\|ymmetric\\)\\|t\\(erminal\\|"
                 "ranspose\\)\\|vector\\|zeros\\)"
                 "\\>")
         0 'font-lock-function-name-face)
   (list (concat "\\<"
                 "\\(assert\\|terminate\\)"
                 "\\>")
         0 'font-lock-warning-face)
   (list (concat "\\<"
                 "\\(annotation\\|connect\\)"
                 "\\>")
         0 'font-lock-builtin-face)
   (list (concat "\\<"
                 "\\(false\\|true\\)"
                 "\\>")
         0)
   (list (concat "\\<"
                 "\\(time\\)"
                 "\\>")
         0 'font-lock-variable-name-face))
  "Keywords to highlight for Modelica. See variable `font-lock-keywords'.")

;;; The mode
  (defvar modelica-basic-offset 2
    "*basic offset for indentation in Modelica Mode")
  (defvar modelica-comment-offset 2
    "*offset for indentation in comments in Modelica Mode")
  (defvar modelica-statement-offset 2
    "*offset for indentation in statements in Modelica Mode")
  (defvar modelica-mode-syntax-table nil
    "Syntax table used while in Modelica mode.")
  (defvar modelica-mode-abbrev-table nil
    "Abbrev table used while in Modelica mode.")
  (define-abbrev-table 'modelica-mode-abbrev-table ())

  (declare-function modelica-last-unended-begin "modelica-mode" (&optional arg))
  (declare-function modelica-within-comment "modelica-mode" (&optional arg))
  (declare-function modelica-within-matrix-expression "modelica-mode" (&optional arg))
  (declare-function modelica-within-equation "modelica-mode" (&optional arg))
  (declare-function mdc-forward-begin "modelica-mode" ())

  (defun modelica-statement-start (&optional ref-point)
    "Move point to the first character of the current statement;
   optional argument points to the last end or unended begin"
    (let ((save-point (point)))
      (if ref-point
          ()
        (condition-case nil
            (modelica-last-unended-begin t)
          (error (goto-char (point-min))))
        (setq ref-point (point))
        (goto-char save-point))
      (while (progn
               (re-search-backward
                ;; ("]" ")" ";"
                ;;  "algorithm" "equation" "external"
                ;;  "else" "elseif" "elsewhen"
                ;;  "loop" "protected" "public" "then")
                (concat
                 "[\]\);]\\|"
                 "\\<"
                 "\\(algorithm\\|e\\(lse\\(if\\|when\\)?\\|quation\\|"
                 "xternal\\)\\|loop\\|p\\(rotected\\|ublic\\)\\|then\\)"
                 "\\>")
                ref-point 'no-error)
               (and
                (> (point) ref-point)
                (or (modelica-within-comment t)
                    (modelica-within-string)
                    (if (looking-at "[\]\)]")
                        (progn
                          (forward-char 1)
                          (forward-sexp -1)
                          t))
                    (if (looking-at ";")
                        (modelica-within-matrix-expression t)
                      (modelica-within-equation t))))))
      (cond
       ((= (point) ref-point)
        ;; we arrived at last unended begin,
        ;; but might be looking for first statement of block
        (mdc-forward-begin)
        (forward-comment (- (buffer-size)))
        (if (> (point) save-point)
            (goto-char ref-point)))
       ((looking-at ";")
        (forward-char 1))
       (t
        (forward-word 1)))
      (forward-comment (buffer-size))))

  (unless modelica-mode-syntax-table
    (setq modelica-mode-syntax-table (make-syntax-table))

    (modify-syntax-entry ?_ "w"       modelica-mode-syntax-table)
    (modify-syntax-entry ?. "w"       modelica-mode-syntax-table)
    (modify-syntax-entry ?/  ". 124b"  modelica-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"    modelica-mode-syntax-table)
    (modify-syntax-entry ?\n "> b"     modelica-mode-syntax-table))


;;;
;;;  Macros
;;;
  (defun modelica-declaration-end ()
    (let ((nest 1))
      (while (and (> nest 0)
                  (re-search-forward
                   "[:=]\\|\\(\\<record\\>\\)\\|\\(\\<end\\>\\)"
                   (line-end-position 2) t))
        (cond ((match-beginning 1) (setq nest (1+ nest)))
              ((match-beginning 2) (setq nest (1- nest)))
              ((looking-at "[^(\n]+)") (setq nest 0))))))


  (defun modelica-declaration-beg ()
    (let ((nest 1))
      (while (and (> nest 0)
                  (re-search-backward "[:=]\\|\\<\\(type\\|var\\|label\\|const\\)\\>\\|\\(\\<record\\>\\)\\|\\(\\<end\\>\\)" (line-beginning-position) t))
        (cond ((match-beginning 1) (setq nest 0))
              ((match-beginning 2) (setq nest (1- nest)))
              ((match-beginning 3) (setq nest (1+ nest)))))
      (= nest 0)))


(defun modelica-within-string ()
  (nth 3 (parse-partial-sexp (line-beginning-position) (point))))


;;;###autoload
  (define-derived-mode modelica-mode prog-mode "Modelica"
    "Major mode for editing Modelica code.\\<modelica-mode-map>
TAB indents for Modelica code.
Delete converts tabs to spaces as it moves back.
\\[completion-at-point] completes the word around current point with respect \
to position in code
\\[completion-help-at-point] shows all possible completions at this point.
Other useful functions are:
\\[modelica-mark-defun]\t- Mark function.
\\[modelica-star-comment]\t- insert (* ... *)
\\[modelica-comment-area]\t- Comment marked area.
\\[modelica-uncomment-area]\t- Uncomment an area commented with \
\\[modelica-comment-area].
\\[modelica-beg-of-defun]\t- Move to beginning of current function.
\\[modelica-end-of-defun]\t- Move to end of current function.
\\[modelica-goto-class]\t- Goto class prompted for in the minibuffer.
\\[modelica-outline-mode]\t- Enter `modelica-outline-mode'.
Variables controlling indentation/edit style:
 `modelica-indent-level' (default 2)
    Indentation of Modelica statements with respect to containing block.
 `modelica-case-indent' (default 2)
    Indentation for case statements.
 `modelica-auto-newline' (default nil)
    Non-nil means automatically newline after semicolons and the punctuation
    mark after an end.
 `modelica-indent-nested-classes' (default t)
    Non-nil means nested functions are indented.
 `modelica-tab-always-indent' (default t)
    Non-nil means TAB in Modelica mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 `modelica-auto-endcomments' (default t)
    Non-nil means a comment { ... } is set after the ends which ends cases and
    functions. The name of the function or case will be set between the braces.
 `modelica-auto-lineup' (default t)
    List of contexts where auto lineup of :'s or ='s should be done.
See also `modelica-type-keywords', `modelica-start-keywords',
and `modelica-separator-keywords'."

    `;;For comments
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-multi-line)
  (setq
   comment-column 32
   comment-start "// "
   comment-start-skip "//*"
   comment-end ""
   comment-multi-line nil)


  (setq-local local-abbrev-table modelica-mode-abbrev-table)
  (setq-local indent-line-function 'modelica-indent-line)
  (setq-local comment-indent-function 'modelica-indent-comment)
  (setq-local parse-sexp-ignore-comments nil)
  (setq-local blink-matching-paren-dont-ignore-comments t)
  (setq-local case-fold-search t)
  (setq-local line-move-ignore-invisible t)
  (modelica--native-compilation-maybe)
  (when modelica-native-compile-on-save
    (add-hook 'after-save-hook #'modelica--native-compilation-after-save nil t))
  (add-hook 'completion-at-point-functions 'modelica-completions-at-point nil t)
  ;; Font lock support
  (setq-local font-lock-defaults '(modelica-font-lock-keywords nil nil))
    (setq-local imenu-case-fold-search t)
    ;; Modelica-mode's own hide/show support.
    (add-to-invisibility-spec '(modelica . t)))

;;;
;;;  Electric functions
;;;
  (defun electric-modelica-terminate-line ()
    "Terminate line and indent next line."
    (interactive)
    ;; First, check if current line should be indented
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (if (looking-at modelica-autoindent-lines-re)
          (modelica-indent-line)))
    (delete-horizontal-space) ; Removes trailing whitespaces
    (newline)
    ;; Indent next line
    (modelica-indent-line)
    ;; Check if we shall indent inside comment
    (let ((setstar nil))
      (save-excursion
        (forward-line -1)
        (skip-chars-forward " \t")
        (cond ((looking-at "\\*[ \t]+)")
               ;; Delete region between `*' and `)' if there is only whitespaces.
               (forward-char 1)
               (delete-horizontal-space))
              ((and (looking-at "(\\*\\|\\*[^)]")
                    (not (save-excursion (search-forward "*)" (line-end-position) t))))
               (setq setstar t))))
      ;; If last line was a star comment line then this one shall be too.
      (if (null setstar)
          (modelica-indent-line)
        (insert "*  "))))


  (defun electric-modelica-semi-or-dot ()
    "Insert `;' or `.' character and reindent the line."
    (interactive)
    (insert last-command-event)
    (save-excursion
      (beginning-of-line)
      (modelica-indent-line))
    (if modelica-auto-newline
        (electric-modelica-terminate-line)))

  (defun electric-modelica-colon ()
    "Insert `:' and do all indentations except line indent on this line."
    (interactive)
    (insert last-command-event)
    ;; Do nothing if within string.
    (if (modelica-within-string)
        ()
      (save-excursion
        (beginning-of-line)
        (modelica-indent-line))
      (let ((modelica-tab-always-indent nil))
        (modelica-indent-command))))

  (defun electric-modelica-equal ()
    "Insert `=', and do indentation if within type declaration."
    (interactive)
    (insert last-command-event)
    (if (eq (car (modelica-calculate-indent)) 'declaration)
        (let ((modelica-tab-always-indent nil))
          (modelica-indent-command))))

  (defun electric-modelica-hash ()
    "Insert `#', and indent to column 0 if this is a CPP directive."
    (interactive)
    (insert last-command-event)
    (if (save-excursion (beginning-of-line) (looking-at "^[ \t]*#"))
        (save-excursion (beginning-of-line)
                        (delete-horizontal-space))))

  (defun electric-modelica-tab ()
    "Function called when TAB is pressed in Modelica mode."
    (interactive)
    ;; Do nothing if within a string or in a CPP directive.
    (if (or (modelica-within-string)
            (and (not (bolp))
                 (save-excursion (beginning-of-line) (eq (following-char) ?#))))
        (insert "\t")
      ;; If modelica-tab-always-indent, indent the beginning of the line.
      (if modelica-tab-always-indent
          (save-excursion
            (beginning-of-line)
            (modelica-indent-line))
        (if (save-excursion
              (skip-chars-backward " \t")
              (bolp))
            (modelica-indent-line)
          (insert "\t")))
      (modelica-indent-command)))
;;;
;;; Interactive functions
;;;
  (defvar modelica--extra-indent 0)

  (defun modelica-insert-block ()
    "Insert Modelica begin ... end; block in the code with right indentation."
    (interactive)
    (insert "begin")
    (electric-modelica-terminate-line)
    (save-excursion
      (newline)
      (insert "end")
      (beginning-of-line)
      (modelica-indent-line)))

  (defun modelica-star-comment ()
    "Insert Modelica star comment at point."
    (interactive)
    (modelica-indent-line)
    (insert "/*")
    (electric-modelica-terminate-line)
    (save-excursion
      (electric-modelica-terminate-line)
      (delete-horizontal-space)
      (insert "*/"))
    (insert "  "))

  (defun modelica-mark-defun ()
    "Mark the current Modelica function (or procedure).
This puts the mark at the end, and point at the beginning."
    (interactive)
    (push-mark)
    (modelica-end-of-defun)
    (push-mark)
    (modelica-beg-of-defun))

  (defun modelica-comment-area (start end)
    "Comment region from START to END."
    (interactive "r")
    (comment-region start end))

  (defun modelica-uncomment-area (start end)
    "Uncomment region from START to END."
    (interactive "r")
    (comment-region start end '(4)))

  (defun modelica-newline-and-indent ()
    "Insert a newline and reindent according to Modelica mode."
    (interactive)
    (electric-modelica-terminate-line))

  (defun modelica-insert-end ()
    "Insert `end` at point."
    (interactive)
    (insert "end")
    (modelica-indent-line))

  (defun modelica-indent-for-comment ()
    "Indent the current comment using `comment-indent'."
    (interactive)
    (comment-indent))

  (defun modelica-indent-new-comment-line ()
    "Insert a correctly indented comment continuation line."
    (interactive)
    (comment-indent-new-line))

  (defun modelica-forward-statement ()
    "Move forward to the end of the current statement."
    (interactive)
    (modelica-end-of-statement))

  (defun modelica-backward-statement ()
    "Move backward to the beginning of the current statement."
    (interactive)
    (modelica-statement-start))

  (defun modelica-forward-block ()
    "Move forward to the end of the current block."
    (interactive)
    (modelica-end-of-defun))

  (defun modelica-backward-block ()
    "Move backward to the start of the current block."
    (interactive)
    (modelica-beg-of-defun))

  (defun modelica-to-block-begin ()
    "Move to block beginning."
    (interactive)
    (modelica-backward-block))

  (defun modelica-to-block-end ()
    "Move to block end."
    (interactive)
    (modelica-forward-block))

  (defun modelica-beg-of-defun ()
    "Move backward to the beginning of the current function or procedure."
    (interactive)
    (catch 'found
      (if (not (looking-at (concat "\\s \\|\\s)\\|" modelica-defun-re)))
          (forward-sexp 1))
      (let ((nest 0) (max -1) (func 0)
            (reg (concat modelica-beg-block-re "\\|"
                         modelica-end-block-re "\\|"
                         modelica-defun-re)))
        (while (re-search-backward reg nil 'move)
          (cond ((let ((state (save-excursion
                                (parse-partial-sexp (point-min) (point)))))
                   (or (nth 3 state) (nth 4 state))) ; Inside string or comment
                 ())
                ((match-end 1)                       ; begin|case|record|repeat
                 (if (and (looking-at "\\<record\\>") (>= max 0))
                     (setq func (1- func)))
                 (setq nest (1+ nest)
                       max (max nest max)))
                ((match-end 2)                       ; end|until
                 (if (and (= nest max) (>= max 0))
                     (setq func (1+ func)))
                 (setq nest (1- nest)))
                ((match-end 3)                       ; function|procedure
                 (if (= 0 func)
                     (throw 'found t)
                   (setq func (1- func)))))))
      nil))

  (defun modelica-end-of-defun ()
    "Move forward to the end of the current function or procedure."
    (interactive)
    (if (looking-at "\\s ")
        (forward-sexp 1))
    (if (not (looking-at modelica-defun-re))
        (modelica-beg-of-defun))
    (forward-char 1)
    (let ((nest 0) (func 1)
          (reg (concat modelica-beg-block-re "\\|"
                       modelica-end-block-re "\\|"
                       modelica-defun-re)))
      (while (and (/= func 0)
                  (re-search-forward reg nil 'move))
        (cond ((let ((state (save-excursion
                              (parse-partial-sexp (point-min) (point)))))
                 (or (nth 3 state) (nth 4 state))) ; Inside string or comment
               ())
              ((match-end 1)
               (setq nest (1+ nest))
               (if (save-excursion
                     (goto-char (match-beginning 0))
                     (looking-at "\\<record\\>"))
                   (setq func (1+ func))))
              ((match-end 2)
               (setq nest (1- nest))
               (if (= nest 0)
                   (setq func (1- func))))
              ((match-end 3)
               (setq func (1+ func))))))
    (forward-line 1))

  (defun modelica-end-of-statement ()
    "Move forward to end of current statement."
    (interactive)
    (let ((parse-sexp-ignore-comments t)
          (nest 0) pos
          (regexp (concat "\\(" modelica-beg-block-re "\\)\\|\\("
                          modelica-end-block-re "\\)")))
      (if (not (looking-at "[ \t\n]")) (forward-sexp -1))
      (or (looking-at modelica-beg-block-re)
          ;; Skip to end of statement
          (setq pos (catch 'found
                      (while t
                        (forward-sexp 1)
                        (cond ((looking-at "[ \t]*;")
                               (skip-chars-forward "^;")
                               (forward-char 1)
                               (throw 'found (point)))
                              ((save-excursion
                                 (forward-sexp -1)
                                 (looking-at modelica-beg-block-re))
                               (goto-char (match-beginning 0))
                               (throw 'found nil))
                              ((eobp)
                               (throw 'found (point))))))))
      (if (not pos)
          ;; Skip a whole block
          (catch 'found
            (while t
              (re-search-forward regexp nil 'move)
              (setq nest (if (match-end 1)
                             (1+ nest)
                           (1- nest)))
              (cond ((eobp)
                     (throw 'found (point)))
                    ((= 0 nest)
                     (throw 'found (modelica-end-of-statement))))))
        pos)))

  (defun modelica-downcase-keywords ()
    "Downcase all Modelica keywords in the buffer."
    (interactive)
    (modelica-change-keywords 'downcase-word))

  (defun modelica-upcase-keywords ()
    "Upcase all Modelica keywords in the buffer."
    (interactive)
    (modelica-change-keywords 'upcase-word))

  (defun modelica-capitalize-keywords ()
    "Capitalize all Modelica keywords in the buffer."
    (interactive)
    (modelica-change-keywords 'capitalize-word))

  ;; Change the keywords according to argument.
  (defun modelica-change-keywords (change-word)
    (save-excursion
      (let ((keyword-re (concat "\\<\\("
                                (mapconcat 'identity modelica-keywords "\\|")
                                "\\)\\>")))
        (goto-char (point-min))
          (while (re-search-forward keyword-re nil t)
            (funcall change-word -1)))))

;;;
;;; Indentation
;;;
(defconst modelica-indent-alist
  '((block . (+ modelica--extra-indent modelica-indent-level))
    (case . (+ modelica--extra-indent modelica-case-indent))
    (caseblock . modelica--extra-indent) (cpp . 0)
    (declaration . (+ modelica--extra-indent modelica-indent-level))
    (paramlist . (modelica-indent-paramlist t))
    (comment . (modelica-indent-comment))
    (defun . modelica--extra-indent) (contexp . modelica--extra-indent)
    (unknown . modelica--extra-indent) (string . 0) (progbeg . 0)))

(defun modelica--indent-line-text ()
  "Return current line text without leading or trailing whitespace."
  (string-trim
   (buffer-substring-no-properties (line-beginning-position)
                                   (line-end-position))))

(defun modelica--indent-line-kind (line)
  "Return the syntactic indentation kind for LINE."
  (cond
   ((string-match-p "\\`\\(model\\|class\\|package\\|function\\|block\\|connector\\|record\\|uniontype\\)\\b" line)
    'class)
   ((string-match-p "\\`\\(equation\\|algorithm\\|initial[ \t]+equation\\|initial[ \t]+algorithm\\)\\b" line)
    'section)
   ((string-match-p "\\`\\(if\\|for\\|while\\|when\\)\\b.*\\b\\(then\\|loop\\)\\b" line)
    'control)
   ((string-match-p "\\`end\\b" line)
    'end)
   ((string-match-p "\\bmatch\\(continue\\)?\\b" line)
    'match)
   ((string-match-p "\\`case\\b" line)
    'case)
   ((string-match-p "\\`else\\(if\\)?\\b" line)
    'else)
   (t 'statement)))

(defun modelica--indent-top-kind (stack)
  (caar stack))

(defun modelica--indent-top-base (stack)
  (cdar stack))

(defun modelica--indent-compute-column ()
  "Return parser-style indentation column for the current line."
  (or (and (fboundp 'modelica-mode-parser-indent-column)
           (modelica-mode-parser-indent-column (point) modelica-indent-level))
      (save-excursion
        (let ((target (line-beginning-position))
              (indent 0)
              stack)
          (goto-char (point-min))
          (while (< (line-beginning-position) target)
            (let* ((line (modelica--indent-line-text))
                   (kind (modelica--indent-line-kind line)))
              (unless (or (string-empty-p line)
                          (string-prefix-p "//" line))
                (pcase kind
                  ('class
                   (push (cons 'class indent) stack)
                   (setq indent (+ indent modelica-indent-level)))
                  ('section
                   (setq indent (+ (or (modelica--indent-top-base stack) 0)
                                   modelica-indent-level)))
                  ('control
                   (push (cons 'control indent) stack)
                   (setq indent (+ indent modelica-indent-level)))
                  ('match
                   (push (cons 'match indent) stack)
                   (setq indent (+ indent modelica-indent-level)))
                  ('end
                   (when stack
                     (setq indent (modelica--indent-top-base stack))
                     (pop stack))))))
            (forward-line 1))
          (let* ((line (modelica--indent-line-text))
                 (kind (modelica--indent-line-kind line)))
            (pcase kind
              ('section (or (modelica--indent-top-base stack) 0))
              ('end (if stack (modelica--indent-top-base stack) 0))
              ('else (if (eq (modelica--indent-top-kind stack) 'match)
                         indent
                       (max 0 (- indent modelica-indent-level))))
              (_ indent)))))))

(defun modelica-indent-command ()
  "Indent for special part of code."
  (let* ((indent-str (modelica-calculate-indent))
         (type (car indent-str)))
    (cond ((and (eq type 'paramlist)
                (or (memq 'all modelica-auto-lineup)
                    (memq 'paramlist modelica-auto-lineup)))
           (modelica-indent-paramlist)
           (modelica-indent-paramlist))
          ((and (eq type 'declaration)
                (or (memq 'all modelica-auto-lineup)
                    (memq 'declaration  modelica-auto-lineup)))
           (modelica-indent-declaration))
          ((and (eq type 'case) (not (looking-at "^[ \t]*$"))
                (or (memq 'all modelica-auto-lineup)
                    (memq 'case modelica-auto-lineup)))
           (modelica-indent-case)))
    (if (looking-at "[ \t]+$")
        (skip-chars-forward " \t"))))

(defun modelica-indent-line ()
  "Indent current line as a Modelica statement."
  (let ((indent (modelica--indent-compute-column)))
    (delete-horizontal-space)
    (indent-to indent)))

(defun modelica-calculate-indent ()
  "Calculate the indent of the current Modelica line.
Return a list of two elements: (INDENT-TYPE INDENT-LEVEL)."
  (save-excursion
    (let* ((parse-sexp-ignore-comments t)
	   (oldpos (point))
	   (state (save-excursion (parse-partial-sexp (point-min) (point))))
	   (nest 0) (par 0) (complete (looking-at "[ \t]*end\\>"))
	   (elsed (looking-at "[ \t]*else\\>")) (funccnt 0)
	   (did-func (looking-at "[ \t]*\\(procedure\\|function\\)\\>"))
	   (type (catch 'nesting
		   ;; Check if inside a string, comment or parenthesis
		   (cond ((nth 3 state) (throw 'nesting 'string))
			 ((nth 4 state) (throw 'nesting 'comment))
			 ((> (car state) 0)
			  (goto-char (scan-lists (point) -1 (car state)))
			  (setq par (1+ (current-column))))
			 ((save-excursion (beginning-of-line)
					  (eq (following-char) ?#))
			  (throw 'nesting 'cpp)))
		   ;; Loop until correct indent is found
		   (while t
		     (backward-sexp 1)
		     (cond (;--Escape from case statements
			    (and (looking-at "[A-Za-z0-9]+[ \t]*:[^=]")
				 (not complete)
				 (save-excursion (skip-chars-backward " \t")
						 (bolp))
				 (= (save-excursion
				      (end-of-line) (backward-sexp) (point))
				    (point))
				 (> (save-excursion (goto-char oldpos)
						    (beginning-of-line)
						    (point))
				    (point)))
			    (throw 'nesting 'caseblock))
			   (;--Beginning of program
			    (looking-at modelica-progbeg-re)
			    (throw 'nesting 'progbeg))
			   (;--No known statements
			    (bobp)
			    (throw 'nesting 'progbeg))
			   (;--Nest block outwards
			    (looking-at modelica-beg-block-re)
			    (if (= nest 0)
				(cond ((looking-at "case\\>")
				       (throw 'nesting 'case))
				      ((looking-at "record\\>")
				       (throw 'nesting 'declaration))
				      (t (throw 'nesting 'block)))
			      (if (and (looking-at "record\\>") (= nest 1))
				  (setq funccnt (1- funccnt)))
			      (setq nest (1- nest))))
			   (;--Nest block inwards
			    (looking-at modelica-end-block-re)
			    (if (and (looking-at "end\\s ")
				     elsed (not complete))
				(throw 'nesting 'block))
			    (if (= nest 0)
				(setq funccnt (1+ funccnt)))
			    (setq complete t
				  nest (1+ nest)))
			   (;--Defun (or parameter list)
			    (and (looking-at modelica-defun-re)
				 (progn (setq funccnt (1- funccnt)
					      did-func t)
					(or (bolp) (< funccnt 0))))
			    ;; Prevent searching whole buffer
			    (if (and (bolp) (>= funccnt 0))
				(throw 'nesting 'progbeg))
			    (if (= 0 par)
				(throw 'nesting 'defun)
			      (setq par 0)
			      (let ((n 0))
				(while (re-search-forward
					"\\(\\<record\\>\\)\\|\\<end\\>"
					oldpos t)
				  (if (match-end 1)
				      (setq n (1+ n)) (setq n (1- n))))
				(if (> n 0)
				    (throw 'nesting 'declaration)
				  (throw 'nesting 'paramlist)))))
			   (;--Declaration part
			    (and (looking-at modelica-declaration-re)
				 (not did-func)
				 (= funccnt 0))
			    (if (save-excursion
				  (goto-char oldpos)
				  (forward-line -1)
				  (looking-at "^[ \t]*$"))
				(throw 'nesting 'unknown)
			      (throw 'nesting 'declaration)))
			   (;--If, else or while statement
			    (and (not complete)
				 (looking-at modelica-sub-block-re))
			    (throw 'nesting 'block))
			   (;--Found complete statement
			    (save-excursion (forward-sexp 1)
					    (= (following-char) ?\;))
			    (setq complete t))
			   )))))

    ;; Return type of block and indent level.
    (if (> par 0)                               ; Unclosed Parenthesis
	  (list 'contexp par)
	(list type (modelica-indent-level))))))

(defun modelica-indent-level ()
  "Return the indent-level the current statement has.
Do not count labels, case statements or records."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[ \t]*[0-9a-zA-Z]+[ \t]*:[^=]")
        (search-forward ":" nil t)
      (if (looking-at ".*=[ \t]*record\\>")
          (search-forward "=" nil t)))
    (skip-chars-forward " \t")
    (current-column)))

(defun modelica-indent-comment ()
  "Return indent for current comment."
  (save-excursion
    (re-search-backward "\\((\\*\\)\\|{" nil t)
    (if (match-beginning 1)
        (1+ (current-column))
      (current-column))))

(defun modelica-indent-case ()
  "Indent within case statements."
  (let ((savepos (point-marker))
        (end (prog2
                 (end-of-line)
                 (point-marker)
               (re-search-backward "\\<case\\>" nil t)))
        (beg (point))
        (modelica--extra-indent 0))
    ;; Get right indent
    (while (< (point) end)
      (if (re-search-forward
           "^[ \t]*[^ \t,:]+[ \t]*\\(,[ \t]*[^ \t,:]+[ \t]*\\)*:"
           (marker-position end) 'move)
          (forward-char -1))
      (if (< (point) end)
          (progn
            (delete-horizontal-space)
            (if (> (current-column) modelica--extra-indent)
                (setq modelica--extra-indent (current-column)))
            (modelica-end-of-statement))))
    (goto-char beg)
    ;; Indent all case statements
    (while (< (point) end)
      (if (re-search-forward
           "^[ \t]*[^][ \t,\\.:]+[ \t]*\\(,[ \t]*[^ \t,:]+[ \t]*\\)*:"
           (marker-position end) 'move)
          (forward-char -1))
      (indent-to (1+ modelica--extra-indent))
      (if (/= (following-char) ?:)
          ()
        (forward-char 1)
        (delete-horizontal-space)
        (insert " "))
      (modelica-end-of-statement))
    (goto-char savepos)))

(defun modelica-indent-paramlist (&optional arg)
  "Indent current line in parameterlist.
If optional ARG is non-nil, just return the
indent of the current line in parameterlist."
  (save-excursion
    (let* ((oldpos (point))
           (stpos (progn (goto-char (scan-lists (point) -1 1)) (point)))
           (stcol (1+ (current-column)))
           (edpos (progn (modelica-declaration-end)
                         (search-backward ")" (line-beginning-position) t)
                         (point)))
           (usevar (re-search-backward "\\<var\\>" stpos t)))
      (if arg (progn
                ;; If arg, just return indent
                (goto-char oldpos)
                (beginning-of-line)
                (if (or (not usevar) (looking-at "[ \t]*var\\>"))
                    stcol (+ 4 stcol)))
        (goto-char stpos)
        (forward-char 1)
        (delete-horizontal-space)
        (if (and usevar (not (looking-at "var\\>")))
            (indent-to (+ 4 stcol)))
        (modelica-indent-declaration nil stpos edpos)))))

(defun modelica-indent-declaration (&optional arg start end)
  "Indent current lines as declaration, lining up the `:'s or `='s."
  (let ((pos (point-marker)))
    (if (and (not (or arg start)) (not (modelica-declaration-beg)))
        ()
      (let ((lineup (if (or (looking-at "\\<var\\>\\|\\<record\\>") arg start)
                        ":" "="))
            (stpos (if start start
                     (forward-word-strictly 2) (backward-word 1) (point)))
            (edpos (set-marker (make-marker)
                               (if end end
                                 (max (progn (modelica-declaration-end)
                                             (point))
                                      pos))))
            modelica--extra-indent)

        (goto-char stpos)
        ;; Indent lines in record block
        (if arg
            (while (<= (point) edpos)
              (beginning-of-line)
              (delete-horizontal-space)
              (if (looking-at "end\\>")
                  (indent-to arg)
                (indent-to (+ arg modelica-indent-level)))
              (forward-line 1)))

        ;; Do lineup
        (setq modelica--extra-indent (modelica-get-lineup-indent stpos edpos lineup))
        (goto-char stpos)
        (while (and (<= (point) edpos) (not (eobp)))
          (if (search-forward lineup (line-end-position) 'move)
              (forward-char -1))
          (delete-horizontal-space)
          (indent-to modelica--extra-indent)
          (if (not (looking-at lineup))
              (forward-line 1) ; No more indent if there is no : or =
            (forward-char 1)
            (delete-horizontal-space)
            (insert " ")
            ;; Indent record block
            (if (looking-at "record\\>")
                (modelica-indent-declaration (current-column)))
            (forward-line 1)))))

    ;; If arg - move point
    (if arg (forward-line -1)
      (goto-char pos))))

                                        ;  "Return the indent level that will line up several lines within the region
                                        ;from b to e nicely. The lineup string is str."
(defun modelica-get-lineup-indent (b e str)
  (save-excursion
    (let ((modelica--extra-indent 0)
          (reg (concat str "\\|\\(\\<record\\>\\)\\|" modelica-defun-re)))
      (goto-char b)
      ;; Get rightmost position
      (while (< (point) e)
        (and (re-search-forward reg (min e (line-end-position 2)) 'move)
             (cond ((match-beginning 1)
                    ;; Skip record blocks
                    (modelica-declaration-end))
                   ((match-beginning 2)
                    ;; We have entered a new procedure.  Exit.
                    (goto-char e))
                   (t
                    (goto-char (match-beginning 0))
                    (skip-chars-backward " \t")
                    (if (> (current-column) modelica--extra-indent)
                        (setq modelica--extra-indent (current-column)))
                    (goto-char (match-end 0))
                    (end-of-line)
                    ))))
      ;; In case no lineup was found
      (if (> modelica--extra-indent 0)
          (1+ modelica--extra-indent)
        ;; No lineup-string found
        (goto-char b)
        (end-of-line)
        (skip-chars-backward " \t")
        (1+ (current-column))))))

;;;
;;; Completion
(defun modelica-string-diff (str1 str2)
  "Return index of first letter where STR1 and STR2 differs."
  (catch 'done
    (let ((diff 0))
      (while t
        (if (or (> (1+ diff) (length str1))
                (> (1+ diff) (length str2)))
            (throw 'done diff))
        (or (equal (aref str1 diff) (aref str2 diff))
            (throw 'done diff))
        (setq diff (1+ diff))))))

;; Calculate all possible completions for functions if argument is `function',
;; completions for procedures if argument is `procedure' or both functions and
;; procedures otherwise.

(defun modelica-func-completion (type modelica-str)
  ;; Build regular expression for function/procedure names
  (save-excursion
    (if (string= modelica-str "")
        (setq modelica-str "[a-zA-Z_]"))
    (let ((modelica-str (concat (cond
                                 ((eq type 'procedure) "\\<\\(procedure\\)\\s +")
                                 ((eq type 'function) "\\<\\(function\\)\\s +")
                                 (t "\\<\\(function\\|procedure\\)\\s +"))
                                "\\<\\(" modelica-str "[a-zA-Z0-9_.]*\\)\\>"))
          (modelica-all ())
          match)

      (if (not (looking-at "\\<\\(function\\|procedure\\)\\>"))
          (re-search-backward "\\<\\(function\\|procedure\\)\\>" nil t))
      (forward-char 1)

      ;; Search through all reachable functions
      (while (modelica-beg-of-defun)
        (if (re-search-forward modelica-str (line-end-position) t)
            (progn (setq match (buffer-substring (match-beginning 2)
                                                 (match-end 2)))
                   (push match modelica-all)))
        (goto-char (match-beginning 0)))

      modelica-all)))

(defun modelica-get-completion-decl (modelica-str)
  ;; Macro for searching through current declaration (var, type or const)
  ;; for matches of `str' and adding the occurrence to `all'
  (let ((end (save-excursion (modelica-declaration-end)
                             (point)))
        (modelica-all ())
        match)
    ;; Traverse lines
    (while (< (point) end)
      (if (re-search-forward "[:=]" (line-end-position) t)
          ;; Traverse current line
          (while (and (re-search-backward
                       (concat "\\((\\|\\<\\(var\\|type\\|const\\)\\>\\)\\|"
                               modelica-symbol-re)
                       (line-beginning-position) t)
                      (not (match-end 1)))
            (setq match (buffer-substring (match-beginning 0) (match-end 0)))
            (if (string-match (concat "\\<" modelica-str) match)
                (push match modelica-all))))
      (if (re-search-forward "\\<record\\>" (line-end-position) t)
          (modelica-declaration-end)
        (forward-line 1)))

    modelica-all))

(defun modelica-type-completion (modelica-str)
  "Calculate all possible completions for types."
  (let ((start (point))
        (modelica-all ())
        goon)
    ;; Search for all reachable type declarations
    (while (or (modelica-beg-of-defun)
               (setq goon (not goon)))
      (save-excursion
        (if (and (< start (prog1 (save-excursion (modelica-end-of-defun)
                                                 (point))
                            (forward-char 1)))
                 (re-search-forward
                  "\\<type\\>\\|\\<\\(begin\\|function\\)\\>"
                  start t)
                 (not (match-end 1)))
            ;; Check current type declaration
            (setq modelica-all
                  (nconc (modelica-get-completion-decl modelica-str)
                         modelica-all)))))

    modelica-all))

(defun modelica-var-completion (prefix)
  "Calculate all possible completions for variables (or constants)."
  (save-excursion
    (let ((start (point))
          (modelica-all ())
          goon twice)
      ;; Search for all reachable var declarations
      (while (or (modelica-beg-of-defun)
                 (setq goon (not goon)))
        (save-excursion
          (if (> start (prog1 (save-excursion (modelica-end-of-defun)
                                              (point))))
              ()                        ; Declarations not reachable
            (if (search-forward "(" (line-end-position) t)
                ;; Check parameterlist
                ;; FIXME: modelica-get-completion-decl doesn't understand
                ;; the var declarations in parameter lists :-(
                (setq modelica-all
                      (nconc (modelica-get-completion-decl prefix)
                             modelica-all)))
            (setq twice 2)
            (while (>= (setq twice (1- twice)) 0)
              (cond
               ((and (re-search-forward
                      (concat "\\<\\(var\\|const\\)\\>\\|"
                              "\\<\\(begin\\|function\\|procedure\\)\\>")
                      start t)
                     (not (match-end 2)))
                ;; Check var/const declarations
                (setq modelica-all
                      (nconc (modelica-get-completion-decl prefix)
                             modelica-all)))
               ((match-end 2)
                (setq twice 0)))))))
      modelica-all)))


(defun modelica-keyword-completion (keyword-list modelica-str)
  "Give list of all possible completions of keywords in KEYWORD-LIST."
  (let ((modelica-all ()))
    (dolist (s keyword-list)
      (if (string-match (concat "\\<" modelica-str) s)
          (push s modelica-all)))
    modelica-all))

;; Function passed to completing-read, try-completion or
;; all-completions to get completion on STR. If predicate is non-nil,
;; it must be a function to be called for every match to check if this
;; should really be a match. If flag is t, the function returns a list
;; of all possible completions. If it is nil it returns a string, the
;; longest possible completion, or t if STR is an exact match. If flag
;; is 'lambda, the function returns t if STR is an exact match, nil
;; otherwise.

(defvar modelica-completion-cache nil)

(defun modelica-completion (modelica-str modelica-pred modelica-flag)
  (let ((all (car modelica-completion-cache)))
    ;; Check the cache's freshness.
    (unless (and modelica-completion-cache
                 (string-prefix-p (nth 1 modelica-completion-cache) modelica-str)
                 (eq (current-buffer) (nth 2 modelica-completion-cache))
                 (eq (field-beginning) (nth 3 modelica-completion-cache)))
      (let ((state (car (modelica-calculate-indent))))
        (setq all
              ;; Determine what should be completed
              (cond
               (              ;--Within a declaration or parameterlist
                (or (eq state 'declaration) (eq state 'paramlist)
                    (and (eq state 'defun)
                         (save-excursion
                           (re-search-backward ")[ \t]*:" (line-beginning-position) t))))
                (if (or (eq state 'paramlist) (eq state 'defun))
                    (modelica-beg-of-defun))
                (nconc
                 (modelica-type-completion modelica-str)
                 (modelica-keyword-completion modelica-type-keywords modelica-str)))
               (                        ;--Starting a new statement
                (and (not (eq state 'contexp))
                     (save-excursion
                       (skip-chars-backward "a-zA-Z0-9_.")
                       (backward-sexp 1)
                       (or (looking-at modelica-nosemi-re)
                           (progn
                             (forward-sexp 1)
                             (looking-at "\\s *\\(;\\|:[^=]\\)")))))
                (nconc
                 (modelica-var-completion modelica-str)
                 (modelica-func-completion 'procedure modelica-str)
                 (modelica-keyword-completion modelica-start-keywords modelica-str)))
               (t                       ;--Anywhere else
                (nconc
                 (modelica-var-completion modelica-str)
                 (modelica-func-completion 'function modelica-str)
                 (modelica-keyword-completion modelica-separator-keywords
                                              modelica-str)))))

        (setq modelica-completion-cache
              (list all modelica-str (current-buffer) (field-beginning)))))

    ;; Now we have built a list of all matches. Give response to caller
    (complete-with-action modelica-flag all modelica-str modelica-pred)))

(defvar modelica-last-word-numb 0)
(defvar modelica-last-word-shown nil)
(defvar modelica-last-completions nil)

(defun modelica-completions-at-point ()
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_") (point)))
         (e (save-excursion (skip-chars-forward "a-zA-Z0-9_") (point))))
    (when (> e b)
      (list b e #'modelica-completion))))

(defalias 'modelica-complete-word #'completion-at-point)

(defalias 'modelica-show-completions #'completion-help-at-point)


(defun modelica-get-default-symbol ()
  "Return symbol around current point as a string."
  (save-excursion
    (buffer-substring (progn
                        (skip-chars-backward " \t")
                        (skip-chars-backward "a-zA-Z0-9_")
                        (point))
                      (progn
                        (skip-chars-forward "a-zA-Z0-9_")
                        (point)))))

(defun modelica-build-defun-re (str &optional arg)
  "Return function/procedure starting with STR as regular expression.
With optional second arg non-nil, STR is the complete name of the instruction."
  (if arg
      (concat "^\\(function\\|class\\package|\\uniontype|record\\block|connector\\)[ \t]+\\(" str "\\)\\>")
    (concat "^\\(function\\|class\\package|\\uniontype|record\\block|connector\\)[ \t]+\\(" str "[a-zA-Z0-9_]*\\)\\>")))
;; Function passed to completing-read, try-completion or
;; all-completions to get completion on any function name. If
;; predicate is non-nil, it must be a function to be called for every
;; match to check if this should really be a match. If flag is t, the
;; function returns a list of all possible completions. If it is nil
;; it returns a string, the longest possible completion, or t if STR
;; is an exact match. If flag is 'lambda, the function returns t if
;; STR is an exact match, nil otherwise.

(defun modelica-comp-defun (modelica-str modelica-pred modelica-flag)
  (save-excursion
    (let ((modelica-all nil))

      ;; Build regular expression for functions
      (let ((modelica-str (modelica-build-defun-re (if (string= modelica-str "")
                                                       "[a-zA-Z_]"
                                                     modelica-str))))
        (goto-char (point-min))

        ;; Build a list of all possible completions
        (while (re-search-forward modelica-str nil t)
          (push (match-string 2) modelica-all)))

      ;; Now we have built a list of all matches. Give response to caller
      (complete-with-action modelica-flag modelica-all modelica-str modelica-pred))))

(defun modelica-goto-class ()
  "Move to specified Modelica function/procedure/model/class.
The default is a name found in the buffer around point."
  (interactive)
  (let* ((default (modelica-get-default-symbol))
         (default (if (modelica-comp-defun default nil 'lambda)
                      default ""))
         (label
          ;; Do completion with default.
          (completing-read (if (not (string= default ""))
                               (concat "Label (default " default "): ")
                             "Label: ")
                           ;; Complete with the defuns found in the
                           ;; current-buffer.
                           (let ((buf (current-buffer)))
                             (lambda (s p a)
                               (with-current-buffer buf
                                 (modelica-comp-defun s p a))))
                           nil t "")))
    ;; If there was no response on prompt, use default value.
    (if (string= label "")
        (setq label default))
    ;; Goto right place in buffer if label is not an empty string.
    (or (string= label "")
        (progn
          (goto-char (point-min))
          (re-search-forward (modelica-build-defun-re label t))
          (beginning-of-line)))))

;;;
;;; Modelica-outline-mode
;;;
(defvar modelica-outline-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-\C-a"  'modelica-outline-prev-defun)
    (define-key map "\M-\C-e"  'modelica-outline-next-defun)
    (define-key map "\C-c\C-d" 'modelica-outline-goto-defun)
    (define-key map "\C-c\C-s" 'modelica-show-all)
    (define-key map "\C-c\C-h" 'modelica-hide-other-defuns)
    map)
  "Keymap used in Modelica Outline mode.")

(define-minor-mode modelica-outline-mode
  "Outline-line minor mode for Modelica mode.
When enabled, portions of the text being edited may be made
invisible.\\<modelica-outline-map>
Modelica Outline mode provides some additional commands.
\\[modelica-outline-prev-defun]\
\t- Move to previous function/procedure, hiding everything else.
\\[modelica-outline-next-defun]\
\t- Move to next function/procedure, hiding everything else.
\\[modelica-outline-goto-defun]\
\t- Goto function/procedure prompted for in minibuffer,
\t  hide all other functions.
\\[modelica-show-all]\t- Show the whole buffer.
\\[modelica-hide-other-defuns]\
\t- Hide everything but the current function (function under the cursor).
\\[modelica-outline-mode]\t- Leave Modelica Outline mode."
  :init-value nil :lighter " Outl" :keymap modelica-outline-map
  (add-to-invisibility-spec '(modelica . t))
  (unless modelica-outline-mode
    (modelica-show-all)))

(defun modelica-outline-change (b e hide)
  (when (> e b)
    ;; We could try and optimize this in the case where the region is
    ;; already hidden.  But I'm not sure it's worth the trouble.
    (remove-overlays b e 'invisible 'modelica)
    (when hide
      (let ((ol (make-overlay b e nil t nil)))
        (overlay-put ol 'invisible 'modelica)
        (overlay-put ol 'evaporate t)))))

(defun modelica-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (modelica-outline-change (point-min) (point-max) nil))

(defun modelica-hide-other-defuns ()
  "Show only the current defun."
  (interactive)
  (save-excursion
    (let ((beg (progn (if (not (looking-at "\\(function\\|procedure\\)\\>"))
                          (modelica-beg-of-defun))
                      (line-beginning-position)))
          (end (progn (modelica-end-of-defun)
                      (backward-sexp 1)
                      (line-beginning-position 2)))
          (opoint (point-min)))
      ;; BEG at BOL.
      ;; OPOINT at EOL.
      ;; END at BOL.
      (goto-char (point-min))

      ;; Hide all functions before current function
      (while (re-search-forward "^[ \t]*\\(function\\|procedure\\)\\>"
                                beg 'move)
        (modelica-outline-change opoint (line-end-position 0) t)
        (setq opoint (line-end-position))
        ;; Functions may be nested
        (if (> (progn (modelica-end-of-defun) (point)) beg)
            (goto-char opoint)))
      (if (> beg opoint)
          (modelica-outline-change opoint (1- beg) t))

      ;; Show current function
      (modelica-outline-change (1- beg) end nil)
      ;; Hide nested functions
      (forward-char 1)
      (while (re-search-forward "^\\(function\\|class\\package|\\uniontype|record\\block|connector\\)\\>" end 'move)
        (setq opoint (line-end-position))
        (modelica-end-of-defun)
        (modelica-outline-change opoint (line-end-position) t))

      (goto-char end)
      (setq opoint end)

      ;; Hide all function after current function
      (while (re-search-forward "^\\(function\\|class\\package|\\uniontype|record\\block|connector\\)\\>" nil 'move)
        (modelica-outline-change opoint (line-end-position 0) t)
        (setq opoint (line-end-position))
        (modelica-end-of-defun))
      (modelica-outline-change opoint (point-max) t)

      ;; Hide main program
      (if (< (progn (forward-line -1) (point)) end)
          (progn
            (goto-char beg)
            (modelica-end-of-defun)
            (backward-sexp 1)
            (modelica-outline-change (line-end-position) (point-max) t))))))

(defun modelica-outline-next-defun ()
  "Move to next function/procedure, hiding all others."
  (interactive)
  (modelica-end-of-defun)
  (modelica-hide-other-defuns))

(defun modelica-outline-prev-defun ()
  "Move to previous function/procedure, hiding all others."
  (interactive)
  (modelica-beg-of-defun)
  (modelica-hide-other-defuns))

(defun modelica-outline-goto-defun ()
  "Move to specified function/procedure, hiding all others."
  (interactive)
  (modelica-goto-class)
  (modelica-hide-other-defuns))

(defun modelica-check-buffer ()
  "Parse the current buffer and report the first Modelica parse error.
On success, report that parsing succeeded.  On failure, move point to
the reported line and column when the parser message contains one."
  (interactive)
  (modelica--ensure-parser-loaded)
  (if (not (fboundp 'modelica-mode-parse-buffer))
      (user-error "Modelica parser is unavailable")
    (condition-case err
        (progn
          (modelica-mode-clear-parse-cache)
          (modelica-mode-parse-buffer)
          (message "Modelica parse OK")
          t)
      (absyn-parse-error
       (let ((msg (or (cadr err) (format "%S" err))))
         (when (string-match "\\([0-9]+\\):\\([0-9]+\\)" msg)
           (goto-char (point-min))
           (forward-line (1- (string-to-number (match-string 1 msg))))
           (move-to-column (1- (string-to-number (match-string 2 msg)))))
         (message "Modelica parse error: %s" msg)
         nil)))))

;; hide/show annotations
(add-to-invisibility-spec '(modelica-annotation . t))


(defun modelica-hide-annotations (beg end)
  "Hide all annotations."
  (save-excursion
    (let (beg-hide end-hide)
      (goto-char beg)
      (while
          (and (< (point) end)
               (re-search-forward "\\<annotation[ \t\n]*\(" end t))
        (setq beg-hide (match-end 0))
        (backward-char)
        (forward-sexp)
        (setq end-hide (- (point) 1))
        (modelica-flag-region beg-hide end-hide t)))))

(defun modelica-show-annotations (beg end)
  "Show annotations from beg to end"
  (modelica-flag-region beg end nil))

(defun modelica-hide-all-annotations ()
  "Hide all annotations"
  (interactive)
  (modelica-hide-annotations (point-min) (point-max)))

(defun modelica-hide-annotation ()
  "Hide annotation of current statement"
  (interactive)
  (save-excursion
    (let (beg end)
      ;; move to beginning of current statement
      (modelica-statement-start)
      (setq beg (point))
      ;; move to beginning of next statement
      (modelica-forward-statement)
      (setq end (point))
      ;; hide annotations from beg to end
      (modelica-hide-annotations beg end))))

(defun modelica-show-all-annotations ()
  "Show all annotations"
  (interactive)
  (modelica-show-annotations (point-min) (point-max)))

(defun modelica-show-annotation ()
  "Show annotation of current statement"
  (interactive)
  (save-excursion
    (let (beg end)
      ;; move to beginning of current statement
      (modelica-statement-start)
      (setq beg (point))
      ;; move to beginning of next statement
      (modelica-forward-statement)
      (setq end (point))
      ;; show annotations from beg to end
      (modelica-show-annotations beg end))))

(defun modelica-discard-overlays (beg end value)
  (if (< end beg)
      (setq beg (prog1 end (setq end beg))))
  (save-excursion
    (let ((overlays (overlays-in beg end))
          o)
      (while overlays
        (setq o (car overlays))
        (if (eq (overlay-get o 'invisible) value)
            (delete-overlay o))
        (setq overlays (cdr overlays))))))

(defun modelica-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
  (save-excursion
    (goto-char from)
    (modelica-discard-overlays from to 'modelica-annotation)
    (if flag
        (let ((o (make-overlay from to)))
          (overlay-put o 'invisible 'modelica-annotation)
          (overlay-put o 'isearch-open-invisible
                       'modelica-isearch-open-invisible)))))


  (defvar modelica-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-j"   'modelica-newline-and-indent)
      (define-key map "\C-c\C-e" 'modelica-insert-end)
      (define-key map "\C-c\C-p" 'modelica-check-buffer)
      (define-key map "\C-c\C-s" 'modelica-show-annotation)
      (define-key map "\C-c\C-h" 'modelica-hide-annotation)
      (define-key map "\es"   'modelica-show-all-annotations)
      (define-key map "\eh"   'modelica-hide-all-annotations)
      (define-key map "\C-c\C-c" 'comment-region)
      (define-key map "\e;"   'modelica-indent-for-comment)
      (define-key map "\ej"   'modelica-indent-new-comment-line)
      (define-key map "\ef"   'modelica-forward-statement)
      (define-key map "\eb"   'modelica-backward-statement)
      (define-key map "\en"   'modelica-forward-block)
      (define-key map "\ep"   'modelica-backward-block)
      (define-key map "\ea"   'modelica-to-block-begin)
      (define-key map "\ee"   'modelica-to-block-end)
      map)
    "Keymap for Modelica mode.")



  (defvar modelica-mode-menu
    '("Modelica"
      ("Move to"
       [" - next statement"        modelica-forward-statement t]
       [" - previous statement"    modelica-backward-statement t]
       [" - start of code block"   modelica-to-block-begin t]
       [" - end of code block"     modelica-to-block-end t]
       )
      [" - next code block"        modelica-forward-block t]
      [" - previous code block"    modelica-backward-block t]
      "-"
      ("Annotation"
       [" - show all"              modelica-show-all-annotations t]
       [" - hide all"              modelica-hide-all-annotations t]
       )
      [" - show current"          modelica-show-annotation t]
      [" - hide current"          modelica-hide-annotation
       :keys "C-c C-h" :active t]
      "-"
      ("Indent"
       [" - for comment"           modelica-indent-for-comment t]
       ["Newline and indent"       modelica-newline-and-indent
        :keys "C-j" :active t]
       ["New comment line"         modelica-indent-new-comment-line t]
       )
      [" - line"                   indent-for-tab-command t]
      [" - region"                 indent-region (mark)]
      "-"
      ["Comment out region"        comment-region  (mark)]
      ["Uncomment region"          (comment-region (point) (mark) '(4))
       :keys "C-u C-c C-c" :active (mark)]
      "-"
      ["End code block"            modelica-insert-end t]
      )
    "Menu for Modelica mode.")

(provide 'modelica-mode)

;;; modelica-mode.el ends here
