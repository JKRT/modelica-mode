;;; absyn-parser.el --- Top-down Modelica parser using Absyn AST -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The modelica-mode contributors.
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This file mirrors the OpenModelica Absyn uniontypes through
;; `absyn-ast.el` and provides a hand-written recursive-descent parser.

(require 'cl-lib)
(require 'absyn-ast)
;;; Lexer
;;; ---------------------------------------------------------------------------

(define-error 'absyn-parse-error "Absyn parse error")

(cl-defstruct (absyn-token (:constructor absyn-token
                                         (kind value start end line column end-line end-column))
                            (:copier nil))
  kind value start end line column end-line end-column)

(defconst absyn--keywords
  '("algorithm" "and" "annotation" "block" "break" "class" "connect" "connector"
    "constant" "constrainedby" "der" "discrete" "each" "else" "elseif" "elsewhen"
    "encapsulated" "end" "enumeration" "equation" "expandable" "extends" "external"
    "false" "final" "flow" "for" "function" "guard" "if" "import" "impure" "in"
    "initial" "inner" "input" "loop" "match" "matchcontinue" "model" "not" "operator"
    "or" "outer" "output" "overload" "package" "parameter" "partial" "parfor"
    "parglobal" "parlocal" "protected" "public" "pure" "record" "redeclare"
    "replaceable" "return" "stream" "then" "threaded" "true" "try" "type" "uniontype"
    "when" "while" "within"
    ;; Extensions used by the supplied Absyn.
    "mode" "field" "reconfigure" "upon" "prompt" "modification" "variables"
    "ensure" "invariant" "constraint" "constraints" "optimization" "failure" "defineunit"))

(defun absyn--keyword-p (s)
  (member s absyn--keywords))

(defun absyn-lex-string (text &optional _file)
  "Lex TEXT into a vector of `absyn-token' values."
  (let ((len (length text))
        (i 0)
        (line 1)
        (col 1)
        toks)
    (cl-labels
        ((eof () (>= i len))
         (ch (&optional n) (aref text (+ i (or n 0))))
         (has (n) (< (+ i n) len))
         (advance (&optional n)
           (dotimes (_ (or n 1))
             (unless (eof)
               (let ((c (ch)))
                 (setq i (1+ i))
                 (if (= c ?\n)
                     (progn (setq line (1+ line)) (setq col 1))
                   (setq col (1+ col)))))))
         (emit (kind value start start-line start-col)
           (push (absyn-token kind value start i start-line start-col line col) toks))
         (identifier-start-p (c)
           (or (and (>= c ?A) (<= c ?Z))
               (and (>= c ?a) (<= c ?z))
               (= c ?_)))
         (identifier-part-p (c)
           (or (identifier-start-p c)
               (and (>= c ?0) (<= c ?9))
               (= c ?$)))
         (digit-p (c) (and (>= c ?0) (<= c ?9)))
         (skip-line-comment ()
           (while (and (not (eof)) (/= (ch) ?\n)) (advance)))
         (skip-block-comment ()
           (advance 2)
           (while (and (not (eof))
                       (not (and (= (ch) ?*) (has 1) (= (ch 1) ?/))))
             (advance))
           (when (not (eof)) (advance 2)))
         (scan-identifier ()
           (let ((start i) (sl line) (sc col))
             (while (and (not (eof)) (identifier-part-p (ch))) (advance))
             (let ((s (substring text start i)))
               (emit (if (absyn--keyword-p s) 'keyword 'identifier) s start sl sc))))
         (scan-quoted-identifier ()
           (let ((start i) (sl line) (sc col) (buf nil))
             (advance) ; opening quote
             (while (and (not (eof)) (/= (ch) ?'))
               (if (and (= (ch) ?\\) (has 1))
                   (progn (advance) (push (ch) buf) (advance))
                 (push (ch) buf)
                 (advance)))
             (when (eof)
               (signal 'absyn-parse-error (list "Unterminated quoted identifier")))
             (advance)
             (emit 'identifier (apply #'string (nreverse buf)) start sl sc)))
         (scan-string ()
           (let ((start i) (sl line) (sc col) (buf nil))
             (advance) ; opening double quote
             (while (and (not (eof)) (/= (ch) ?\"))
               (if (and (= (ch) ?\\) (has 1))
                   (progn
                     (advance)
                     (let ((esc (ch)))
                       (push (pcase esc
                               (?n ?\n) (?t ?\t) (?r ?\r) (?\" ?\") (?\\ ?\\) (_ esc))
                             buf))
                     (advance))
                 (push (ch) buf)
                 (advance)))
             (when (eof)
               (signal 'absyn-parse-error (list "Unterminated string literal")))
             (advance)
             (emit 'string (apply #'string (nreverse buf)) start sl sc)))
         (scan-number ()
           (let ((start i) (sl line) (sc col) (realp nil))
             (while (and (not (eof)) (digit-p (ch))) (advance))
             (when (and (not (eof)) (= (ch) ?.) (has 1) (digit-p (ch 1)))
               (setq realp t)
               (advance)
               (while (and (not (eof)) (digit-p (ch))) (advance)))
             (when (and (not (eof)) (or (= (ch) ?e) (= (ch) ?E)))
               (setq realp t)
               (let ((save-i i) (save-line line) (save-col col))
                 (advance)
                 (when (and (not (eof)) (or (= (ch) ?+) (= (ch) ?-))) (advance))
                 (if (and (not (eof)) (digit-p (ch)))
                     (while (and (not (eof)) (digit-p (ch))) (advance))
                   ;; Not actually an exponent; rewind.
                   (setq i save-i line save-line col save-col))))
             (let ((s (substring text start i)))
               (emit (if realp 'real 'integer)
                     (if realp s (string-to-number s))
                     start sl sc))))
         (scan-symbol ()
           (let ((start i) (sl line) (sc col)
                 (multi '(".*" "./" ".^" ".+" ".-" "<=" ">=" "==" "<>" ":=" "=>" "..")))
             (let ((matched nil))
               (dolist (m multi)
                 (when (and (not matched)
                            (<= (+ i (length m)) len)
                            (string= (substring text i (+ i (length m))) m))
                   (setq matched m)))
               (if matched
                   (progn (advance (length matched)) (emit 'symbol matched start sl sc))
                 (let ((c (ch)))
                   (if (member c '(?\( ?\) ?\[ ?\] ?\{ ?\} ?\; ?\, ?\: ?\. ?= ?+ ?- ?* ?/ ?^ ?< ?>))
                       (progn (advance) (emit 'symbol (char-to-string c) start sl sc))
                     (signal 'absyn-parse-error
                             (list (format "Unexpected character %S at %d:%d" c line col))))))))))
      (while (not (eof))
        (cond
         ((memq (ch) '(?\s ?\t ?\r ?\n)) (advance))
         ((and (= (ch) ?/) (has 1) (= (ch 1) ?/)) (skip-line-comment))
         ((and (= (ch) ?/) (has 1) (= (ch 1) ?*)) (skip-block-comment))
         ((identifier-start-p (ch)) (scan-identifier))
         ((= (ch) ?') (scan-quoted-identifier))
         ((= (ch) ?\") (scan-string))
         ((digit-p (ch)) (scan-number))
         (t (scan-symbol))))
      (let ((start i) (sl line) (sc col))
        (emit 'eof nil start sl sc))
      (vconcat (nreverse toks)))))

;;; ---------------------------------------------------------------------------
;;; Parser state and utilities
;;; ---------------------------------------------------------------------------

(cl-defstruct (absyn-parser-state (:copier nil))
  tokens
  (pos 0)
  file
  last)

(defun absyn--make-state (tokens file)
  (make-absyn-parser-state :tokens tokens :file (or file "<string>")))

(defun absyn--peek (st &optional n)
  (let* ((idx (+ (absyn-parser-state-pos st) (or n 0)))
         (tokens (absyn-parser-state-tokens st)))
    (if (< idx (length tokens))
        (aref tokens idx)
      (aref tokens (1- (length tokens))))))

(defun absyn--advance-token (st)
  (let ((tok (absyn--peek st)))
    (setf (absyn-parser-state-last st) tok)
    (unless (eq (absyn-token-kind tok) 'eof)
      (cl-incf (absyn-parser-state-pos st)))
    tok))

(defun absyn--token-matches-p (tok kind value)
  (and (eq (absyn-token-kind tok) kind)
       (if value (equal (absyn-token-value tok) value) t)))

(defun absyn--eof-p (st)
  (eq (absyn-token-kind (absyn--peek st)) 'eof))

(defun absyn--err (st fmt &rest args)
  (let ((tok (absyn--peek st)))
    (signal 'absyn-parse-error
            (list (format "%s at %s:%d:%d, got %S"
                          (apply #'format fmt args)
                          (absyn-parser-state-file st)
                          (absyn-token-line tok)
                          (absyn-token-column tok)
                          (absyn-token-value tok))))))

(defun absyn--accept-symbol (st sym)
  (when (absyn--token-matches-p (absyn--peek st) 'symbol sym)
    (absyn--advance-token st)))

(defun absyn--accept-keyword (st kw)
  (when (absyn--token-matches-p (absyn--peek st) 'keyword kw)
    (absyn--advance-token st)))

(defun absyn--accept-kind (st kind)
  (when (eq (absyn-token-kind (absyn--peek st)) kind)
    (absyn--advance-token st)))

(defun absyn--expect-symbol (st sym)
  (or (absyn--accept-symbol st sym)
      (absyn--err st "Expected symbol %s" sym)))

(defun absyn--expect-keyword (st kw)
  (or (absyn--accept-keyword st kw)
      (absyn--err st "Expected keyword %s" kw)))

(defun absyn--expect-ident-token (st)
  (or (absyn--accept-kind st 'identifier)
      (absyn--err st "Expected identifier")))

(defun absyn--expect-ident (st)
  (absyn-token-value (absyn--expect-ident-token st)))

(defun absyn--info (st start-token &optional end-token)
  (let ((e (or end-token (absyn-parser-state-last st) start-token)))
    (absyn-source-info (absyn-parser-state-file st)
                       nil
                       (absyn-token-line start-token)
                       (absyn-token-column start-token)
                       (absyn-token-end-line e)
                       (absyn-token-end-column e))))

(defun absyn--current-keyword-p (st kw)
  (absyn--token-matches-p (absyn--peek st) 'keyword kw))

(defun absyn--current-symbol-p (st sym)
  (absyn--token-matches-p (absyn--peek st) 'symbol sym))

(defun absyn--next-symbol-p (st sym)
  (absyn--token-matches-p (absyn--peek st 1) 'symbol sym))

(defun absyn--skip-extra-semicolons (st)
  (while (absyn--accept-symbol st ";")))

(defun absyn--reverse! (xs)
  (nreverse xs))

;;; ---------------------------------------------------------------------------
;;; Public API
;;; ---------------------------------------------------------------------------

(defun absyn-parse-string (text &optional file)
  "Parse Modelica source TEXT into an Absyn PROGRAM record."
  (let* ((tokens (absyn-lex-string text file))
         (st (absyn--make-state tokens file)))
    (prog1 (absyn--parse-stored-definition st)
      (unless (absyn--eof-p st)
        (absyn--err st "Expected end of input")))))

(defun absyn-parse-file (file)
  "Parse FILE into an Absyn PROGRAM record."
  (with-temp-buffer
    (insert-file-contents file)
    (absyn-parse-string (buffer-string) file)))

;;; ---------------------------------------------------------------------------
;;; Stored definition and class declarations
;;; ---------------------------------------------------------------------------

(defun absyn--parse-stored-definition (st)
  (let ((within (absyn-top))
        classes)
    (when (absyn--accept-keyword st "within")
      (setq within
            (if (absyn--current-symbol-p st ";")
                (absyn-top)
              (absyn-within (absyn--parse-path st))))
      (absyn--expect-symbol st ";"))
    (absyn--skip-extra-semicolons st)
    (while (not (absyn--eof-p st))
      (push (absyn--parse-class-definition st) classes)
      (absyn--expect-symbol st ";")
      (absyn--skip-extra-semicolons st))
    (absyn-program (absyn--reverse! classes) within)))

(defun absyn--class-start-token-p (tok)
  (and (eq (absyn-token-kind tok) 'keyword)
       (member (absyn-token-value tok)
               '("encapsulated" "partial" "final" "pure" "impure" "operator" "class" "model"
                 "record" "block" "connector" "type" "package" "function" "uniontype"
                 "optimization" "expandable"))))

(defun absyn--parse-class-definition (st)
  (let ((start (absyn--peek st))
        (encapsulated nil)
        (partial nil)
        (final nil)
        type-vars restriction name body)
    (setq encapsulated (not (null (absyn--accept-keyword st "encapsulated"))))
    (setq partial (not (null (absyn--accept-keyword st "partial"))))
    (setq final (not (null (absyn--accept-keyword st "final"))))
    (setq restriction (absyn--parse-restriction st))
    ;; `class extends Base ... end Base' has no ordinary identifier before the
    ;; `extends' keyword.  A normal class whose first element is an extends
    ;; clause is parsed by the long-class branch below.
    (if (absyn--accept-keyword st "extends")
        (progn
          (setq name (absyn--expect-ident st))
          (setq type-vars nil)
          (setq body (absyn--parse-class-extends-rest st name)))
      (setq name (absyn--expect-ident st))
      (setq type-vars (absyn--parse-type-vars st))
      (setq body (absyn--parse-class-body st name type-vars)))
    (absyn-class name partial final encapsulated restriction body (absyn--info st start))))

(defun absyn--parse-type-vars (st)
  (let (vars)
    (when (absyn--accept-symbol st "<")
      (unless (absyn--current-symbol-p st ">")
        (push (absyn--expect-ident st) vars)
        (while (absyn--accept-symbol st ",")
          (push (absyn--expect-ident st) vars)))
      (absyn--expect-symbol st ">"))
    (absyn--reverse! vars)))

(defun absyn--parse-restriction (st)
  (let ((purity (absyn-no-purity)))
    (cond
     ((absyn--accept-keyword st "pure")
      (setq purity (absyn-pure))
      (absyn--expect-keyword st "function")
      (absyn-r-function (absyn-fr-normal-function purity)))
     ((absyn--accept-keyword st "impure")
      (setq purity (absyn-impure))
      (absyn--expect-keyword st "function")
      (absyn-r-function (absyn-fr-normal-function purity)))
     ((absyn--accept-keyword st "operator")
      (cond
       ((absyn--accept-keyword st "record") (absyn-r-operator-record))
       ((absyn--accept-keyword st "function") (absyn-r-function (absyn-fr-operator-function)))
       (t (absyn-r-operator))))
     ((absyn--accept-keyword st "expandable")
      (absyn--expect-keyword st "connector")
      (absyn-r-exp-connector))
     ((absyn--accept-keyword st "class") (absyn-r-class))
     ((absyn--accept-keyword st "optimization") (absyn-r-optimization))
     ((absyn--accept-keyword st "model") (absyn-r-model))
     ((absyn--accept-keyword st "record") (absyn-r-record))
     ((absyn--accept-keyword st "block") (absyn-r-block))
     ((absyn--accept-keyword st "connector") (absyn-r-connector))
     ((absyn--accept-keyword st "type") (absyn-r-type))
     ((absyn--accept-keyword st "package") (absyn-r-package))
     ((absyn--accept-keyword st "function") (absyn-r-function (absyn-fr-normal-function purity)))
     ((absyn--accept-keyword st "uniontype") (absyn-r-uniontype))
     (t (absyn--err st "Expected class restriction")))))

(defun absyn--parse-class-body (st class-name type-vars)
  (if (absyn--accept-symbol st "=")
      (absyn--parse-short-class-body st)
    (absyn--parse-long-class-body st class-name type-vars)))

(defun absyn--parse-short-class-body (st)
  (cond
   ((absyn--accept-keyword st "enumeration")
    (absyn--parse-enumeration-body st))
   ((absyn--accept-keyword st "overload")
    (absyn--parse-overload-body st))
   ((absyn--accept-keyword st "der")
    (absyn--parse-pder-body st))
   (t
    (let* ((attrs (absyn--parse-element-attributes st))
           (ts (absyn--parse-type-spec st))
           (args (if (absyn--current-symbol-p st "(")
                     (absyn--parse-element-arg-list-parens st)
                   nil))
           (comment (absyn--parse-comment-opt st)))
      (absyn-derived ts attrs args comment)))))

(defun absyn--parse-enumeration-body (st)
  (let (lits enum comment)
    (absyn--expect-symbol st "(")
    (setq enum
          (cond
           ((absyn--accept-symbol st ":") (absyn-enum-colon))
           (t
            (unless (absyn--current-symbol-p st ")")
              (push (absyn--parse-enum-literal st) lits)
              (while (absyn--accept-symbol st ",")
                (push (absyn--parse-enum-literal st) lits)))
            (absyn-enumliterals (absyn--reverse! lits)))))
    (absyn--expect-symbol st ")")
    (setq comment (absyn--parse-comment-opt st))
    (absyn-enumeration enum comment)))

(defun absyn--parse-enum-literal (st)
  (absyn-enumliteral (absyn--expect-ident st) (absyn--parse-comment-opt st)))

(defun absyn--parse-overload-body (st)
  (let (paths comment)
    (absyn--expect-symbol st "(")
    (unless (absyn--current-symbol-p st ")")
      (push (absyn--parse-path st) paths)
      (while (absyn--accept-symbol st ",")
        (push (absyn--parse-path st) paths)))
    (absyn--expect-symbol st ")")
    (setq comment (absyn--parse-comment-opt st))
    (absyn-overload (absyn--reverse! paths) comment)))

(defun absyn--parse-pder-body (st)
  (let (fn vars comment)
    (absyn--expect-symbol st "(")
    (setq fn (absyn--parse-path st))
    (while (absyn--accept-symbol st ",")
      (push (absyn--expect-ident st) vars))
    (absyn--expect-symbol st ")")
    (setq comment (absyn--parse-comment-opt st))
    (absyn-pder fn (absyn--reverse! vars) comment)))

(defun absyn--parse-long-class-body (st class-name type-vars)
  (let ((comment-str (when (eq (absyn-token-kind (absyn--peek st)) 'string)
                       (absyn-token-value (absyn--advance-token st))))
        parts ann pair end-name)
    (setq pair (absyn--parse-composition st))
    (setq parts (car pair) ann (cdr pair))
    (absyn--expect-keyword st "end")
    (setq end-name (absyn--expect-ident st))
    (unless (string= class-name end-name)
      (message "absyn-parser: class starts as %s but ends as %s" class-name end-name))
    (absyn-parts type-vars nil parts ann comment-str)))

(defun absyn--parse-class-extends-rest (st base)
  (let (modifications comment pair parts ann end-name)
    (when (absyn--current-symbol-p st "(")
      (setq modifications (absyn--parse-element-arg-list-parens st)))
    (when (eq (absyn-token-kind (absyn--peek st)) 'string)
      (setq comment (absyn-token-value (absyn--advance-token st))))
    (setq pair (absyn--parse-composition st))
    (setq parts (car pair) ann (cdr pair))
    (absyn--expect-keyword st "end")
    (setq end-name (absyn--expect-ident st))
    (unless (string= base end-name)
      (message "absyn-parser: class extends %s but ends as %s" base end-name))
    (absyn-class-extends base modifications comment parts ann)))

;;; ---------------------------------------------------------------------------
;;; Class composition and sections
;;; ---------------------------------------------------------------------------

(defun absyn--section-start-p (st)
  (let ((tok (absyn--peek st)))
    (and (eq (absyn-token-kind tok) 'keyword)
         (member (absyn-token-value tok)
                 '("public" "protected" "equation" "algorithm" "initial" "external" "annotation" "end")))))

(defun absyn--parse-composition (st)
  "Parse class composition. Return (PARTS . ANNOTATIONS)."
  (let (parts annotations)
    (while (not (or (absyn--eof-p st) (absyn--current-keyword-p st "end")))
      (cond
       ((absyn--accept-keyword st "public")
        (let ((items (absyn--parse-element-list-until-section st)))
          (push (absyn-public items) parts)))
       ((absyn--accept-keyword st "protected")
        (let ((items (absyn--parse-element-list-until-section st)))
          (push (absyn-protected items) parts)))
       ((absyn--accept-keyword st "equation")
        (push (absyn-equations (absyn--parse-equation-items-until-class-section st)) parts))
       ((absyn--accept-keyword st "algorithm")
        (push (absyn-algorithms (absyn--parse-algorithm-items-until-class-section st)) parts))
       ((absyn--accept-keyword st "initial")
        (cond
         ((absyn--accept-keyword st "equation")
          (push (absyn-initialequations (absyn--parse-equation-items-until-class-section st)) parts))
         ((absyn--accept-keyword st "algorithm")
          (push (absyn-initialalgorithms (absyn--parse-algorithm-items-until-class-section st)) parts))
         (t (absyn--err st "Expected equation or algorithm after initial"))))
       ((absyn--current-keyword-p st "external")
        (push (absyn--parse-external-class-part st) parts))
       ((absyn--current-keyword-p st "annotation")
        (push (absyn--parse-annotation st) annotations)
        (absyn--expect-symbol st ";"))
       (t
        ;; Default element list is public.
        (let ((items (absyn--parse-element-list-until-section st)))
          (when items (push (absyn-public items) parts))))))
    (cons (absyn--reverse! parts) (absyn--reverse! annotations))))

(defun absyn--parse-element-list-until-section (st)
  (let (items)
    (while (not (or (absyn--eof-p st) (absyn--section-start-p st)))
      (push (absyn-elementitem (absyn--parse-element st)) items)
      (absyn--expect-symbol st ";")
      (absyn--skip-extra-semicolons st))
    (absyn--reverse! items)))

(defun absyn--parse-external-class-part (st)
  (let (start lang output func-name args annotation)
    (setq start (absyn--expect-keyword st "external"))
    (when (eq (absyn-token-kind (absyn--peek st)) 'string)
      (setq lang (absyn-token-value (absyn--advance-token st))))
    (when (and (eq (absyn-token-kind (absyn--peek st)) 'identifier)
               (absyn--next-symbol-p st "="))
      (setq output (absyn--parse-cref st))
      (absyn--expect-symbol st "="))
    (when (eq (absyn-token-kind (absyn--peek st)) 'identifier)
      (setq func-name (absyn--expect-ident st))
      (when (absyn--accept-symbol st "(")
        (unless (absyn--current-symbol-p st ")")
          (push (absyn--parse-expression st) args)
          (while (absyn--accept-symbol st ",")
            (push (absyn--parse-expression st) args)))
        (absyn--expect-symbol st ")")))
    (when (absyn--current-keyword-p st "annotation")
      (setq annotation (absyn--parse-annotation st)))
    (absyn--expect-symbol st ";")
    (absyn-external (absyn-externaldecl func-name lang output (absyn--reverse! args) annotation)
                    nil)))

;;; ---------------------------------------------------------------------------
;;; Elements and declarations
;;; ---------------------------------------------------------------------------

(defun absyn--parse-element (st)
  (let ((start (absyn--peek st))
        (final nil)
        rk
        inner-outer
        spec constrain done)
    ;; Accept common prefix orderings, e.g. `final redeclare', `redeclare final',
    ;; `inner replaceable', etc.  The AST still keeps the same Absyn fields.
    (while (not done)
      (cond
       ((and (not final) (absyn--accept-keyword st "final"))
        (setq final t))
       ((and (not rk)
             (or (absyn--current-keyword-p st "redeclare")
                 (absyn--current-keyword-p st "replaceable")))
        (setq rk (absyn--parse-redeclare-keywords-opt st)))
       ((and (not inner-outer)
             (or (absyn--current-keyword-p st "inner")
                 (absyn--current-keyword-p st "outer")))
        (setq inner-outer (absyn--parse-inner-outer st)))
       (t (setq done t))))
    (unless inner-outer
      (setq inner-outer (absyn-not-inner-outer)))
    (cond
     ((absyn--current-keyword-p st "defineunit")
      (absyn--advance-token st)
      (let ((name (absyn--expect-ident st))
            (args nil))
        (when (absyn--current-symbol-p st "(")
          (setq args (absyn--parse-named-args-parens st)))
        (absyn-defineunit name args (absyn--info st start))))
     (t
      (setq spec (absyn--parse-element-spec st rk))
      (setq constrain (absyn--parse-constrain-class-opt st))
      (absyn-element final rk inner-outer spec (absyn--info st start) constrain)))))

(defun absyn--parse-redeclare-keywords-opt (st)
  (cond
   ((absyn--accept-keyword st "redeclare")
    (if (absyn--accept-keyword st "replaceable")
        (absyn-redeclare-replaceable)
      (absyn-redeclare)))
   ((absyn--accept-keyword st "replaceable")
    (if (absyn--accept-keyword st "redeclare")
        (absyn-redeclare-replaceable)
      (absyn-replaceable)))
   (t nil)))

(defun absyn--replaceable-keywords-p (rk)
  (or (absyn-replaceable-p rk)
      (absyn-redeclare-replaceable-p rk)))

(defun absyn--parse-inner-outer (st)
  (cond
   ((absyn--accept-keyword st "inner")
    (if (absyn--accept-keyword st "outer") (absyn-inner-outer) (absyn-inner)))
   ((absyn--accept-keyword st "outer") (absyn-outer))
   (t (absyn-not-inner-outer))))

(defun absyn--parse-element-spec (st rk)
  (cond
   ((absyn--accept-keyword st "extends")
    (let ((path (absyn--parse-path st))
          (args nil)
          (ann nil))
      (when (absyn--current-symbol-p st "(")
        (setq args (absyn--parse-element-arg-list-parens st)))
      (when (absyn--current-keyword-p st "annotation")
        (setq ann (absyn--parse-annotation st)))
      (absyn-extends path args ann)))
   ((absyn--accept-keyword st "import")
    (let ((start (absyn-parser-state-last st))
          (im (absyn--parse-import-body st))
          (comment (absyn--parse-comment-opt st)))
      (absyn-import im comment (absyn--info st start))))
   ((absyn--class-start-token-p (absyn--peek st))
    (absyn-classdef (absyn--replaceable-keywords-p rk)
                    (absyn--parse-class-definition st)))
   (t
    (let* ((attrs (absyn--parse-element-attributes st))
           (ts (absyn--parse-type-spec st))
           (components (absyn--parse-component-items st)))
      (absyn-components attrs ts components)))))

(defun absyn--parse-constrain-class-opt (st)
  (when (absyn--accept-keyword st "constrainedby")
    (let ((path (absyn--parse-path st))
          args comment)
      (when (absyn--current-symbol-p st "(")
        (setq args (absyn--parse-element-arg-list-parens st)))
      (setq comment (absyn--parse-comment-opt st))
      (absyn-constrainclass (absyn-extends path args nil) comment))))

(defun absyn--parse-import-body (st)
  (cond
   ;; import A = B.C;
   ((and (eq (absyn-token-kind (absyn--peek st)) 'identifier)
         (absyn--next-symbol-p st "="))
    (let ((name (absyn--expect-ident st)))
      (absyn--expect-symbol st "=")
      (absyn-named-import name (absyn--parse-path st))))
   (t
    (let ((prefix (absyn--parse-path st)))
      (cond
       ;; import A.B.*;
       ((and (absyn--accept-symbol st ".")
             (absyn--accept-symbol st "*"))
        (absyn-unqual-import prefix))
       ;; import A.B.{C, D = E};
       ((absyn--accept-symbol st ".")
        (if (absyn--accept-symbol st "{")
            (let (groups)
              (unless (absyn--current-symbol-p st "}")
                (push (absyn--parse-group-import st) groups)
                (while (absyn--accept-symbol st ",")
                  (push (absyn--parse-group-import st) groups)))
              (absyn--expect-symbol st "}")
              (absyn-group-import prefix (absyn--reverse! groups)))
          (absyn--err st "Expected * or group import after .")))
       (t (absyn-qual-import prefix)))))))

(defun absyn--parse-group-import (st)
  (let ((first (absyn--expect-ident st)))
    (if (absyn--accept-symbol st "=")
        (absyn-group-import-rename first (absyn--expect-ident st))
      (absyn-group-import-name first))))

(defun absyn--parse-element-attributes (st)
  (let ((flow nil)
        (stream nil)
        (parallelism (absyn-non-parallel))
        (variability (absyn-var))
        (direction (absyn-bidir))
        (is-field (absyn-nonfield))
        (is-mode nil)
        done)
    (while (not done)
      (cond
       ((absyn--accept-keyword st "flow") (setq flow t))
       ((absyn--accept-keyword st "stream") (setq stream t))
       ((absyn--accept-keyword st "parglobal") (setq parallelism (absyn-parglobal)))
       ((absyn--accept-keyword st "parlocal") (setq parallelism (absyn-parlocal)))
       ((absyn--accept-keyword st "discrete") (setq variability (absyn-discrete)))
       ((absyn--accept-keyword st "parameter") (setq variability (absyn-param)))
       ((absyn--accept-keyword st "constant") (setq variability (absyn-const)))
       ((absyn--accept-keyword st "input")
        (setq direction (if (absyn--accept-keyword st "output") (absyn-input-output) (absyn-input))))
       ((absyn--accept-keyword st "output") (setq direction (absyn-output)))
       ((absyn--accept-keyword st "field") (setq is-field (absyn-field)))
       ((absyn--accept-keyword st "mode") (setq is-mode t))
       (t (setq done t))))
    (absyn-attr flow stream parallelism variability direction is-field nil is-mode)))

(defun absyn--parse-component-items (st)
  (let (items)
    (push (absyn--parse-component-item st) items)
    (while (absyn--accept-symbol st ",")
      (push (absyn--parse-component-item st) items))
    (absyn--reverse! items)))

(defun absyn--parse-component-item (st)
  (let* ((name (absyn--expect-ident st))
         (array-dim (absyn--parse-array-dim-opt st))
         modification condition comment)
    (when (or (absyn--current-symbol-p st "(")
              (absyn--current-symbol-p st "=")
              (absyn--current-symbol-p st ":="))
      (setq modification (absyn--parse-modification st)))
    (when (absyn--accept-keyword st "if")
      (setq condition (absyn--parse-expression st)))
    (setq comment (absyn--parse-comment-opt st))
    (absyn-componentitem (absyn-component name array-dim modification) condition comment)))

(defun absyn--parse-type-spec (st)
  (let ((path (absyn--parse-path st))
        type-specs array-dim)
    (when (absyn--accept-symbol st "<")
      (unless (absyn--current-symbol-p st ">")
        (push (absyn--parse-type-spec st) type-specs)
        (while (absyn--accept-symbol st ",")
          (push (absyn--parse-type-spec st) type-specs)))
      (absyn--expect-symbol st ">"))
    (setq array-dim (absyn--parse-array-dim-opt st))
    (if type-specs
        (absyn-tcomplex path (absyn--reverse! type-specs) array-dim)
      (absyn-tpath path array-dim))))

(defun absyn--parse-array-dim-opt (st)
  (when (absyn--current-symbol-p st "[")
    (absyn--parse-subscripts st)))

;;; ---------------------------------------------------------------------------
;;; Modifications, comments, annotations
;;; ---------------------------------------------------------------------------

(defun absyn--parse-modification (st)
  (let ((args nil)
        (eqmod (absyn-nomod))
        start)
    (when (absyn--current-symbol-p st "(")
      (setq args (absyn--parse-element-arg-list-parens st)))
    (when (or (absyn--current-symbol-p st "=")
              (absyn--current-symbol-p st ":="))
      (setq start (absyn--advance-token st))
      (setq eqmod (absyn-eqmod (absyn--parse-expression st) (absyn--info st start))))
    (absyn-classmod args eqmod)))

(defun absyn--parse-element-arg-list-parens (st)
  (let (args)
    (absyn--expect-symbol st "(")
    (unless (absyn--current-symbol-p st ")")
      (push (absyn--parse-element-arg st) args)
      (while (absyn--accept-symbol st ",")
        (push (absyn--parse-element-arg st) args)))
    (absyn--expect-symbol st ")")
    (absyn--reverse! args)))

(defun absyn--parse-element-arg (st)
  (let ((start (absyn--peek st))
        (final nil)
        (each (absyn-non-each))
        rk)
    ;; Modelica permits `each final' and `final each'; accept both.
    (while (or (absyn--current-keyword-p st "each")
               (absyn--current-keyword-p st "final"))
      (cond
       ((absyn--accept-keyword st "each") (setq each (absyn-each)))
       ((absyn--accept-keyword st "final") (setq final t))))
    (setq rk (absyn--parse-redeclare-keywords-opt st))
    (if rk
        (let* ((spec (absyn--parse-element-spec st rk))
               (constrain (absyn--parse-constrain-class-opt st)))
          (absyn-redeclaration final rk each spec constrain (absyn--info st start)))
      (let* ((path (absyn--parse-path st))
             (mod (when (or (absyn--current-symbol-p st "(")
                            (absyn--current-symbol-p st "=")
                            (absyn--current-symbol-p st ":="))
                    (absyn--parse-modification st)))
             (comment (when (eq (absyn-token-kind (absyn--peek st)) 'string)
                        (absyn-token-value (absyn--advance-token st)))))
        (absyn-modification final each path mod comment (absyn--info st start))))))

(defun absyn--parse-named-args-parens (st)
  (let (args)
    (absyn--expect-symbol st "(")
    (unless (absyn--current-symbol-p st ")")
      (push (absyn--parse-named-arg st) args)
      (while (absyn--accept-symbol st ",")
        (push (absyn--parse-named-arg st) args)))
    (absyn--expect-symbol st ")")
    (absyn--reverse! args)))

(defun absyn--parse-named-arg (st)
  (let ((name (absyn--expect-ident st)))
    (absyn--expect-symbol st "=")
    (absyn-namedarg name (absyn--parse-expression st))))

(defun absyn--parse-comment-opt (st)
  (let (str ann)
    (when (eq (absyn-token-kind (absyn--peek st)) 'string)
      (setq str (absyn-token-value (absyn--advance-token st))))
    (when (absyn--current-keyword-p st "annotation")
      (setq ann (absyn--parse-annotation st)))
    (when (or str ann)
      (absyn-comment ann str))))

(defun absyn--parse-annotation (st)
  (absyn--expect-keyword st "annotation")
  (absyn-annotation (absyn--parse-element-arg-list-parens st)))

;;; ---------------------------------------------------------------------------
;;; Equations
;;; ---------------------------------------------------------------------------

(defun absyn--class-section-stop-p (st)
  (let ((tok (absyn--peek st)))
    (and (eq (absyn-token-kind tok) 'keyword)
         (member (absyn-token-value tok)
                 '("public" "protected" "equation" "algorithm" "initial" "external" "annotation" "end")))))

(defun absyn--parse-equation-items-until-class-section (st)
  (absyn--parse-equation-items-until st #'absyn--class-section-stop-p))

(defun absyn--parse-equation-items-until (st stop-fn)
  (let (items)
    (while (not (or (absyn--eof-p st) (funcall stop-fn st)))
      (push (absyn--parse-equation-item st) items)
      (absyn--skip-extra-semicolons st))
    (absyn--reverse! items)))

(defun absyn--equation-branch-stop-p (st)
  (let ((tok (absyn--peek st)))
    (and (eq (absyn-token-kind tok) 'keyword)
         (member (absyn-token-value tok) '("elseif" "else" "elsewhen" "end")))))

(defun absyn--parse-equation-item (st)
  (let ((start (absyn--peek st)) eq comment)
    (setq eq (absyn--parse-equation st))
    (setq comment (absyn--parse-comment-opt st))
    (absyn--expect-symbol st ";")
    (absyn-equationitem eq comment (absyn--info st start))))

(defun absyn--parse-equation (st)
  (cond
   ((absyn--accept-keyword st "if") (absyn--parse-eq-if-after-if st))
   ((absyn--accept-keyword st "for") (absyn--parse-eq-for-after-for st))
   ((absyn--accept-keyword st "when") (absyn--parse-eq-when-after-when st))
   ((absyn--accept-keyword st "connect") (absyn--parse-eq-connect-after-connect st))
   ((absyn--accept-keyword st "failure") (absyn--parse-eq-failure-after-failure st))
   ((absyn--accept-keyword st "reconfigure") (absyn--parse-eq-reconfigure-after-keyword st))
   (t (absyn--parse-simple-equation st))))

(defun absyn--parse-eq-if-after-if (st)
  (let ((cond (absyn--parse-expression st))
        true-items elseif-items else-items)
    (absyn--expect-keyword st "then")
    (setq true-items (absyn--parse-equation-items-until st #'absyn--equation-branch-stop-p))
    (while (absyn--accept-keyword st "elseif")
      (let ((e (absyn--parse-expression st)))
        (absyn--expect-keyword st "then")
        (push (list e (absyn--parse-equation-items-until st #'absyn--equation-branch-stop-p)) elseif-items)))
    (when (absyn--accept-keyword st "else")
      (setq else-items (absyn--parse-equation-items-until st #'absyn--equation-branch-stop-p)))
    (absyn--expect-keyword st "end")
    (absyn--expect-keyword st "if")
    (absyn-eq-if cond true-items (absyn--reverse! elseif-items) else-items)))

(defun absyn--parse-eq-for-after-for (st)
  (let ((iters (absyn--parse-for-iterators st)))
    (absyn--expect-keyword st "loop")
    (let ((body (absyn--parse-equation-items-until st (lambda (s) (absyn--current-keyword-p s "end")))))
      (absyn--expect-keyword st "end")
      (absyn--expect-keyword st "for")
      (absyn-eq-for iters body))))

(defun absyn--parse-eq-when-after-when (st)
  (let ((cond (absyn--parse-expression st))
        body elsewhens)
    (absyn--expect-keyword st "then")
    (setq body (absyn--parse-equation-items-until st #'absyn--equation-branch-stop-p))
    (while (absyn--accept-keyword st "elsewhen")
      (let ((e (absyn--parse-expression st)))
        (absyn--expect-keyword st "then")
        (push (list e (absyn--parse-equation-items-until st #'absyn--equation-branch-stop-p)) elsewhens)))
    (absyn--expect-keyword st "end")
    (absyn--expect-keyword st "when")
    (absyn-eq-when-e cond body (absyn--reverse! elsewhens))))

(defun absyn--parse-eq-connect-after-connect (st)
  (absyn--expect-symbol st "(")
  (let ((c1 (absyn--parse-cref st)))
    (absyn--expect-symbol st ",")
    (let ((c2 (absyn--parse-cref st)))
      (absyn--expect-symbol st ")")
      (absyn-eq-connect c1 c2))))

(defun absyn--parse-eq-failure-after-failure (st)
  (absyn--expect-symbol st "(")
  (let ((item (absyn--parse-equation-item st)))
    (absyn--expect-symbol st ")")
    (absyn-eq-failure item)))

(defun absyn--parse-eq-reconfigure-after-keyword (st)
  "Parse a permissive reconfigure block.
Recognized clauses are:
  variables ... end variables;
  upon EXPR [ensure|invariant|constraint EXPR];
  prompt EXPR;
  modification/equation ...
  initial equation ...
  end reconfigure
The parser maps them to EQ_RECONFIGURE fields."
  (let (variables whens prompt modification initial-equations)
    (while (not (or (absyn--eof-p st)
                    (and (absyn--current-keyword-p st "end")
                         (absyn--token-matches-p (absyn--peek st 1) 'keyword "reconfigure"))))
      (cond
       ((absyn--accept-keyword st "variables")
        (setq variables (append variables (absyn--parse-element-list-until
                                           st
                                           (lambda (s) (absyn--current-keyword-p s "end")))))
        (absyn--expect-keyword st "end")
        (absyn--expect-keyword st "variables")
        (absyn--expect-symbol st ";"))
       ((absyn--accept-keyword st "upon")
        (let ((condition (absyn--parse-expression st)) constraint)
          (when (or (absyn--accept-keyword st "ensure")
                    (absyn--accept-keyword st "invariant")
                    (absyn--accept-keyword st "constraint"))
            (setq constraint (absyn--parse-expression st)))
          (push (if constraint
                    (absyn-when-conditional condition constraint)
                  (absyn-when-trigger condition))
                whens)
          (absyn--expect-symbol st ";")))
       ((absyn--accept-keyword st "prompt")
        (setq prompt (absyn--parse-expression st))
        (absyn--expect-symbol st ";"))
       ((or (absyn--accept-keyword st "modification")
            (absyn--accept-keyword st "equation"))
        (setq modification
              (absyn--parse-equation-items-until
               st
               (lambda (s)
                 (or (absyn--current-keyword-p s "initial")
                     (and (absyn--current-keyword-p s "end")
                          (absyn--token-matches-p (absyn--peek s 1) 'keyword "reconfigure")))))))
       ((absyn--accept-keyword st "initial")
        (absyn--expect-keyword st "equation")
        (setq initial-equations
              (absyn--parse-equation-items-until
               st
               (lambda (s)
                 (and (absyn--current-keyword-p s "end")
                      (absyn--token-matches-p (absyn--peek s 1) 'keyword "reconfigure"))))))
       (t (absyn--err st "Unexpected token in reconfigure block"))))
    (absyn--expect-keyword st "end")
    (absyn--expect-keyword st "reconfigure")
    (absyn-eq-reconfigure variables (absyn--reverse! whens) prompt modification initial-equations)))

(defun absyn--parse-element-list-until (st stop-fn)
  (let (items)
    (while (not (or (absyn--eof-p st) (funcall stop-fn st)))
      (push (absyn-elementitem (absyn--parse-element st)) items)
      (absyn--expect-symbol st ";")
      (absyn--skip-extra-semicolons st))
    (absyn--reverse! items)))

(defun absyn--parse-simple-equation (st)
  (let ((lhs (absyn--parse-expression st)))
    (cond
     ((absyn--accept-symbol st "=")
      (let ((rhs (absyn--parse-expression st)))
        (if (absyn--accept-keyword st "indomain")
            (absyn-eq-pde lhs rhs (absyn--parse-cref st))
          (absyn-eq-equals lhs rhs))))
     ((absyn-call-p lhs)
      (absyn-eq-noretcall (absyn-call-function lhs) (absyn-call-function-args lhs)))
     (t (absyn--err st "Expected equation assignment or no-return call")))))

;;; ---------------------------------------------------------------------------
;;; Algorithms
;;; ---------------------------------------------------------------------------

(defun absyn--parse-algorithm-items-until-class-section (st)
  (absyn--parse-algorithm-items-until st #'absyn--class-section-stop-p))

(defun absyn--parse-algorithm-items-until (st stop-fn)
  (let (items)
    (while (not (or (absyn--eof-p st) (funcall stop-fn st)))
      (push (absyn--parse-algorithm-item st) items)
      (absyn--skip-extra-semicolons st))
    (absyn--reverse! items)))

(defun absyn--algorithm-branch-stop-p (st)
  (let ((tok (absyn--peek st)))
    (and (eq (absyn-token-kind tok) 'keyword)
         (member (absyn-token-value tok) '("elseif" "else" "elsewhen" "end")))))

(defun absyn--parse-algorithm-item (st)
  (let ((start (absyn--peek st)) alg comment)
    (setq alg (absyn--parse-algorithm st))
    (setq comment (absyn--parse-comment-opt st))
    (absyn--expect-symbol st ";")
    (absyn-algorithmitem alg comment (absyn--info st start))))

(defun absyn--parse-algorithm (st)
  (cond
   ((absyn--accept-keyword st "if") (absyn--parse-alg-if-after-if st))
   ((absyn--accept-keyword st "for") (absyn--parse-alg-for-after-for st nil))
   ((absyn--accept-keyword st "parfor") (absyn--parse-alg-for-after-for st t))
   ((absyn--accept-keyword st "while") (absyn--parse-alg-while-after-while st))
   ((absyn--accept-keyword st "when") (absyn--parse-alg-when-after-when st))
   ((absyn--accept-keyword st "return") (absyn-alg-return))
   ((absyn--accept-keyword st "break") (absyn-alg-break))
   ((absyn--accept-keyword st "continue") (absyn-alg-continue))
   ((absyn--accept-keyword st "try") (absyn--parse-alg-try-after-try st))
   ((absyn--accept-keyword st "failure") (absyn--parse-alg-failure-after-failure st))
   (t (absyn--parse-simple-algorithm st))))

(defun absyn--parse-alg-if-after-if (st)
  (let ((cond (absyn--parse-expression st)) true elseif else)
    (absyn--expect-keyword st "then")
    (setq true (absyn--parse-algorithm-items-until st #'absyn--algorithm-branch-stop-p))
    (while (absyn--accept-keyword st "elseif")
      (let ((e (absyn--parse-expression st)))
        (absyn--expect-keyword st "then")
        (push (list e (absyn--parse-algorithm-items-until st #'absyn--algorithm-branch-stop-p)) elseif)))
    (when (absyn--accept-keyword st "else")
      (setq else (absyn--parse-algorithm-items-until st #'absyn--algorithm-branch-stop-p)))
    (absyn--expect-keyword st "end")
    (absyn--expect-keyword st "if")
    (absyn-alg-if cond true (absyn--reverse! elseif) else)))

(defun absyn--parse-alg-for-after-for (st parp)
  (let ((iters (absyn--parse-for-iterators st)))
    (absyn--expect-keyword st "loop")
    (let ((body (absyn--parse-algorithm-items-until st (lambda (s) (absyn--current-keyword-p s "end")))))
      (absyn--expect-keyword st "end")
      (absyn--expect-keyword st (if parp "parfor" "for"))
      (if parp (absyn-alg-parfor iters body) (absyn-alg-for iters body)))))

(defun absyn--parse-alg-while-after-while (st)
  (let ((cond (absyn--parse-expression st)))
    (absyn--expect-keyword st "loop")
    (let ((body (absyn--parse-algorithm-items-until st (lambda (s) (absyn--current-keyword-p s "end")))))
      (absyn--expect-keyword st "end")
      (absyn--expect-keyword st "while")
      (absyn-alg-while cond body))))

(defun absyn--parse-alg-when-after-when (st)
  (let ((cond (absyn--parse-expression st)) body elsewhens)
    (absyn--expect-keyword st "then")
    (setq body (absyn--parse-algorithm-items-until st #'absyn--algorithm-branch-stop-p))
    (while (absyn--accept-keyword st "elsewhen")
      (let ((e (absyn--parse-expression st)))
        (absyn--expect-keyword st "then")
        (push (list e (absyn--parse-algorithm-items-until st #'absyn--algorithm-branch-stop-p)) elsewhens)))
    (absyn--expect-keyword st "end")
    (absyn--expect-keyword st "when")
    (absyn-alg-when-a cond body (absyn--reverse! elsewhens))))

(defun absyn--parse-alg-try-after-try (st)
  (let ((body (absyn--parse-algorithm-items-until st #'absyn--algorithm-branch-stop-p)) else-body)
    (when (absyn--accept-keyword st "else")
      (setq else-body (absyn--parse-algorithm-items-until st #'absyn--algorithm-branch-stop-p)))
    (absyn--expect-keyword st "end")
    (absyn--expect-keyword st "try")
    (absyn-alg-try body else-body)))

(defun absyn--parse-alg-failure-after-failure (st)
  (absyn--expect-symbol st "(")
  (let (items)
    (unless (absyn--current-symbol-p st ")")
      (push (absyn--parse-algorithm-item st) items)
      (while (not (absyn--current-symbol-p st ")"))
        (push (absyn--parse-algorithm-item st) items)))
    (absyn--expect-symbol st ")")
    (absyn-alg-failure (absyn--reverse! items))))

(defun absyn--parse-simple-algorithm (st)
  (let ((lhs (absyn--parse-expression st)))
    (cond
     ((absyn--accept-symbol st ":=")
      (absyn-alg-assign lhs (absyn--parse-expression st)))
     ((absyn-call-p lhs)
      (absyn-alg-noretcall (absyn-call-function lhs) (absyn-call-function-args lhs)))
     (t (absyn--err st "Expected algorithm assignment or no-return call")))))

;;; ---------------------------------------------------------------------------
;;; Iterators
;;; ---------------------------------------------------------------------------

(defun absyn--parse-for-iterators (st)
  (let (iters)
    (push (absyn--parse-for-iterator st) iters)
    (while (absyn--accept-symbol st ",")
      (push (absyn--parse-for-iterator st) iters))
    (absyn--reverse! iters)))

(defun absyn--parse-for-iterator (st)
  (let ((name (absyn--expect-ident st)) guard range)
    (when (absyn--accept-keyword st "in")
      (setq range (absyn--parse-expression st)))
    (when (absyn--accept-keyword st "guard")
      (setq guard (absyn--parse-expression st)))
    (absyn-iterator name guard range)))

;;; ---------------------------------------------------------------------------
;;; Expressions
;;; ---------------------------------------------------------------------------

(defun absyn--parse-expression (st)
  (absyn--parse-if-expression st))

(defun absyn--parse-if-expression (st)
  (if (absyn--accept-keyword st "if")
      (let ((cond (absyn--parse-expression st)) elseif then else)
        (absyn--expect-keyword st "then")
        (setq then (absyn--parse-expression st))
        (while (absyn--accept-keyword st "elseif")
          (let ((e (absyn--parse-expression st)))
            (absyn--expect-keyword st "then")
            (push (list e (absyn--parse-expression st)) elseif)))
        (absyn--expect-keyword st "else")
        (setq else (absyn--parse-expression st))
        (absyn-ifexp cond then else (absyn--reverse! elseif)))
    (absyn--parse-range-expression st)))

(defun absyn--parse-range-expression (st)
  (let ((start (absyn--parse-or-expression st)) step stop)
    (if (absyn--accept-symbol st ":")
        (progn
          (setq stop (absyn--parse-or-expression st))
          (when (absyn--accept-symbol st ":")
            (setq step stop)
            (setq stop (absyn--parse-or-expression st)))
          (absyn-range start step stop))
      start)))

(defun absyn--parse-or-expression (st)
  (let ((e (absyn--parse-and-expression st)))
    (while (absyn--accept-keyword st "or")
      (setq e (absyn-lbinary e (absyn-or) (absyn--parse-and-expression st))))
    e))

(defun absyn--parse-and-expression (st)
  (let ((e (absyn--parse-relation-expression st)))
    (while (absyn--accept-keyword st "and")
      (setq e (absyn-lbinary e (absyn-and) (absyn--parse-relation-expression st))))
    e))

(defun absyn--relation-op (s)
  (pcase s
    ("<" (absyn-less))
    ("<=" (absyn-lesseq))
    (">" (absyn-greater))
    (">=" (absyn-greatereq))
    ("==" (absyn-equal))
    ("<>" (absyn-nequal))
    (_ nil)))

(defun absyn--parse-relation-expression (st)
  (let ((e (absyn--parse-add-expression st)) op)
    (while (setq op (and (eq (absyn-token-kind (absyn--peek st)) 'symbol)
                         (absyn--relation-op (absyn-token-value (absyn--peek st)))))
      (absyn--advance-token st)
      (setq e (absyn-relation e op (absyn--parse-add-expression st))))
    e))

(defun absyn--add-op (s unaryp)
  (pcase s
    ("+" (if unaryp (absyn-uplus) (absyn-add)))
    ("-" (if unaryp (absyn-uminus) (absyn-sub)))
    (".+" (if unaryp (absyn-uplus-ew) (absyn-add-ew)))
    (".-" (if unaryp (absyn-uminus-ew) (absyn-sub-ew)))
    (_ nil)))

(defun absyn--mul-op (s)
  (pcase s
    ("*" (absyn-mul))
    ("/" (absyn-div))
    (".*" (absyn-mul-ew))
    ("./" (absyn-div-ew))
    (_ nil)))

(defun absyn--pow-op (s)
  (pcase s
    ("^" (absyn-pow))
    (".^" (absyn-pow-ew))
    (_ nil)))

(defun absyn--parse-add-expression (st)
  (let ((e (absyn--parse-mul-expression st)) op)
    (while (setq op (and (eq (absyn-token-kind (absyn--peek st)) 'symbol)
                         (absyn--add-op (absyn-token-value (absyn--peek st)) nil)))
      (absyn--advance-token st)
      (setq e (absyn-binary e op (absyn--parse-mul-expression st))))
    e))

(defun absyn--parse-mul-expression (st)
  (let ((e (absyn--parse-power-expression st)) op)
    (while (setq op (and (eq (absyn-token-kind (absyn--peek st)) 'symbol)
                         (absyn--mul-op (absyn-token-value (absyn--peek st)))))
      (absyn--advance-token st)
      (setq e (absyn-binary e op (absyn--parse-power-expression st))))
    e))

(defun absyn--parse-power-expression (st)
  (let ((e (absyn--parse-unary-expression st))
        op)
    (if (setq op (and (eq (absyn-token-kind (absyn--peek st)) 'symbol)
                      (absyn--pow-op (absyn-token-value (absyn--peek st)))))
        (progn
          (absyn--advance-token st)
          (absyn-binary e op (absyn--parse-power-expression st)))
      e)))

(defun absyn--parse-unary-expression (st)
  (let ((tok (absyn--peek st)) op)
    (cond
     ((and (eq (absyn-token-kind tok) 'symbol)
           (setq op (absyn--add-op (absyn-token-value tok) t)))
      (absyn--advance-token st)
      (absyn-unary op (absyn--parse-unary-expression st)))
     ((absyn--accept-keyword st "not")
      (absyn-lunary (absyn-not) (absyn--parse-unary-expression st)))
     (t (absyn--parse-primary-expression st)))))

(defun absyn--parse-primary-expression (st)
  (let ((tok (absyn--peek st)))
    (pcase (absyn-token-kind tok)
      ('integer (absyn--advance-token st) (absyn-integer (absyn-token-value tok)))
      ('real (absyn--advance-token st) (absyn-real (absyn-token-value tok)))
      ('string (absyn--advance-token st) (absyn-string (absyn-token-value tok)))
      ('keyword
       (pcase (absyn-token-value tok)
         ("true" (absyn--advance-token st) (absyn-bool t))
         ("false" (absyn--advance-token st) (absyn-bool nil))
         ("end" (absyn--advance-token st) (absyn-end))
         ;; Built-in operators that are keyword-like in some grammars but appear
         ;; syntactically as calls in expressions, e.g. der(x) and initial().
         ((or "der" "initial")
          (let ((name (absyn-token-value tok)))
            (absyn--advance-token st)
            (if (absyn--current-symbol-p st "(")
                (absyn-call (absyn-cref-ident name nil) (absyn--parse-function-args st) nil)
              (absyn--err st "Expected call syntax after %s" name))))
         (_ (absyn--err st "Unexpected keyword in expression"))))
      ('symbol
       (cond
        ((absyn--current-symbol-p st "(") (absyn--parse-parenthesized-expression st))
        ((absyn--current-symbol-p st "{") (absyn--parse-array-expression st))
        ((absyn--current-symbol-p st "[") (absyn--parse-matrix-expression st))
        ((absyn--current-symbol-p st ".") (absyn--parse-cref-or-call-expression st))
        (t (absyn--err st "Unexpected symbol in expression"))))
      ('identifier (absyn--parse-cref-or-call-expression st))
      (_ (absyn--err st "Expected expression")))))

(defun absyn--parse-parenthesized-expression (st)
  (let (exps)
    (absyn--expect-symbol st "(")
    (if (absyn--accept-symbol st ")")
        (absyn-tuple nil)
      (progn
        (push (absyn--parse-expression st) exps)
        (if (absyn--accept-symbol st ",")
            (progn
              (push (absyn--parse-expression st) exps)
              (while (absyn--accept-symbol st ",")
                (push (absyn--parse-expression st) exps))
              (absyn--expect-symbol st ")")
              (absyn-tuple (absyn--reverse! exps)))
          (absyn--expect-symbol st ")")
          (car exps))))))

(defun absyn--parse-array-expression (st)
  (let (exps)
    (absyn--expect-symbol st "{")
    (if (absyn--current-symbol-p st "}")
        (progn
          (absyn--expect-symbol st "}")
          (absyn-array nil))
      (let ((first (absyn--parse-expression st)))
        ;; Array constructor with iterators: {expr for i in range}
        ;; This follows OpenModelica's convention of representing reductions and
        ;; array comprehensions as CALL nodes with FOR_ITER_FARG arguments.
        (if (or (absyn--current-keyword-p st "for")
                (absyn--current-keyword-p st "threaded"))
            (let ((iter-type (if (absyn--accept-keyword st "threaded")
                                 (progn (absyn--expect-keyword st "for") (absyn-thread))
                               (absyn--expect-keyword st "for")
                               (absyn-combine))))
              (prog1 (absyn-call (absyn-cref-ident "array" nil)
                                  (absyn-for-iter-farg first iter-type (absyn--parse-for-iterators st))
                                  nil)
                (absyn--expect-symbol st "}")))
          (push first exps)
          (while (absyn--accept-symbol st ",")
            (push (absyn--parse-expression st) exps))
          (absyn--expect-symbol st "}")
          (absyn-array (absyn--reverse! exps)))))))

(defun absyn--parse-matrix-expression (st)
  (let (rows current)
    (absyn--expect-symbol st "[")
    (unless (absyn--current-symbol-p st "]")
      (push (absyn--parse-expression st) current)
      (while (or (absyn--accept-symbol st ",")
                 (absyn--accept-symbol st ";"))
        (if (equal (absyn-token-value (absyn-parser-state-last st)) ";")
            (progn
              (push (absyn--reverse! current) rows)
              (setq current nil))
          nil)
        (push (absyn--parse-expression st) current))
      (push (absyn--reverse! current) rows))
    (absyn--expect-symbol st "]")
    (absyn-matrix (absyn--reverse! rows))))

(defun absyn--parse-cref-or-call-expression (st)
  (let ((cr (absyn--parse-cref st)))
    (if (absyn--current-symbol-p st "(")
        (absyn-call cr (absyn--parse-function-args st) nil)
      (absyn-cref cr))))

(defun absyn--parse-function-args (st)
  (let (args named)
    (absyn--expect-symbol st "(")
    (cond
     ((absyn--accept-symbol st ")")
      (absyn-functionargs nil nil))
     ;; Named-argument-only call: f(a = 1, b = 2)
     ((and (eq (absyn-token-kind (absyn--peek st)) 'identifier)
           (absyn--next-symbol-p st "="))
      (push (absyn--parse-named-arg st) named)
      (while (absyn--accept-symbol st ",")
        (push (absyn--parse-named-arg st) named))
      (absyn--expect-symbol st ")")
      (absyn-functionargs nil (absyn--reverse! named)))
     (t
      (let ((first (absyn--parse-expression st)))
        (cond
         ;; Reduction call: f(expr for i in 1:n) or f(expr threaded for i in ...)
         ((or (absyn--current-keyword-p st "for")
              (absyn--current-keyword-p st "threaded"))
          (let ((iter-type (if (absyn--accept-keyword st "threaded")
                               (progn (absyn--expect-keyword st "for") (absyn-thread))
                             (absyn--expect-keyword st "for")
                             (absyn-combine))))
            (prog1 (absyn-for-iter-farg first iter-type (absyn--parse-for-iterators st))
              (absyn--expect-symbol st ")"))))
         (t
          (push first args)
          (while (absyn--accept-symbol st ",")
            (if (and (eq (absyn-token-kind (absyn--peek st)) 'identifier)
                     (absyn--next-symbol-p st "="))
                (push (absyn--parse-named-arg st) named)
              (push (absyn--parse-expression st) args)))
          (absyn--expect-symbol st ")")
          (absyn-functionargs (absyn--reverse! args) (absyn--reverse! named)))))))))

;;; ---------------------------------------------------------------------------
;;; Paths, component references, subscripts
;;; ---------------------------------------------------------------------------

(defun absyn--parse-path (st)
  (let ((fully nil) names)
    (when (absyn--accept-symbol st ".")
      (setq fully t))
    (push (absyn--expect-ident st) names)
    (while (and (absyn--current-symbol-p st ".")
                (eq (absyn-token-kind (absyn--peek st 1)) 'identifier))
      (absyn--expect-symbol st ".")
      (push (absyn--expect-ident st) names))
    (let ((p (absyn--names-to-path (absyn--reverse! names))))
      (if fully (absyn-fullyqualified p) p))))

(defun absyn--names-to-path (names)
  (if (null (cdr names))
      (absyn-ident (car names))
    (absyn-qualified (car names) (absyn--names-to-path (cdr names)))))

(defun absyn--parse-cref (st)
  (let ((fully nil) segments)
    (when (absyn--accept-symbol st ".")
      (setq fully t))
    (if (and (eq (absyn-token-kind (absyn--peek st)) 'identifier)
             (string= (absyn-token-value (absyn--peek st)) "_"))
        (progn (absyn--advance-token st) (absyn-wild))
      (push (list (absyn--expect-ident st) (absyn--parse-array-dim-opt st)) segments)
      (while (and (absyn--current-symbol-p st ".")
                  (eq (absyn-token-kind (absyn--peek st 1)) 'identifier))
        (absyn--expect-symbol st ".")
        (push (list (absyn--expect-ident st) (absyn--parse-array-dim-opt st)) segments))
      (let ((cr (absyn--segments-to-cref (absyn--reverse! segments))))
        (if fully (absyn-cref-fullyqualified cr) cr)))))

(defun absyn--segments-to-cref (segments)
  (let ((seg (car segments)))
    (if (null (cdr segments))
        (absyn-cref-ident (car seg) (cadr seg))
      (absyn-cref-qual (car seg) (cadr seg) (absyn--segments-to-cref (cdr segments))))))

(defun absyn--parse-subscripts (st)
  (let (subs)
    (absyn--expect-symbol st "[")
    (unless (absyn--current-symbol-p st "]")
      (push (absyn--parse-subscript st) subs)
      (while (absyn--accept-symbol st ",")
        (push (absyn--parse-subscript st) subs)))
    (absyn--expect-symbol st "]")
    (absyn--reverse! subs)))

(defun absyn--parse-subscript (st)
  (if (absyn--accept-symbol st ":")
      (absyn-nosub)
    (absyn-subscript (absyn--parse-expression st))))

;;; ---------------------------------------------------------------------------
;;; Developer helpers
;;; ---------------------------------------------------------------------------

(defun absyn-tokenize-string (text &optional file)
  "Return lexer tokens for TEXT. Useful for debugging the parser."
  (absyn-lex-string text file))

(provide 'absyn-parser)
;;; absyn-parser.el ends here
