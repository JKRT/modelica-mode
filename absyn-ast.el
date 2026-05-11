;;; absyn-ast.el --- Absyn AST record definitions for parser -*- lexical-binding: t; -*-

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
;; This file mirrors the OpenModelica Absyn uniontypes as Emacs Lisp
;; cl-defstruct records.

;; AST conventions:
;; - NONE() is represented as nil.
;; - SOME(x) is represented as x.
;; - Lists are represented as ordinary Emacs Lisp lists.
;; - Constructor names are lower-case and prefixed with `absyn-', e.g.
;;   PROGRAM(...) -> (absyn-program ...), EQ_EQUALS(...) -> (absyn-eq-equals ...).

(require 'cl-lib)

;;; ---------------------------------------------------------------------------
;;; Absyn record definitions
;;; ---------------------------------------------------------------------------

(defmacro absyn-defrecord (name slots)
  "Define a positional cl-defstruct constructor named absyn-NAME."
  (declare (indent 1))
  (let ((struct (intern (format "absyn-%s" name))))
    `(cl-defstruct (,struct (:constructor ,struct ,slots)
                            (:copier nil))
       ,@slots)))

(absyn-defrecord source-info (file-name is-read-only start-line start-column end-line end-column))

;; ForIterator
(absyn-defrecord iterator (name guard-exp range))

;; Program / Within
(absyn-defrecord program (classes within))
(absyn-defrecord within (path))
(absyn-defrecord top ())

;; Class / ClassDef
(absyn-defrecord class (name partial-prefix final-prefix encapsulated-prefix restriction body info))
(absyn-defrecord parts (type-vars class-attrs class-parts ann comment))
(absyn-defrecord derived (type-spec attributes arguments comment))
(absyn-defrecord enumeration (enum-literals comment))
(absyn-defrecord overload (function-names comment))
(absyn-defrecord class-extends (base-class-name modifications comment parts ann))
(absyn-defrecord pder (function-name vars comment))

;; TypeSpec / Enum
(absyn-defrecord tpath (path array-dim))
(absyn-defrecord tcomplex (path type-specs array-dim))
(absyn-defrecord enumliterals (enum-literals))
(absyn-defrecord enum-colon ())
(absyn-defrecord enumliteral (literal comment))

;; ClassPart / ElementItem / Element
(absyn-defrecord public (contents))
(absyn-defrecord protected (contents))
(absyn-defrecord constraints (contents))
(absyn-defrecord equations (contents))
(absyn-defrecord initialequations (contents))
(absyn-defrecord algorithms (contents))
(absyn-defrecord initialalgorithms (contents))
(absyn-defrecord external (external-decl annotation))
(absyn-defrecord elementitem (element))
(absyn-defrecord lexer-comment (comment))
(absyn-defrecord element (final-prefix redeclare-keywords inner-outer specification info constrain-class))
(absyn-defrecord defineunit (name args info))
(absyn-defrecord text (opt-name string info))
(absyn-defrecord constrainclass (element-spec comment))

;; ElementSpec / InnerOuter / Import
(absyn-defrecord classdef (replaceable class))
(absyn-defrecord extends (path element-arg annotation-opt))
(absyn-defrecord import (import comment info))
(absyn-defrecord components (attributes type-spec components))
(absyn-defrecord inner ())
(absyn-defrecord outer ())
(absyn-defrecord inner-outer ())
(absyn-defrecord not-inner-outer ())
(absyn-defrecord named-import (name path))
(absyn-defrecord qual-import (path))
(absyn-defrecord unqual-import (path))
(absyn-defrecord group-import (prefix groups))
(absyn-defrecord group-import-name (name))
(absyn-defrecord group-import-rename (rename name))

;; Components / Equations / Algorithms
(absyn-defrecord componentitem (component condition comment))
(absyn-defrecord component (name array-dim modification))
(absyn-defrecord equationitem (equation comment info))
(absyn-defrecord equationitemcomment (comment))
(absyn-defrecord algorithmitem (algorithm comment info))
(absyn-defrecord algorithmitemcomment (comment))

;; Equation / WhenClause
(absyn-defrecord eq-if (if-exp equation-true-items elseif-branches equation-else-items))
(absyn-defrecord eq-equals (left-side right-side))
(absyn-defrecord eq-pde (left-side right-side domain))
(absyn-defrecord eq-connect (connector1 connector2))
(absyn-defrecord eq-for (iterators for-equations))
(absyn-defrecord eq-when-e (when-exp when-equations elsewhen-equations))
(absyn-defrecord eq-noretcall (function-name function-args))
(absyn-defrecord eq-failure (equ))
(absyn-defrecord eq-reconfigure (variables when-clauses prompt modification initial-equations))
(absyn-defrecord when-trigger (condition))
(absyn-defrecord when-conditional (condition constraint))

;; Algorithm
(absyn-defrecord alg-assign (assign-component value))
(absyn-defrecord alg-if (if-exp true-branch elseif-algorithm-branch else-branch))
(absyn-defrecord alg-for (iterators for-body))
(absyn-defrecord alg-parfor (iterators parfor-body))
(absyn-defrecord alg-while (bool-expr while-body))
(absyn-defrecord alg-when-a (bool-expr when-body elsewhen-algorithm-branch))
(absyn-defrecord alg-noretcall (function-call function-args))
(absyn-defrecord alg-return ())
(absyn-defrecord alg-break ())
(absyn-defrecord alg-failure (equ))
(absyn-defrecord alg-try (body else-body))
(absyn-defrecord alg-continue ())

;; Modifications
(absyn-defrecord classmod (element-arg-lst eq-mod))
(absyn-defrecord nomod ())
(absyn-defrecord eqmod (exp info))
(absyn-defrecord modification (final-prefix each-prefix path modification comment info))
(absyn-defrecord redeclaration (final-prefix redeclare-keywords each-prefix element-spec constrain-class info))
(absyn-defrecord redeclare ())
(absyn-defrecord replaceable ())
(absyn-defrecord redeclare-replaceable ())
(absyn-defrecord each ())
(absyn-defrecord non-each ())

;; Attributes
(absyn-defrecord attr (flow-prefix stream-prefix parallelism variability direction is-field array-dim is-mode))
(absyn-defrecord nonfield ())
(absyn-defrecord field ())
(absyn-defrecord parglobal ())
(absyn-defrecord parlocal ())
(absyn-defrecord non-parallel ())
(absyn-defrecord flow ())
(absyn-defrecord stream ())
(absyn-defrecord not-flow-stream ())
(absyn-defrecord var ())
(absyn-defrecord discrete ())
(absyn-defrecord param ())
(absyn-defrecord const ())
(absyn-defrecord input ())
(absyn-defrecord output ())
(absyn-defrecord bidir ())
(absyn-defrecord input-output ())

;; Expressions / cases / code nodes
(absyn-defrecord integer (value))
(absyn-defrecord real (value))
(absyn-defrecord cref (component-ref))
(absyn-defrecord string (value))
(absyn-defrecord bool (value))
(absyn-defrecord binary (exp1 op exp2))
(absyn-defrecord unary (op exp))
(absyn-defrecord lbinary (exp1 op exp2))
(absyn-defrecord lunary (op exp))
(absyn-defrecord relation (exp1 op exp2))
(absyn-defrecord ifexp (if-exp true-branch else-branch elseif-branch))
(absyn-defrecord call (function function-args type-vars))
(absyn-defrecord partevalfunction (function function-args))
(absyn-defrecord array (array-exp))
(absyn-defrecord matrix (matrix))
(absyn-defrecord range (start step stop))
(absyn-defrecord tuple (expressions))
(absyn-defrecord end ())
(absyn-defrecord code (code))
(absyn-defrecord as (id exp))
(absyn-defrecord cons (head rest))
(absyn-defrecord matchexp (match-ty input-exp local-decls cases comment))
(absyn-defrecord list (exps))
(absyn-defrecord dot (exp index))
(absyn-defrecord subscripted-exp (exp subscripts))
(absyn-defrecord expressioncomment (comment-before exp comment-after))
(absyn-defrecord case (pattern pattern-guard pattern-info local-decls class-part result result-info comment info))
(absyn-defrecord else (local-decls class-part result result-info comment info))
(absyn-defrecord match ())
(absyn-defrecord matchcontinue ())
(absyn-defrecord c-typename (path))
(absyn-defrecord c-variablename (component-ref))
(absyn-defrecord c-constraintsection (boolean equation-item-lst))
(absyn-defrecord c-equationsection (boolean equation-item-lst))
(absyn-defrecord c-algorithmsection (boolean algorithm-item-lst))
(absyn-defrecord c-element (element))
(absyn-defrecord c-expression (exp))
(absyn-defrecord c-modification (modification))

;; FunctionArgs / NamedArg / Operators / Subscript
(absyn-defrecord functionargs (args arg-names))
(absyn-defrecord for-iter-farg (exp iter-type iterators))
(absyn-defrecord combine ())
(absyn-defrecord thread ())
(absyn-defrecord namedarg (arg-name arg-value))
(absyn-defrecord add ())
(absyn-defrecord sub ())
(absyn-defrecord mul ())
(absyn-defrecord div ())
(absyn-defrecord pow ())
(absyn-defrecord uplus ())
(absyn-defrecord uminus ())
(absyn-defrecord add-ew ())
(absyn-defrecord sub-ew ())
(absyn-defrecord mul-ew ())
(absyn-defrecord div-ew ())
(absyn-defrecord pow-ew ())
(absyn-defrecord uplus-ew ())
(absyn-defrecord uminus-ew ())
(absyn-defrecord and ())
(absyn-defrecord or ())
(absyn-defrecord not ())
(absyn-defrecord less ())
(absyn-defrecord lesseq ())
(absyn-defrecord greater ())
(absyn-defrecord greatereq ())
(absyn-defrecord equal ())
(absyn-defrecord nequal ())
(absyn-defrecord nosub ())
(absyn-defrecord subscript (subscript))

;; ComponentRef / Path
(absyn-defrecord cref-fullyqualified (component-ref))
(absyn-defrecord cref-qual (name subscripts component-ref))
(absyn-defrecord cref-ident (name subscripts))
(absyn-defrecord wild ())
(absyn-defrecord allwild ())
(absyn-defrecord qualified (name path))
(absyn-defrecord ident (name))
(absyn-defrecord fullyqualified (path))

;; Restrictions
(absyn-defrecord r-class ())
(absyn-defrecord r-optimization ())
(absyn-defrecord r-model ())
(absyn-defrecord r-record ())
(absyn-defrecord r-block ())
(absyn-defrecord r-connector ())
(absyn-defrecord r-exp-connector ())
(absyn-defrecord r-type ())
(absyn-defrecord r-package ())
(absyn-defrecord r-function (function-restriction))
(absyn-defrecord r-operator ())
(absyn-defrecord r-operator-record ())
(absyn-defrecord r-enumeration ())
(absyn-defrecord r-predefined-integer ())
(absyn-defrecord r-predefined-real ())
(absyn-defrecord r-predefined-string ())
(absyn-defrecord r-predefined-boolean ())
(absyn-defrecord r-predefined-enumeration ())
(absyn-defrecord r-predefined-clock ())
(absyn-defrecord r-uniontype ())
(absyn-defrecord r-metarecord (name index singleton moved type-vars))
(absyn-defrecord r-unknown ())
(absyn-defrecord pure ())
(absyn-defrecord impure ())
(absyn-defrecord no-purity ())
(absyn-defrecord fr-normal-function (purity))
(absyn-defrecord fr-operator-function ())
(absyn-defrecord fr-parallel-function ())
(absyn-defrecord fr-kernel-function ())

;; Annotation / comments / external / misc
(absyn-defrecord annotation (element-args))
(absyn-defrecord comment (annotation comment))
(absyn-defrecord externaldecl (func-name lang output args annotation))
(absyn-defrecord rcr (cr))
(absyn-defrecord rts (ts))
(absyn-defrecord rim (im))
(absyn-defrecord msg (info))
(absyn-defrecord no-msg ())

(defconst absyn-empty-function-args (absyn-functionargs nil nil))

;; ---------------------------------------------------------------------------
;; Public interface
;; ---------------------------------------------------------------------------

(provide 'absyn-ast)

;;; ---------------------------------------------------------------------------
