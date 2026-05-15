;;; modelica-mode-tests.el --- ERT tests for modelica-mode parsing/indentation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The modelica-mode contributors.
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'ert)

(let* ((file-dir (or (and load-file-name (file-name-directory load-file-name))
                     default-directory))
       (project-root (file-name-directory (directory-file-name file-dir))))
  (add-to-list 'load-path project-root)
  (load (expand-file-name "modelica-mode.el" project-root) nil t))

(defmacro modelica-mode-test--with-buffer (code &rest body)
  `(with-temp-buffer
     (insert ,code)
     (modelica-mode)
     (goto-char (point-min))
     ,@body))

(defun modelica-mode-test--indent-string (code)
  "Indent CODE in a temporary `modelica-mode' buffer and return it."
  (modelica-mode-test--with-buffer code
    (indent-region (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest modelica-mode-test-parse-error-message-unterminated-string ()
  (let ((err (should-error
              (modelica-mode-parse-string "within A;\nmodel M\n  parameter String s = \"unterminated;\nend M;"
                                          "fixture.mo")
              :type 'absyn-parse-error)))
    (should (string-match-p "Unterminated string literal" (cadr err)))
    (should err)))

(ert-deftest modelica-mode-test-parse-error-message-unexpected-char ()
  (let ((err (should-error
              (absyn-parse-string "model M\n  equation\n    x := 1&;\nend M;")
              :type 'absyn-parse-error)))
    (should (string-match-p "Unexpected character" (cadr err)))
    (should (string-match-p "38" (cadr err)))))

(ert-deftest modelica-mode-test-check-buffer-jumps-to-parse-error ()
  (modelica-mode-test--with-buffer
      "model M
equation
  x := 1&;
end M;
"
    (let ((ok (modelica-check-buffer)))
      (should-not ok)
      (should (= (line-number-at-pos) 3))
      (should (= (1+ (current-column)) 9)))))

(ert-deftest modelica-mode-test-check-buffer-success ()
  (modelica-mode-test--with-buffer
      "model M
equation
  x = 1;
end M;
"
    (should (modelica-check-buffer))))

(ert-deftest modelica-mode-test-parser-indent-context ()
  (modelica-mode-test--with-buffer
      "within A;\nmodel M\n  parameter Real x = 1;\n  equation\n    x = 1;\n  end M;\n"
    (re-search-forward "parameter Real x = 1;")
    (beginning-of-line)
    (should (eq (modelica-mode-parser-indent-type) 'declaration))
    (re-search-forward "equation")
    (forward-line 1)
    (beginning-of-line)
    (should (eq (modelica-mode-parser-indent-type) 'block))))

(ert-deftest modelica-mode-test-indent-fallback-on-parse-error ()
  (modelica-mode-test--with-buffer
      "model M\n  equation\n    x = 1\nend M;\n"
    ;; The parser should fail here (missing semicolon), so parser context
    ;; should be unavailable and indentation should still return from fallback.
    (goto-char (point-max))
    (should-not (modelica-mode-parser-indent-type))
    (let ((indent (modelica-calculate-indent)))
      (should (listp indent))
      (should (= 2 (length indent)))
      (should (integerp (cadr indent))))))

(ert-deftest modelica-mode-test-indent-model-if-equation ()
  (should
   (equal
    (modelica-mode-test--indent-string
     "model A
equation
if x then
y = 1;
else
y = 2;
end if;
end A;
")
    "model A
equation
  if x then
    y = 1;
  else
    y = 2;
  end if;
end A;
")))

(ert-deftest modelica-mode-test-indent-function-match ()
  (should
   (equal
    (modelica-mode-test--indent-string
     "function f
input Real x;
output Real y;
algorithm
y := match x
case 1 then 2;
else 3;
end match;
end f;
")
    "function f
  input Real x;
  output Real y;
algorithm
  y := match x
    case 1 then 2;
    else 3;
  end match;
end f;
")))

(ert-deftest modelica-mode-test-indent-uniontype-record ()
  (should
   (equal
    (modelica-mode-test--indent-string
     "uniontype T
record R
Integer x;
end R;
end T;
")
    "uniontype T
  record R
    Integer x;
  end R;
end T;
")))

(provide 'modelica-mode-tests)
