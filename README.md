
XModelicaMode
===============

This directory contains extensions for Emacs supporting Modelica.
Modelica is a unified object-oriented language for physical systems modeling
(see http://www.Modelica.org).

This code has been written for use with Emacs and shares its licensing (See COPYING).

Emacs lisp code
===============
  modelica-mode.el     -- major mode for editing Modelica files
  
Installation
============

See the files modelica-mode.el and mdc-browse.el for installation
instuctions.

At least put the files
  modelica-mode.el

to an Emacs lisp directory, e.g. ~/elisp

and add the following lines to your file ~/.emacs

;; Modelica mode
(setq load-path (cons "~/elisp" load-path))
(autoload 'modelica-mode "modelica-mode" "Modelica Editing Mode" t)
(setq auto-mode-alist (cons '("\.mo$" . modelica-mode) auto-mode-alist))

;; Enable Modelica browsing
(autoload 'mdc-browse "mdc-browse" "Modelica Class Browsing" t)
(autoload 'br-mdc "br-mdc" "Modelica Class Browsing" t)

;; Enable native .eln compilation for this package
(setq modelica-enable-native-compilation t)
;; Optional: recompile parser/mode files after saving package sources
;; (setq modelica-native-compile-on-save t)

;; Optional manual command:
;; M-x modelica-native-compile

(defvar br-env-lang-avector
  '[
    ("C++/C"   . "c++-")
    ("Eiffel"  . "eif-")
    ("Info"    . "info-")
    ("Java"    . "java-")
    ("Lisp"    . "clos-")
    ("Modelica" . "mdc-")
    ("Obj-C"   . "objc-")
    ("Python"  . "python-")
    ]
  "Association vector of elements of OO-Browser languages.") 

Modelica mode for Emacs
======================
The aim is to provide basic support as known from many programming
languages under Emacs. This includes proper indentation, automated
closing of code blocks, movement by statements and code blocks,
support for writing comments, and syntax highlighting.

Originally by

Ruediger Franke
Later additions by Dietmar Winkler

Current author/maintainers note
======================
I have updated this quite significantly since the old Modelica-mode did not work some some cases and so on.
I also removed support for XEmacs. However, this fork can be considered XModelicaMode, since it differs quite alot from
the original. It also posesses capabilities to edit and navigate in MetaModelica. An extension to the core Modelica language

Cheers! 

If there are any issues raise them here, you are a user and like this package leave a star :)  

John
