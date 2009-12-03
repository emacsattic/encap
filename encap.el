;;; encap.el --- Code to convert sexps to independent functions etc

;; Copyright (C) 2000 by Tom Breton

;; Author: Tom Breton <tob@world.std.com>
;; Keywords: lisp, maint, extensions, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code lets you excerpt list-forms in a few keystrokes, without
;; (much) chance of changing their logic.  

;; NB, some of the weirder stuff, like unquoted lambda forms in
;; macros, can still be altered in small ways.

;; Also NB, it is important to properly tell function-types (defuns,
;; etc) what should be a parm and what shouldn't.

;; We automatically remove any redundant surrounding (progn ... ) when
;; building a function.  This lets the user build a progn surrounding
;; more than one expression and then encap it.


;; The use of insert-parentheses or tehom-insert-parentheses is
;; recommended with this, to cut your keystrokes down to a bare
;; minimum with numeric prefix args

;;;; Non-features

;; A variant could grab the next/previous N sexps into a progn or
;; list.  That requires much more understanding of the code, and
;; co-ordination as to which types we may produce.  It makes more
;; sense to just do it by hand.  

;; Instead of assuming only leaves may be parms, we could assume that
;; any form may be a literal.  But building the interaction for that
;; is much harder, and much harder for the user to navigate.
   
;; A variant, or just another def-type, could write the definition
;; into an enclosing let statement (let*, flet, etc as appropriate).
;; This requires searching, using backward-up-list a lot.
   
;; Another variant could read it from a let statement, understanding
;; the form as different than forms inside progn's etc.

;; Could walk the code better, not collecting "arguments" that are
;; defined intermediately, eg in a let form.  In its pure form. this
;; would involve expanding macros.  

;; It could also not quote lambdas-transformed-to-calls when they
;; shouldn't be quoted, eg in some macros.  Again, would require
;; code-walking and macro expansion.

;; It would be quite difficult to support macroizing list-forms, since
;; it's not clear what could and couldn't be args.

;; One approach is that the user could transform the whole thing into
;; an (eval `()) form (or instead of backquote, build lists, etc) and
;; then we could strip off the eval and transform it into a macro.
;; That only requires that the user write "eval" and "`", and commas
;; as needed, and surround forms that should spread out with ,@'(...)
;; It can pretty much be done in place with a few numeric arguments.

;;; Code:

(require 'local-vars)
(require 'cl)
(require 'tehom-cl) 
(require 'pp)
(require 'arrange) 


;;;; Configuration

;;These could vary by mode.  EG, lisp-mode has no defun*,
;;emacs-lisp-mode does.  So these should be parameters.


;;No generics are supported,
(defconst encap-defun-types
  '(defun defun* defsubst defsubst*)
  "")

(defconst encap-defconst-types
  '(defconst defconstant defvar defparameter)
  "")

(defconst encap-all-types
  (append encap-defconst-types encap-defun-types) "" )


;;;;
;;Promptable versions of the above
;;Helper
(defun encap-symbol-to-promptable (x)
  (list (symbol-name x)))

(defconst encap-defun-types-promptable
  (mapcar #'encap-symbol-to-promptable encap-defun-types)
  "" )

(defconst encap-all-types-promptable 
  (mapcar #'encap-symbol-to-promptable encap-all-types)
  "" )


;;;;;;;;;;;;;;;
;;Interaction

(defvar encap-type-history nil "")

(defun get-encap-type
  (args types-promptable)
  ""
  (let*
    ((initial-value
       (if args "defun" "defconst"))

      (type-name
	(completing-read 
	  "Definition is which type? " 
	  types-promptable
	  nil
	  t
	  initial-value 
	  encap-type-history 
	  initial-value)))

    (intern-soft type-name)))


(defun encap-pick-parms (args) 
  "Let the user rearrange the list of parms." 

  (if args
    (arrange-syms-other-window args)
    nil))


;;;;;;;;;;;;;;;
;;Predicates

(defsubst encap-defsym-is-0-arg-p
  (def-sym)
  ""
  (memq def-sym encap-defconst-types))


(defun encap-is-lambda-p
  (form)
  ""
  (and
    (consp form)
    (eq
      (car form)
      'lambda)))

(eval-when-compile
  (setf
    (get 'encap-is-lambda-p 'rtest-suite)
    '("encap-is-lambda-p"
       ("Lambda expressions give non-nil"
	 (encap-is-lambda-p '(lambda (a) a))
	 t)

       ("Quoted lambda expressions give nil.  They must be unquoted."
	 (encap-is-lambda-p '#'(lambda (a) a))
	 nil)

       ("Non lambda expressions give nil"
	 (encap-is-lambda-p '(1 2))
	 nil))))

(defun encap-is-quoted-lambda-p (form) ""
  (and
    (consp form)
    (or
      (eq
	(car form)
	'quote)
      (eq
	(car form)
	'function))
    (encap-is-lambda-p (second form))))

(eval-when-compile
  (setf
    (get 'encap-is-quoted-lambda-p 'rtest-suite)
    '("encap-is-quoted-lambda-p"
       ("Lambda expressions give non-nil"
	 (encap-is-quoted-lambda-p '(lambda (a) a))
	 nil)

       ("Quoted lambda expressions give nil.  They must be unquoted."
	 (encap-is-quoted-lambda-p '#'(lambda (a) a))
	 t)

       ("Non lambda expressions give nil"
	 (encap-is-quoted-lambda-p '(1 2))
	 nil)
       )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Various builders.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constant builder

(defun encap-build-constant-replacement (form name defname)
  "Build call and definition for a encapped constant"

  (list 
    name 
    (list defname name form "" )))

(eval-when-compile
  (setf
    (get 'encap-build-constant-replacement 'rtest-suite)
    '("encap-build-constant-replacement"
       (
	 (encap-build-constant-replacement 13 'my-13 'defvar)
	 '(my-13 (defvar my-13 13 "")))
       
       ( "Forms are used, not evalled."
	 (encap-build-constant-replacement '(+ 7 6) 'my-13 'defvar)
	 '(my-13 (defvar my-13 (+ 7 6) ""))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List-form builder


(defun encap-simplify-function-form (form) 
  "Return a simplified version of FORM as a list of forms."
  (if
    (and
      (consp form)
      (eq
	(car form)
	'progn))
    (cdr form)
    (list form)))

(eval-when-compile
  (setf
    (get 'encap-simplify-function-form 'rtest-suite)
    '("encap-simplify-function-form"

       ( "`progn's are made implicit"
	 (encap-simplify-function-form '(progn (foo)) )
	 '((foo)))
       
       ( "Forms that can't be simplified are just made into lists."
	 (encap-simplify-function-form '12 )
	 '(12))
       
       ( "Forms that were already lists are given another layer of list-ness."
	 (encap-simplify-function-form '(foo a b) )
	 '((foo a b)))

       )))


;;$$Now that constantp gives us literalness, this could merge
;;with encap-get-parms-recursive, remove that extra '() return.
(defun encap-get-parms-in-list-recursive
  (form)
  ""
  (if
    (eq
      (car form)
      'quote)
    '()
    (apply 'append
      ;;Explore heads only if they are themselves lists, eg the first
      ;;element of a `let'.
      (if
	(listp
	  (car form))
	(encap-get-parms-recursive
	  (car form)))
      (mapcar #'encap-get-parms-recursive
	(cdr form)))))

(defun encap-get-parms-recursive (form) 
  "Return a list of the non-literal atoms in FORM."

  (if (atom form)
    (if
      (constantp form)
      '()
      (list form))

    (encap-get-parms-in-list-recursive form)))



(eval-when-compile
  (setf
    (get 'encap-get-parms-recursive 'rtest-suite)
    '("encap-get-parms-recursive"
       ;;
       ( "Ignores quoted forms"
	 (encap-get-parms-recursive ''())
	 nil)

       ( "Returns a single-level list even for nested forms."
	 (encap-get-parms-recursive '(foo a (bar b)))
	 '(a b))

       ( "Collects non-heads that are symbols"
	 (encap-get-parms-recursive '(foo a))
	 '(a))

       ( "Skips non-heads that are literals."
	 (encap-get-parms-recursive '(foo 12))
	 nil)

       ( "Skip non-heads that are literals."
	 (encap-get-parms-recursive '(foo "abc" a))
	 '(a))

       ( "Skips non-heads that are self-evaluating symbols,"
	 (encap-get-parms-recursive '(foo a t))
	 '(a))

       ( "Skips non-heads that are quoted, which makes them literals."
	 (encap-get-parms-recursive '(foo 'a))
	 nil)

       ;;Vectors are assumed to be literals, even tho sometimes they
       ;;aren't.  They could be mapped on as well.

       ( "Heads that are lists, eg in let, are explored"
	 (encap-get-parms-recursive
	   '(let ((a (foo b)))))
	 '(b))
       
       )))



(defun encap-get-list-form-parms (form)
  ""

  (assert (not (encap-is-lambda-p form)))
  (remove-duplicates (encap-get-parms-recursive form)))

(eval-when-compile
  (setf
    (get 'encap-get-list-form-parms 'rtest-suite)
    '("encap-get-list-form-parms"
       ( (encap-get-list-form-parms '(foo a a a))
	 '(a)
         )
       )))



(defun encap-build-list-form-replacement (raw-form name defname args)
  "Build call and definition for a encapped function"

  (assert (not (encap-is-lambda-p raw-form)))

  (let
    ( (actual-args (encap-pick-parms args))
      (form (encap-simplify-function-form raw-form)))

    (list
      (list* name actual-args)
      (list* defname name actual-args "" form))))

(eval-when-compile
  (setf
    (get 'encap-build-list-form-replacement 'rtest-suite)
    '("encap-build-list-form-replacement"

       ( (let-defalias
	   encap-pick-parms identity
	   (encap-build-list-form-replacement
	     '(foo a b) 'my-foo 'defun '()))
	 
	 '((my-foo) (defun my-foo nil "" (foo a b))))
       
       ( 
	 (let-defalias
	   encap-pick-parms identity
	   (encap-build-list-form-replacement
	     '(foo a b) 'my-foo 'defun '(a)))
	 '((my-foo a) (defun my-foo (a) "" (foo a b))))
       
       ( "A redundant progn, if any, is stripped out"
	 (let-defalias
	   encap-pick-parms identity
	   (encap-build-list-form-replacement
	     '(progn (foo a b)) 'my-foo 'defun '()))
	 '((my-foo) (defun my-foo nil "" (foo a b))))

       )))

(defun encap-build-nonlambda-replacement
  (form name)
  ""
  (let*
    (
      (args
	(encap-get-list-form-parms form))
      (def-sym
	(get-encap-type args encap-all-types-promptable)))


    (if (encap-defsym-is-0-arg-p def-sym)
      (encap-build-constant-replacement  form name def-sym)
      (encap-build-list-form-replacement form name def-sym args))))

(eval-when-compile
  (setf
    (get 'encap-build-nonlambda-replacement 'rtest-suite)
    '("encap-build-nonlambda-replacement"
       (
	 (let-defalias
	   get-encap-type (lambda (&rest d) 'defvar)
	   (encap-build-nonlambda-replacement '(bar a) 'foo ))
	 
	 '(foo (defvar foo (bar a) "")))
       
       (
	 (let-defaliases
	   ( (get-encap-type (lambda (&rest d) 'defun))
	     (encap-pick-parms identity))
	   
	   (encap-build-nonlambda-replacement '(bar a) 'foo ))
	 '((foo a) (defun foo (a) "" (bar a))))

       )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lambda-replacement builders

(defalias 'encap-get-lambda-parms 'second)

(eval-when-compile
  (setf
    (get 'encap-get-lambda-parms 'rtest-suite)
    '("encap-get-lambda-parms"
       ( (encap-get-lambda-parms '(lambda (a b)))
	 '(a b))
       
       ( (encap-get-lambda-parms #'(lambda (a b)))
	 '(a b))
       )))


;;Here `form' already was a lambda expression in its place, so
;;`lambda' and arglist has to be stripped out of it.
(defun encap-build-lambda-replacement (form name)
  "Build call and definition for an encapped lambda form."
  
  (let* 
    (
      (def-sym
	(get-encap-type t encap-defun-types))

      (args (encap-get-lambda-parms form)))
    
    (list
      name
      (list* def-sym name args (nthcdr 2 form)))))


(eval-when-compile
  (setf
    (get 'encap-build-lambda-replacement 'rtest-suite)
    '("encap-build-lambda-replacement"
       ( (let-defalias 
	   get-encap-type (lambda (&rest dummy) 'defun )
	   (encap-build-lambda-replacement '(lambda(a) a) 'foo))
	 '(foo (defun foo (a) a)))
       
       ( "`&rest' and its friends are carried over unchanged"
	 (let-defalias
	   get-encap-type (lambda (&rest dummy) 'defun )
	   (encap-build-lambda-replacement '(lambda(&rest a) a) 'foo))
	 '(foo (defun foo (&rest a) a)))
       
       )))


;;Here the lambda expression was quoted, so we must undo the quote and
;;put it back around the call.
(defun encap-build-quoted-lambda-replacement (form name)
  ""
  (progv
    '(replacement definition)
    (encap-build-lambda-replacement (second form) name)

    (list
      `(function ,replacement)
      definition)))

(eval-when-compile
  (setf
    (get 'encap-build-quoted-lambda-replacement 'rtest-suite)
    '("encap-build-quoted-lambda-replacement"
       ;;Could we inherit encap-build-lambda-replacement's tests?
       ( (let-defalias 
	   get-encap-type (lambda (&rest dummy) 'defun )
	   (encap-build-quoted-lambda-replacement '#'(lambda(a) a) 'foo))
	 '((function foo) (defun foo (a) a)))
       
       ( "`&rest' and its friends are carried over unchanged"
	 (let-defalias
	   get-encap-type (lambda (&rest dummy) 'defun )
	   (encap-build-quoted-lambda-replacement '#'(lambda(&rest a) a) 'foo))
	 '((function foo) (defun foo (&rest a) a)))
       )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Build every sort of replacement
      

(defun encap-build-replacement (form name)
  ""
  '
  
  (if
    (encap-is-lambda-p form)
    (encap-build-lambda-replacement    form name)
    (encap-build-nonlambda-replacement form name))

  ;;$$Changed:  New code.
  (cond
    (
      (encap-is-lambda-p form)
      (encap-build-lambda-replacement    form name))
     
    (
      (encap-is-quoted-lambda-p form)
      (encap-build-quoted-lambda-replacement form name))

    (t
      (encap-build-nonlambda-replacement form name))))

    
;;Components have been tested separately.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading and writing from/to the buffer.

;;;;;;;;;
;;Helper

(defun encap-find-good-place nil ""
  (progn
    (beginning-of-defun)

    ;;To avoid tromping on autoloads, search backwards for a
    ;;non-comment line
    (beginning-of-line 0)
    (while
      (looking-at "^[ \t]*;")
      (beginning-of-line 0))

    ;;Go to the end in case there's something on the line.  Since we
    ;;know the only thing between here and the defun is comments, the
    ;;end is safe.
    (end-of-line 1)))


(defun encap-replace-by-call
  (start end new-call)
  ""

  (goto-char start)
  (delete-region start end)

  (goto-char start)
  (insert " ")
  (insert
    (pp-to-string new-call))

  ;;Indent the call nicely, in part so that it won't be mistaken for
  ;;the start of a defun.
  (goto-char start)
  (lisp-indent-line)
  (indent-sexp))

;;;###autoload
(defun encap-next-sexp (start name)
  ""

  (interactive "d\nSName of new entity: ")

  ;;Not sure this is needed, but lets be safe.
  (save-excursion
    
    (using-local-vars

      ;;Read it.  If it's not readable, that halts us.
      (goto-char start)
      ;;This could be nicely replaced by the purelist reader.
      (local-var form 
	(
	  ;;pl-read ;;$$Changed
	  read 
	  (current-buffer)))
    
      ;;Acquire the end point.  We'll need it to delete the original.
      (local-var end (point))

      ;;Get the new forms.
      (progv
	'(new-call new-definition)
	(encap-build-replacement form name)

	;;Replace the in-line definition with a use of the name 
	(encap-replace-by-call start end new-call)

	;;Write the new definition.
	(encap-find-good-place)
	(insert "\n" )
	;;$$Creating a good new definition in these terms will be a
	;;feat. 
	;;(pl-pp-insert new-definition) ;;$$Changed
	(insert (pp-to-string new-definition))

	))))

;;Test with a buffer with known contents:

;;It works OK in the middle of defun
;;It works OK even at the beginning of a line
;;It works OK when rite between symbols, not joining them together.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Overall tests

(eval-when-compile
  (setf
    (get 'encap 'rtest-suite)
    '("encap"

	 encap-build-quoted-lambda-replacement
	 encap-build-lambda-replacement
	 encap-get-lambda-parms
	 encap-build-nonlambda-replacement
	 encap-build-list-form-replacement
	 encap-get-list-form-parms
	 encap-get-parms-recursive
	 encap-simplify-function-form
	 encap-build-constant-replacement
	 encap-is-quoted-lambda-p
	 encap-is-lambda-p
       
       )))

(provide 'encap)

;;; encap.el ends here