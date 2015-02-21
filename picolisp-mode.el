;;; picolisp-mode.el --- Major mode for PicoLisp programming.

;; Copyright (C) 2014-2015  Alexis <flexibeast@gmail.com>

;; Author: Alexis <flexibeast@gmail.com>
;; Maintainer: Alexis <flexibeast@gmail.com>
;; Created: 2014-11-18
;; URL: https://github.com/flexibeast/picolisp-mode
;; Keywords: picolisp, lisp, programming

;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; Commentary:

;; `picolisp-mode' provides a major mode for PicoLisp programming.

;; This package is not based on, nor connected with, the PicoLisp support for Emacs provided in [the PicoLisp distribution](http://software-lab.de/down.html), or the more recently [updated version of that support](https://github.com/tj64/picolisp-mode). At this stage, the main advantages provided by this package are:

;; * an actively maintained and supported system;

;; * access to the PicoLisp reference documentation; and

;; * a cleaner codebase.

;; ## Table of Contents

;; - [Features](#features)
;; - [Installation](#installation)
;; - [Usage](#usage)
;; - [TODO](#todo)
;; - [Issues](#issues)
;; - [License](#license)

;; ## Features

;; * Syntax highlighting of PicoLisp code. (But please read the below [note on syntax highlighting](#note).)

;; * Comint-based `pil' REPL buffers.

;; * Quick access to documentation for symbol at point.

;; ## Installation

;; Install [picolisp-mode from MELPA](http://melpa.org/#/picolisp-mode), or put the `picolisp-mode' folder in your load-path and do a `(require 'picolisp-mode)'.

;; ## Usage

;; Enable syntax highlighting for a PicoLisp source buffer with `M-x picolisp-mode'. 

;; Start a `pil' REPL session with `M-x picolisp-repl'.

;; Access documentation for the function at point with `C-c C-d' (`picolisp-describe-symbol'). By default, documentation will be displayed via the `lynx' HTML browser. However, one can set the value of `picolisp-documentation-method' to either a string containing the absolute path to an alternative browser, or - for users of Emacs 24.4 and above - to the symbol `picolisp-display-documentation'; this function uses the `shr' library to display the documentation in an Emacs buffer. The absolute path to the documentation is specified via `picolisp-documentation-path', and defaults to `/usr/share/doc/picolisp/doc/'.

;; Comment a region in a `picolisp-mode' buffer with `C-c C-;' (`picolisp-comment-region'); uncomment a region in a `picolisp-mode' buffer with `C-c C-:' (`picolisp-uncomment-region'). By default one '#' character is added/removed; to specify more, supply a numeric prefix argument to either command.

;; The various customisation options, including the faces used for syntax highlighting, are available via the `picolisp' customize-group.

;; <a name="note"></a>

;; ### A note on syntax highlighting

;; PicoLisp's creator is opposed to syntax highlighting of symbols in PicoLisp, for [good reasons](http://www.mail-archive.com/picolisp@software-lab.de/msg05019.html). However, some - such as the author of this package! - feel that, even taking such issues into consideration, the benefits can outweigh the costs. (For example, when learning PicoLisp, it can be useful to get immediate visual feedback about unintentionally redefining a PicoLisp 'builtin'.) To accommodate both views, syntax highlighting can be enabled or disabled via the `picolisp-syntax-highlighting-p' variable; by default, it is set to `t' (enabled).

;; ## TODO

;; * Add ElDoc support.

;; * Add indentation support.

;; * Add PicoLisp menu to menubar.

;; <a name="issues"></a>

;; ## Issues / bugs

;; If you discover an issue or bug in `picolisp-mode' not already noted:

;; * as a TODO item, or

;; * in [the project's "Issues" section on GitHub](https://github.com/flexibeast/picolisp-mode/issues),

;; please create a new issue with as much detail as possible, including:

;; * which version of Emacs you're running on which operating system, and

;; * how you installed `picolisp-mode'.

;; ## License

;; [GNU General Public License version 3](http://www.gnu.org/licenses/gpl.html), or (at your option) any later version.

;;; Code:


;;
;; User-customisable settings.
;;

(defgroup picolisp nil
  "PicoLisp support."
  :group 'languages)

(defcustom picolisp-picolisp-executable "/usr/bin/picolisp"
  "Absolute path of the `picolisp' executable."
  :type '(file :must-match t)
  :group 'picolisp)

(defcustom picolisp-pil-executable "/usr/bin/pil"
  "Absolute path of the `pil' executable."
  :type '(file :must-match t)
  :group 'picolisp)

(defcustom picolisp-documentation-directory "/usr/share/doc/picolisp/doc/"
  "Absolute path of the PicoLisp HTML documentation directory."
  :type 'directory
  :group 'picolisp)

(defcustom picolisp-documentation-method 'picolisp-display-documentation
  "System to be used to display PicoLisp documentation."
  :type '(radio (function :tag "Function - must already be defined" :value 'picolisp-display-documentation)
                (file :tag "HTML browser - absolute path" :value "/usr/bin/lynx"))
  :group 'picolisp)

(defcustom picolisp-repl-debug-p t
  "Whether to enable debug mode in the REPL.
Must be `t' to access documentation via `picolisp-describe-symbol'."
  :type 'boolean
  :group 'picolisp)

(defcustom picolisp-syntax-highlighting-p t
  "Whether to enable syntax highlighting."
  :type 'boolean
  :group 'picolisp)

(defgroup picolisp-faces nil
  "Faces for PicoLisp syntax highlighting."
  :group 'picolisp)

(defface picolisp-abstract-class-face
  '((((background light)) :foreground "blue"))
  "Face for PicoLisp abstract classes."
  :group 'picolisp-faces)

(defface picolisp-builtin-face
  '((((background light)) :foreground "Purple"))
  "Face for PicoLisp builtins."
  :group 'picolisp-faces)

(defface picolisp-comment-face
  '((((background light)) :foreground "green"))
  "Face for PicoLisp comments."
  :group 'picolisp-faces)

(defface picolisp-global-constant-face
  '((((background light)) :foreground "Purple"))
  "Face for PicoLisp global constants."
  :group 'picolisp-faces)

(defface picolisp-global-variable-face
  '((((background light)) :foreground "blue"))
  "Face for PicoLisp global variables."
  :group 'picolisp-faces)

(defface picolisp-local-function-face
  '((((background light)) :foreground "blue"))
  "Face for PicoLisp local functions."
  :group 'picolisp-faces)

(defface picolisp-method-face
  '((((background light)) :foreground "blue"))
  "Face for PicoLisp methods."
  :group 'picolisp-faces)

(defface picolisp-normal-class-face
  '((((background light)) :foreground "blue"))
  "Face for PicoLisp normal classes."
  :group 'picolisp-faces)


;;
;; Internal variables.
;;

;; http://software-lab.de/doc/ref.html#fun

(defvar picolisp-builtins
  '("!" "$" "$dat" "$tim" "%" "&" "*" "**" "*/" "*Allow" "*Bye" "*CPU" "*Class" "*Class" "*DB" "*Dbg" "*Dbg" "*Dbs" "*EAdr" "*Err" "*Fork" "*Hup" "*Led" "*Msg" "*OS" "*PPid" "*Pid" "*Prompt" "*Run" "*Scl" "*Sig1" "*Sig2" "*Solo" "*Tsm" "*Uni" "*Zap" "+" "+Alt" "+Any" "+Aux" "+Bag" "+Blob" "+Bool" "+Date" "+Dep" "+Entity" "+Fold" "+Hook" "+Hook2" "+Idx" "+IdxFold" "+Joint" "+Key" "+Link" "+List" "+Mis" "+Need" "+Number" "+Ref" "+Ref2" "+Sn" "+String" "+Swap" "+Symbol" "+Time" "+UB" "+index" "+relation" "-" "->" "/" ":" "::" ";" "<" "<=" "<>" "=" "=0" "=:" "==" "====" "=T" ">" ">=" ">>" "?" "@" "@@" "@@@" "This" "^" "abort" "abs" "accept" "accu" "acquire" "adr" "alarm" "align" "all" "allow" "allowed" "and" "any" "append" "append/3" "apply" "arg" "args" "argv" "as" "asoq" "assert" "asserta" "asserta/1" "assertz" "assertz/1" "assoc" "at" "atom" "aux" "balance" "be" "beep" "bench" "bin" "bind" "bit?" "blob" "blob!" "bool" "bool/3" "box" "box?" "by" "bye" "bytes" "caaaar" "caaadr" "caaar" "caadar" "caaddr" "caadr" "caar" "cache" "cadaar" "cadadr" "cadar" "caddar" "cadddr" "caddr" "cadr" "call" "call/1" "can" "car" "case" "casq" "catch" "cd" "cdaaar" "cdaadr" "cdaar" "cdadar" "cdaddr" "cdadr" "cdar" "cddaar" "cddadr" "cddar" "cdddar" "cddddr" "cdddr" "cddr" "cdr" "center" "chain" "char" "chdir" "chkTree" "chop" "circ" "circ?" "class" "clause" "clause/2" "clip" "close" "cmd" "cnt" "co" "collect" "commit" "con" "conc" "cond" "connect" "cons" "copy" "count" "ctl" "ctty" "curry" "cut" "d" "daemon" "dat$" "datStr" "datSym" "date" "day" "db" "db/3" "db/4" "db/5" "db:" "dbSync" "dbck" "dbs" "dbs+" "de" "debug" "dec" "def" "default" "del" "delete" "delete/3" "delq" "dep" "depth" "diff" "different/2" "dir" "dirname" "dm" "do" "doc" "e" "echo" "edit" "em" "env" "eof" "eol" "equal/2" "err" "errno" "eval" "expDat" "expTel" "expr" "ext?" "extend" "extern" "extra" "extract" "fail" "fail/0" "fetch" "fifo" "file" "fill" "filter" "fin" "finally" "find" "fish" "flg?" "flip" "flush" "fmt64" "fold" "fold/3" "for" "fork" "forked" "format" "free" "from" "full" "fully" "fun?" "gc" "ge0" "genKey" "get" "getd" "getl" "glue" "goal" "group" "gt0" "hash" "hax" "hd" "head" "head/3" "heap" "hear" "here" "hex" "host" "id" "idx" "if" "if2" "ifn" "import" "in" "inc" "inc!" "index" "info" "init" "insert" "intern" "ipid" "isa" "isa/2" "iter" "job" "journal" "key" "kids" "kill" "last" "later" "ld" "le0" "leaf" "length" "let" "let?" "lieu" "line" "lines" "link" "lint" "lintAll" "list" "listen" "lit" "load" "loc" "local" "locale" "lock" "loop" "low?" "lowc" "lst/3" "lst?" "lt0" "lup" "macro" "made" "mail" "make" "map" "map/3" "mapc" "mapcan" "mapcar" "mapcon" "maplist" "maps" "mark" "match" "max" "maxKey" "maxi" "member" "member/2" "memq" "meta" "meth" "method" "min" "minKey" "mini" "mix" "mmeq" "money" "more" "msg" "n0" "n==" "nT" "name" "nand" "native" "need" "new" "new!" "next" "nil" "nil/1" "nond" "nor" "not" "not/1" "nth" "num?" "obj" "object" "oct" "off" "offset" "on" "onOff" "once" "one" "open" "opid" "opt" "or" "or/2" "out" "pack" "pad" "pair" "part/3" "pass" "pat?" "patch" "path" "peek" "permute/2" "pick" "pico" "pilog" "pipe" "place" "poll" "pool" "pop" "port" "pp" "pr" "prEval" "pre?" "pretty" "prin" "prinl" "print" "println" "printsp" "prior" "proc" "prog" "prog1" "prog2" "prop" "protect" "prove" "prune" "push" "push1" "put" "put!" "putl" "pwd" "qsym" "query" "queue" "quit" "quote" "rand" "range" "range/3" "rank" "raw" "rc" "rd" "read" "recur" "recurse" "redef" "rel" "release" "remote/2" "remove" "repeat" "repeat/0" "replace" "request" "rest" "retract" "retract/1" "reverse" "rewind" "rollback" "root" "rot" "round" "rules" "run" "same/3" "scan" "scl" "script" "sect" "seed" "seek" "select" "select/3" "send" "seq" "set" "set!" "setq" "show" "show/1" "sigio" "size" "skip" "solve" "sort" "sp?" "space" "split" "sqrt" "stack" "stamp" "state" "stem" "step" "store" "str" "str?" "strDat" "strip" "sub?" "subr" "sum" "super" "sym" "sym?" "symbols" "sync" "sys" "t" "tab" "tail" "task" "telStr" "tell" "test" "text" "throw" "tick" "till" "tim$" "time" "timeout" "tmp" "tolr/3" "touch" "trace" "traceAll" "trail" "tree" "trim" "true/0" "try" "type" "u" "ubIter" "udp" "ultimo" "unbug" "undef" "unify" "uniq" "uniq/2" "unless" "until" "untrace" "up" "upd" "update" "upp?" "uppc" "use" "useKey" "usec" "val" "val/3" "var" "var:" "version" "vi" "view" "wait" "week" "what" "when" "while" "who" "wipe" "with" "wr" "wrap" "xchg" "xor" "x|" "yield" "yoke" "zap" "zapTree" "zero" "|"))

(defvar picolisp-builtins-by-length
  (let ((bs (copy-sequence picolisp-builtins)))
    (sort bs #'(lambda (e1 e2)
                 (> (length e1) (length e2)))))
  "List of PicoLisp builtins, sorted by length for use by
`picolisp-builtins-regex'.")

(defvar picolisp-builtins-regex
  (let ((s "")
        (firstp t))
    (dolist (b picolisp-builtins-by-length)
      (if (not firstp)
          (setq s (concat s "\\|" (regexp-quote b)))
        (progn
          (setq s (regexp-quote b))
          (setq firstp nil))))
    s)
  "Regex for use by `picolisp-font-lock-keywords'.")

;; http://software-lab.de/doc/ref.html#conv

(defvar picolisp-font-lock-keywords
  `(("\\_<\\(T\\|NIL\\)\\_>"
     (1 'picolisp-global-constant-face))
    (,(concat "\\_<\\(" picolisp-builtins-regex "\\)\\_>")
     (1 'picolisp-builtin-face))
    ("\\(#.*\\)"
     (1 'picolisp-comment-face))
    ("\\(\\+[a-z]\\S-*\\)"
     (1 'picolisp-abstract-class-face))
    ("\\(\\*[[:alpha:]]+\\)"
     (1 'picolisp-global-variable-face))
    ("\\(_\\S-+\\)"
     (1 'picolisp-local-function-face))
    ("(\\([[:alpha:]]\\S-+>\\s-\\)"
     (1 'picolisp-method-face))
    ("\\(\\+[A-Z][[:alpha:]]*\\)"
     (1 'picolisp-normal-class-face))))

;;
;; http://software-lab.de/doc/ref.html#symbol:
;;
;; Internal symbol names can consist of any printable (non-whitespace)
;; character, except for the following meta characters:
;;
;; "  '  (  )  ,  [  ]  `  ~ { }
;;

(defvar picolisp-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;;;
    ;;; Symbol syntax.
    ;;;

    (modify-syntax-entry ?* "_   " table)
    (modify-syntax-entry ?+ "_   " table)
    (modify-syntax-entry ?- "_   " table)
    (modify-syntax-entry ?: "_   " table)
    (modify-syntax-entry ?> "_   " table)
    (modify-syntax-entry ?< "_   " table)
    (modify-syntax-entry ?@ "_   " table)
    (modify-syntax-entry ?A "_   " table)
    (modify-syntax-entry ?B "_   " table)
    (modify-syntax-entry ?C "_   " table)
    (modify-syntax-entry ?D "_   " table)
    (modify-syntax-entry ?E "_   " table)
    (modify-syntax-entry ?F "_   " table)
    (modify-syntax-entry ?G "_   " table)
    (modify-syntax-entry ?H "_   " table)
    (modify-syntax-entry ?I "_   " table)
    (modify-syntax-entry ?J "_   " table)
    (modify-syntax-entry ?K "_   " table)
    (modify-syntax-entry ?L "_   " table)
    (modify-syntax-entry ?M "_   " table)
    (modify-syntax-entry ?N "_   " table)
    (modify-syntax-entry ?O "_   " table)
    (modify-syntax-entry ?P "_   " table)
    (modify-syntax-entry ?Q "_   " table)
    (modify-syntax-entry ?R "_   " table)
    (modify-syntax-entry ?S "_   " table)
    (modify-syntax-entry ?T "_   " table)
    (modify-syntax-entry ?U "_   " table)
    (modify-syntax-entry ?V "_   " table)
    (modify-syntax-entry ?W "_   " table)
    (modify-syntax-entry ?X "_   " table)
    (modify-syntax-entry ?Y "_   " table)    
    (modify-syntax-entry ?Z "_   " table)
    (modify-syntax-entry ?a "_   " table)
    (modify-syntax-entry ?b "_   " table)
    (modify-syntax-entry ?c "_   " table)
    (modify-syntax-entry ?d "_   " table)
    (modify-syntax-entry ?e "_   " table)
    (modify-syntax-entry ?f "_   " table)
    (modify-syntax-entry ?g "_   " table)
    (modify-syntax-entry ?h "_   " table)
    (modify-syntax-entry ?i "_   " table)
    (modify-syntax-entry ?j "_   " table)
    (modify-syntax-entry ?k "_   " table)
    (modify-syntax-entry ?l "_   " table)
    (modify-syntax-entry ?m "_   " table)
    (modify-syntax-entry ?n "_   " table)
    (modify-syntax-entry ?o "_   " table)
    (modify-syntax-entry ?p "_   " table)
    (modify-syntax-entry ?q "_   " table)
    (modify-syntax-entry ?r "_   " table)
    (modify-syntax-entry ?s "_   " table)
    (modify-syntax-entry ?t "_   " table)
    (modify-syntax-entry ?u "_   " table)
    (modify-syntax-entry ?v "_   " table)
    (modify-syntax-entry ?w "_   " table)
    (modify-syntax-entry ?x "_   " table)
    (modify-syntax-entry ?y "_   " table)    
    (modify-syntax-entry ?z "_   " table)
    
    ;; { and } delimit external symbol names.
    (modify-syntax-entry ?\{ "_   " table)
    (modify-syntax-entry ?\} "_  " table)

    ;; . can be used in a symbol name, even though,
    ;; when surrounded by white space, it's
    ;; a metacharacter indicating a dotted pair.
    (modify-syntax-entry ?. "_   " table)

    ;; " primarily indicates a transient symbol, even
    ;; though it can also be used to indicate strings.
    (modify-syntax-entry ?\" "_    " table)

    ;;;
    ;;; Whitespace syntax.
    ;;;
    
    (modify-syntax-entry ?\s "    " table)
    (modify-syntax-entry ?\x8a0 "    " table)
    (modify-syntax-entry ?\t "    " table)
    (modify-syntax-entry ?\f "    " table)

    ;;;
    ;;; Comment syntax.
    ;;;
    
    (modify-syntax-entry ?# "<   " table)
    (modify-syntax-entry ?\n ">   " table)

    ;;;
    ;;; Quote syntax.
    ;;;
    
    (modify-syntax-entry ?` "'   " table)
    (modify-syntax-entry ?' "'   " table)
    (modify-syntax-entry ?, "'   " table)
    (modify-syntax-entry ?~ "'   " table)

    ;;;
    ;;; Parenthesis syntax.
    ;;;
    
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)

    ;;;
    ;;; Escape syntax.
    ;;;

    (modify-syntax-entry ?\\ "\\   " table)
    
    table)
  
  "Syntax table used in `picolisp-mode'.")


;;
;; Internal functions.
;;

(defmacro picolisp-dolist-pairs (spec &rest body)
  "Loop over a list.
Evaluate BODY with VAR1 bound to each car from LIST, and VAR2
bound to each cadr from LIST, in turn.

\(fn (VAR1 VAR2 LIST) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (let ((tmp '--dolist-pairs-tail--))
    `(let (,(nth 0 spec)
           ,(nth 1 spec)
           (,tmp ,(nth 2 spec)))
       (while ,tmp
         (setq ,(nth 0 spec) (car ,tmp))
         (setq ,(nth 1 spec) (cadr ,tmp))
         ,@body
         (setq ,tmp (cddr ,tmp))))))

(defun picolisp-display-documentation (sym)
  "Use `shr' to display documentation for symbol at point."
  (unless (or (> emacs-major-version 24)
              (and (= emacs-major-version 24)
                   (> emacs-minor-version 3)))
    (error "Emacs 24.4 or greater required"))
  (let* ((char (progn
                 (string-match "^[[:punct:]]*\\([[:alpha:]]\\)" sym)
                 (upcase (match-string 1 sym))))
         (doc (concat picolisp-documentation-directory "ref" char ".html"))
         (bfr (generate-new-buffer " *PicoLisp documentation source*"))
         (dom (progn
                (switch-to-buffer bfr)
                (insert-file-contents doc)
                (libxml-parse-html-region (point-min) (point-max))))
         (dl (nth 5 (nth 3 dom))))
    (picolisp-dolist-pairs (fst snd dl)
      (if (eq 'dt (car-safe fst))
          (if (string= sym (cdaadr (nth 2 fst)))
              (progn
                (switch-to-buffer (generate-new-buffer (concat "*PicoLisp documentation - '" sym "' *")))
                (insert (concat (propertize "Symbol:" 'face '(foreground-color . "ForestGreen")) " " (propertize sym 'face 'picolisp-builtin-face) "\n\n"))
                (shr-insert-document snd)
                (goto-char (point-min))
                (help-mode)))))
    (kill-buffer bfr)))


;;
;; User-facing functions.
;;

(defun picolisp-comment-region (&optional n)
  "Comment lines in region using N '#' characters. N can be
specified by providing a numeric prefix argument; otherwise,
N defaults to 1."
  (interactive "p")
  (if n
      (comment-region (region-beginning) (region-end) n)
    (comment-region (region-beginning) (region-end) 1)))

(defun picolisp-uncomment-region (&optional n)
  "Uncomment lines in region by removing N '#' characters. N can
be specified by providing a numeric prefix argument; otherwise,
N defaults to 1."
  (interactive "p")
  (if n
      (uncomment-region (region-beginning) (region-end) n)
    (comment-region (region-beginning) (region-end) 1)))

(defun picolisp-describe-symbol ()
  "Display documentation for symbol at point, via method
specified by `picolisp-documentation-method'."
  (interactive)
  (let ((process-environment
         (if (eq 'string (type-of picolisp-documentation-method))
             (add-to-list 'process-environment
                          (concat "BROWSER=" picolisp-documentation-method))
           process-environment))
        (sym (symbol-name
              (symbol-at-point))))
    (if (member sym picolisp-builtins)
        (cond
         ((eq 'symbol (type-of picolisp-documentation-method))
          (picolisp-display-documentation sym))
         ((eq 'string (type-of picolisp-documentation-method))
          (start-process-shell-command "picolisp-doc" nil
                                       (concat "pil -\"doc (car (nth (argv) 3)\" -bye - '" sym "' +")))
         nil)
      (message "No PicoLisp builtin at point."))))

;;;###autoload
(define-derived-mode picolisp-mode lisp-mode "PicoLisp"
  "Major mode for PicoLisp programming. Derived from lisp-mode.

\\{picolisp-mode-map}"
  (set-syntax-table picolisp-mode-syntax-table)
  (if picolisp-syntax-highlighting-p
      (setq font-lock-defaults '((picolisp-font-lock-keywords))))
  (define-key picolisp-mode-map (kbd "C-c C-;") 'picolisp-comment-region)
  (define-key picolisp-mode-map (kbd "C-c C-:") 'picolisp-uncomment-region)
  (define-key picolisp-mode-map (kbd "C-c C-d") 'picolisp-describe-symbol)
  (make-local-variable 'comment-start)
  (setq comment-start "#")
  (make-local-variable 'comment-end)
  (setq comment-end "\n"))

;;;###autoload
(define-derived-mode picolisp-repl-mode comint-mode "PicoLisp REPL"
  "Major mode for `pil' REPL sessions. Derived from comint-mode.

\\{picolisp-repl-mode-map}"
  (set-syntax-table picolisp-mode-syntax-table)
  (if picolisp-syntax-highlighting-p
      (setq font-lock-defaults '((picolisp-font-lock-keywords))))
  (define-key picolisp-repl-mode-map (kbd "C-c C-d") 'picolisp-describe-symbol))

;;;###autoload
(defun picolisp-repl ()
  "Start a `pil' session in a new `picolisp-repl-mode' buffer."
  (interactive)
  (let ((process-environment
         (if (eq 'string (type-of picolisp-documentation-method))
             (add-to-list 'process-environment
                          (concat "BROWSER=" picolisp-documentation-method))
           process-environment)))
    (make-comint "picolisp-repl" picolisp-pil-executable nil (if picolisp-repl-debug-p "+" nil))
    (switch-to-buffer "*picolisp-repl*")
    (picolisp-repl-mode)))


;; --

(provide 'picolisp-mode)

;;; picolisp-mode.el ends here
