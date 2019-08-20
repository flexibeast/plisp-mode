;;; inferior-picolisp.el --- Org Babel session support for picolisp-mode

;; Copyright (C) 2009-2019  Guillermo R. Palavecine <grpala@gmail.com>, Thorsten Jolitz <tjolitz@gmail.com>, Alexis <flexibeast@gmail.com>

;; Author: Guillermo R. Palavecine <grpala@gmail.com>, Thorsten Jolitz <tjolitz@gmail.com>, Alexis <flexibeast@gmail.com>
;; Maintainer: Alexis <flexibeast@gmail.com>
;; URL: https://github.com/flexibeast/picolisp-mode
;; Keywords: picolisp, lisp, programming, org

;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; Commentary:

;; A fork of tj64's `inferior-picolisp'
;; (https://github.com/tj64/picolisp-mode/), stripped down to only
;; provide the minimum necessary for Org Babel session support, and
;; modified to be compatible with flexibeast's `picolisp-mode'
;; (https://github.com/flexibeast/picolisp-mode/). Initial work on the
;; fork, removing unneeded functionality, was done by cryptorick
;; (https://github.com/cryptorick).

;;; Code:


(require 'comint)

;;
;; User-customisable settings.
;;

(defgroup inferior-picolisp nil
  "Run a PicoLisp process in a buffer."
  :group 'picolisp)

(defcustom inferior-picolisp-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "Input matching this regexp are not saved on the history list.

Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters."
  :type 'regexp
  :group 'inferior-picolisp)

(defcustom inferior-picolisp-load-hook nil
  "Hook run when `inferior-picolisp' is loaded."
  :type 'hook
  :group 'inferior-picolisp)

(defcustom inferior-picolisp-mode-hook nil
  "Hook for customizing `inferior-picolisp'."
  :type 'hook
  :group 'inferior-picolisp)

(defcustom inferior-picolisp-program-name "/usr/bin/pil"
  "The name of the program used to run PicoLisp."
  :type '(file :must-match t)
  :group 'inferior-picolisp)

(defvar inferior-picolisp-buffer nil
  "The current PicoLisp process buffer.

MULTIPLE PROCESS SUPPORT
==================================================================

inferior-picolisp.el supports, in a fairly simple fashion,
running multiple PicoLisp processes. To run multiple PicoLisp
processes, you start the first up with \\[run-picolisp]. It will
be in a buffer named *picolisp*. Rename this buffer with
\\[rename-buffer]. You may now start up a new process with
another \\[run-picolisp]. It will be in a new buffer, named
*picolisp*. You can switch between the different process buffers
with \\[switch-to-buffer].

Whenever \\[run-picolisp] fires up a new process, it resets
`inferior-picolisp-buffer' to be the new process's buffer. If you
only run one process, this will do the right thing. If you run
multiple processes, you can change `inferior-picolisp-buffer'
to another process buffer with \\[set-variable].")


;;
;; Internal variables.
;;

(defvar inferior-picolisp--emacs-as-editor-p nil
  "If non-nil, use `eedit.l' instead of `edit.l'.")


;;
;; Internal functions.
;;

(defun inferior-picolisp--disable-line-editor ()
  "Disable inbuilt PicoLisp line-editor.

The line-editor is not needed when PicoLisp is run as an Emacs subprocess."
  (let ((pil-tmp-dir (expand-file-name "~/.pil/")))
    ;; renaming of existing editor file
    (cond
     ;; abnormal condition, something went wrong before
     ((and
       (member "editor" (directory-files pil-tmp-dir))
       (member "editor-orig" (directory-files pil-tmp-dir)))
      (let ((ed-size
             (nth
              7
              (file-attributes
               (expand-file-name "editor" pil-tmp-dir))))
            (ed-orig-size
             (nth
              7
              (file-attributes
               (expand-file-name "editor-orig"  pil-tmp-dir)))))
        (if (or (= ed-size 0)
                (<= ed-size ed-orig-size))
            (delete-file
             (expand-file-name "editor" pil-tmp-dir))
          (rename-file
           (expand-file-name "editor" pil-tmp-dir)
           (expand-file-name "editor-orig" pil-tmp-dir)
           'OK-IF-ALREADY-EXISTS))))
     ;; normal condition, only editor file exists
     ((member "editor" (directory-files pil-tmp-dir ))
      (rename-file
       (expand-file-name "editor" pil-tmp-dir)
       (expand-file-name "editor-orig" pil-tmp-dir))))
    ;; after renaming, create new empty editor file
    (with-current-buffer
        (find-file-noselect
         (expand-file-name "editor" pil-tmp-dir))
      (erase-buffer)
      (save-buffer)
      (kill-buffer))))

(defun inferior-picolisp--get-editor-info ()
  "Find out if Emacs is used as editor."
  (let* ((editor-file (expand-file-name "editor" "~/.pil/"))
         (editor-orig-file (expand-file-name "editor-orig" "~/.pil/"))
         (ed-file
          (cond
           ((file-exists-p editor-file) editor-file)
           ((file-exists-p editor-orig-file) editor-orig-file)
           (t nil))))
    (when ed-file
      (with-current-buffer (find-file-noselect ed-file)
        (goto-char (point-min))
        (if (re-search-forward "eedit" nil 'NOERROR)
            (setq inferior-picolisp--emacs-as-editor-p t)
          (setq inferior-picolisp--emacs-as-editor-p nil))
        (kill-buffer)))))

(defun inferior-picolisp--get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end) ) ) )

(defun inferior-picolisp--input-filter (str)
  "Don't save anything matching `inferior-picolisp-filter-regexp'."
  (not (string-match inferior-picolisp-filter-regexp str)) )

(defun inferior-picolisp--reset-line-editor ()
  "Reset inbuilt PicoLisp line-editor to original state."
  (let ((pil-tmp-dir (expand-file-name "~/.pil/")))
    (if (member "editor-orig" (directory-files pil-tmp-dir))
        (rename-file
         (expand-file-name "editor-orig" pil-tmp-dir)
         (expand-file-name "editor" pil-tmp-dir)
         'OK-IF-ALREADY-EXISTS)
      (delete-file
       (expand-file-name "editor" pil-tmp-dir)))))


;;
;; User-facing functions.
;;

;; We can't namespace `run-picolisp' because the `ob-picolisp'
;; functionalty included with Emacs expects the non-namespaced
;; version.

;;;###autoload
(defun run-picolisp (cmd)
  "Run an inferior Picolisp process, input and output via buffer `*picolisp*'.

If there is a process already running in `*picolisp*', switch to
that buffer.

With argument, allows you to edit the command line (default is value
of `inferior-picolisp-program-name').

Runs the hook `inferior-picolisp-mode-hook' (after the `comint-mode-hook'
is run)."

  (interactive (list (if current-prefix-arg
                         (read-string "Run PicoLisp: " inferior-picolisp-program-name)
                       inferior-picolisp-program-name)))
  (when (not (comint-check-proc "*picolisp*"))
    (let ((cmdlist (split-string cmd)))
      (inferior-picolisp--get-editor-info)
      (inferior-picolisp--disable-line-editor)
      (set-buffer
       (apply 'make-comint
              "picolisp"
              (car cmdlist)
              nil
              ;; hack for multi-word PicoLisp arguments:
              ;; separate them with '_XXX_' in the 'cmd' arg
              ;; instead of blanks
              (mapcar
               (lambda (--arg)
                 (replace-regexp-in-string
                  "_XXX_" " " --arg))
               (if inferior-picolisp--emacs-as-editor-p
                   (cons "@lib/eedit.l" (cdr cmdlist))
                 (cons "@lib/edit.l" (cdr cmdlist))))))
      (inferior-picolisp--reset-line-editor)
      (inferior-picolisp-mode)))
  (setq inferior-picolisp-program-name cmd)
  (setq inferior-picolisp-buffer "*picolisp*")
  (pop-to-buffer "*picolisp*"))

;;;###autoload (add-hook 'same-window-buffer-names "*picolisp*")
(define-derived-mode inferior-picolisp-mode comint-mode "Inferior PicoLisp"
  "Major mode for interacting with an inferior PicoLisp process.

A PicoLisp process can be fired up with 'M-x run-picolisp'.

Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`inferior-picolisp-mode-hook' (in that order).

For information on running multiple processes in multiple buffers, see
documentation for variable `inferior-picolisp-buffer'."
  ;; Customize in inferior-picolisp-mode-hook
  (setq comint-prompt-regexp "^[^\n:?!]*[?!:]+ *")
  (setq comint-prompt-read-only nil)
  (setq comint-input-filter (function inferior-picolisp--input-filter))
  (setq comint-get-old-input (function inferior-picolisp--get-old-input))
  (setq mode-line-process '(":%s"))
  (setq comint-input-ring-file-name "~/.pil_history"))


;; --

(run-hooks 'inferior-picolisp-load-hook)
(provide 'inferior-picolisp)

;;; inferior-picolisp.el ends here
