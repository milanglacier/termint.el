;;; termint.el --- Run REPLs in a terminal backend -*- lexical-binding: t; -*-

;; Author: Milan Glacier <dev@milanglacier.com>
;; Maintainer: Milan Glacier <dev@milanglacier.com>
;; Version: 0.1
;; URL: https://github.com/milanglacier/termint.el
;; Package-Requires: ((emacs "29"))

;;; This file is NOT part of GNU Emacs

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file LICENSE.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package offers macros and functions for creating and managing
;; REPL sessions within a terminal emulator backend (term, eat, or
;; vterm).  It facilitates the creation of custom REPL commands
;; tailored to each defined REPL, with features including starting
;; session, sending code, and hiding REPL windows.  This is useful for
;; integrating terminal-based REPLs with Emacs efficiently.

;;; Code:

(defgroup termint nil
  "Group for termint."
  :group 'tools)

(defcustom termint-backend 'term
  "The backend to use for REPL sessions."
  :type '(choice (const :tag "eat" eat)
                 (const :tag "vterm" vterm)
                 (const :tag "term" term)))

(defvar vterm-buffer-name)
(defvar vterm-shell)
(declare-function vterm-send-string "vterm")
(declare-function vterm "vterm")

(defvar eat-buffer-name)
(defvar eat-shell)
(declare-function eat "eat")
(declare-function eat--send-string "eat")
(declare-function eat--synchronize-scroll "eat")

(declare-function term-exec "term")
(declare-function term-mode "term")
(declare-function term-char-mode "term")
(declare-function term-send-raw-string "term")

(defun termint--start-term-backend (repl-buffer-name repl-shell arg)
  "Start REPL-SHELL in REPL-BUFFER-NAME with numeric ARG with term backend."
  (require 'term)
  (let ((term-buffer-name
         (if arg
             (format "%s<%d>" repl-buffer-name arg)
           repl-buffer-name)))
    (if (get-buffer term-buffer-name)
        (pop-to-buffer term-buffer-name)
      (let* ((shell-list (split-string-shell-command repl-shell))
             (shell-cmd (car shell-list))
             (shell-args (cdr shell-list))
             (term-buffer (get-buffer-create term-buffer-name)))
        (with-current-buffer term-buffer
          (term-mode)
          (term-exec term-buffer term-buffer-name shell-cmd nil shell-args)
          (term-char-mode))
        (pop-to-buffer term-buffer)))))

(defun termint--start-eat-backend (repl-buffer-name repl-shell arg)
  "Start REPL-SHELL in REPL-BUFFER-NAME with numeric ARG with eat backend."
  (require 'eat)
  (let ((eat-buffer-name repl-buffer-name)
        (eat-shell repl-shell))
    (eat nil arg)))


(defun termint--start-vterm-backend (repl-buffer-name repl-shell arg)
  "Start REPL-SHELL in REPL-BUFFER-NAME with numeric ARG with vterm backend."
  (require 'vterm)
  (let ((vterm-buffer-name repl-buffer-name)
        (vterm-shell repl-shell))
    (vterm arg)))

(defun termint--send-string-term-backend (repl-buffer-name str)
  "Send STR to the process behind REPL-BUFFER-NAME with term backend."
  (with-current-buffer repl-buffer-name
    (term-send-raw-string str)))

(defun termint--send-string-eat-backend (repl-buffer-name str)
  "Send STR to the process behind REPL-BUFFER-NAME with eat backend."
  (with-current-buffer repl-buffer-name
    (when-let* ((eat-window (get-buffer-window)))
      ;; NOTE: This is crucial to ensure the
      ;; Eat window scrolls in sync with new
      ;; terminal output.
      (eat--synchronize-scroll (list eat-window)))
    (eat--send-string nil str)))

(defun termint--send-string-vterm-backend (repl-buffer-name str)
  "Send STR to the process behind REPL-BUFFER-NAME with vterm backend."
  (with-current-buffer repl-buffer-name
    (vterm-send-string str)))

(defmacro termint-define (repl-name repl-cmd &rest args)
  "Define a REPL schema.

The REPL session will be created via `termint-backend'.  The schema
includes three functions: one to start the REPL, one to send the
region and corresponding Evil operator (for Evil users), and one to
hide the REPL window if it exists.  A keymap, `termint-REPL-NAME-map', is
also included for these commands.

REPL-NAME is a string, REPL-CMD is a string, a form evaluated to a
string, or a function evaluated to a string.  ARGS is a plist, the
following properties are supported:

:bracketed-paste-p whether send the string with bracketed paste mode,
the default value is nil.  You can change the behavior at run time by
setting the generated variable
`termint-REPL-NAME-use-bracketed-paste-mode'.

:start-pattern the first string to send to the REPl before sending the
region.  The default is ''.  You can change the behavior at run time
by setting the generated variable `termint-REPL-NAME-start-pattern'.
Additionally, the value can be a plist with two attributes:
`:single-line' for specifying the string in single-line
scenarios.`:multi-lines' for defining the string in multi-line
contexts.

:end-pattern the last string to send to the REPL after sending the
region.  The default is '\\r'.  You can change the behavior at run time
by setting the generated variable
`termint-REPL-NAME-end-pattern'.  Additionally the value can be a plist
with two attributes: `:single-line' for specifying the string in
single-line scenarios, and `:multi-lines' for defining the string in
multi-line contexts.

:str-process-func the function to process the string before sending it
to the REPL.  The default is `identity'.  You can change the behavior
at run time by setting the generated variable
`termint-REPL-NAME-str-process-func'.

:source-func the function to source the code content to the REPL.  A
common approach involves writing the input string to a temporary file,
then returning a string that sources this file.  The exact \"sourcing\"
syntax depends on the target programming language.

:source-syntax: an alternative to `:source-func' that offers concise
but less flexible syntax specification.  It expects a string formatted
as follows (example for Python):

exec(compile(open(\"{{file}}\", \"r\").read(), \"{{file}}\", \"exec\"))

Where {{file}} represents the temporary file path.

When both `:source-func' and `:source-syntax' are provided,
`:source-syntax' takes precedence."

  (let ((start-func-name (intern (concat "termint-" repl-name "-start")))
        (send-region-func-name (intern (concat "termint-" repl-name "-send-region")))
        (send-region-operator-name (intern (concat "termint-" repl-name "-send-region-operator")))
        (source-region-func-name (intern (concat "termint-" repl-name "-source-region")))
        (source-region-operator-name (intern (concat "termint-" repl-name "-source-region-operator")))
        (send-string-func-name (intern (concat "termint-" repl-name "-send-string")))
        (hide-window-func-name (intern (concat "termint-" repl-name "-hide-window")))
        (keymap-name (intern (concat "termint-" repl-name "-map")))
        (bracketed-paste-p (plist-get args :bracketed-paste-p))
        (start-pattern (or (plist-get args :start-pattern) ""))
        (end-pattern (or (plist-get args :end-pattern) "\r"))
        (str-process-func (or (plist-get args :str-process-func) ''identity))
        (source-func (or (plist-get args :source-func) ''identity))
        (source-syntax (plist-get args :source-syntax))
        (repl-cmd-name (intern (concat "termint-" repl-name "-cmd")))
        (str-process-func-name (intern (concat "termint-" repl-name "-str-process-func")))
        (source-func-name (intern (concat "termint-" repl-name "-source-func")))
        (source-syntax-name (intern (concat "termint-" repl-name "-source-syntax")))
        (bracketed-paste-p-name (intern (concat "termint-" repl-name "-use-bracketed-paste-mode")))
        (start-pattern-name (intern (concat "termint-" repl-name "-start-pattern")))
        (end-pattern-name (intern (concat "termint-" repl-name "-end-pattern"))))

    `(progn

       (defvar ,repl-cmd-name ,repl-cmd
         ,(format "The shell command for the %s REPL." repl-name))

       (defvar ,str-process-func-name ,str-process-func
         ,(format "The function to process the string before sending it to the %s REPL." repl-name))

       (defvar ,source-func-name ,source-func
         ,(format "The function to source the code content for the %s REPL." repl-name))

       (defvar ,source-syntax-name ,source-syntax
         ,(format "The syntax to source the code content for the %s REPL." repl-name))

       (defvar ,bracketed-paste-p-name ,bracketed-paste-p
         ,(format "Whether use bracketed paste mode for sending string to the %s REPL." repl-name))

       (defvar ,start-pattern-name ,start-pattern
         ,(format "The first string to send to the %s REPL before sending the text." repl-name))

       (defvar ,end-pattern-name ,end-pattern
         ,(format "The last string to send to the %s REPL after sending the text." repl-name))

       (defun ,start-func-name (&optional arg)
         ,(format
           "Create a %s REPL buffer.
Start a new %s session or switch to an already active session. Return
the buffer selected (or created). With a numeric prefix arg, create or
switch to the session with that number as a suffix."
           repl-name repl-name)
         (interactive "P")
         (let* ((repl-buffer-name (format "*%s*" ,repl-name))
                (repl-shell (if (functionp ,repl-cmd-name)
                                (funcall ,repl-cmd-name)
                              ,repl-cmd-name)))
           (pcase termint-backend
             ('eat (termint--start-eat-backend repl-buffer-name repl-shell arg))
             ('vterm (termint--start-vterm-backend repl-buffer-name repl-shell arg))
             ('term
              (termint--start-term-backend repl-buffer-name repl-shell arg)))))

       (defun ,send-region-func-name (beg end &optional session)
         ,(format
           "Send the region delimited by BEG and END to %s.
With numeric prefix argument, send region to the process associated
with that number." repl-name)
         (interactive "r\nP")
         (let ((str (buffer-substring-no-properties beg end)))
           (,send-string-func-name str session)))

       (defun ,source-region-func-name (beg end &optional session)
         ,(format
           "Source the region delimited by BEG and END to %s.
With numeric prefix argument, send region to the process associated
with that number." repl-name)
         (interactive "r\nP")
         (let* ((str (buffer-substring-no-properties beg end))
                (str (if ,source-syntax-name
                         (let ((file (termint--make-tmp-file str)))
                           (replace-regexp-in-string "{{file}}" file ,source-syntax-name))
                       (funcall ,source-func-name str))))
           (,send-string-func-name str session)))

       (defun ,send-string-func-name (string &optional session)
         ,(format
           "Send the string to %s.
When invoked interactively, prompt for user input in the minibuffer.
If a numeric prefix argument is provided, send the string to the
process with that number."
           repl-name)
         (interactive "sinput your command: \nP")
         (let* ((repl-buffer-name
                 (if session
                     (format "*%s*<%d>" ,repl-name session)
                   (format "*%s*" ,repl-name)))
                (send-string
                 (pcase termint-backend
                   ('eat #'termint--send-string-eat-backend)
                   ('vterm #'termint--send-string-vterm-backend)
                   ('term #'termint--send-string-term-backend)))
                (multi-lines-p (string-match-p "\n" string))
                (bracketed-paste-start "\e[200~")
                (bracketed-paste-end "\e[201~")
                (string (funcall ,str-process-func-name string))
                (start-pattern (if (stringp ,start-pattern-name) ,start-pattern-name
                                 (if multi-lines-p
                                     (plist-get ,start-pattern-name :multi-lines)
                                   (plist-get ,start-pattern-name :single-line))))
                (end-pattern (if (stringp ,end-pattern-name) ,end-pattern-name
                               (if multi-lines-p
                                   (plist-get ,end-pattern-name :multi-lines)
                                 (plist-get ,end-pattern-name :single-line))))
                (final-string
                 (if multi-lines-p
                     (concat start-pattern
                             (and ,bracketed-paste-p-name bracketed-paste-start)
                             string
                             (and ,bracketed-paste-p-name bracketed-paste-end)
                             end-pattern)
                   (concat start-pattern string end-pattern))))
           (funcall send-string repl-buffer-name final-string)))

       (when (require 'evil nil t)
         (evil-define-operator ,send-region-operator-name (beg end session)
           ,(format
             "A evil operator wrapper around `%s'.
With a numeric prefix argument, send the region to the %s process
associated with that number" send-region-func-name repl-name)
           :move-point nil
           (interactive "<r>P")
           (,send-region-func-name beg end session))

         (evil-define-operator ,source-region-operator-name (beg end session)
           ,(format
             "A evil operator wrapper around `%s'.
With a numeric prefix argument, send the region to the %s process
associated with that number" source-region-func-name repl-name)
           :move-point nil
           (interactive "<r>P")
           (,source-region-func-name beg end session)))

       (defun ,hide-window-func-name (&optional arg)
         ,(format
           "hide the %s window.
With numeric prefix argument, hide the window with that number as a
suffix." repl-name)
         (interactive "P")
         (when-let* ((eat-buffer-name
                      (if arg (format "*%s*<%d>" ,repl-name arg)
                        (format "*%s*" ,repl-name)))
                     (buf (get-buffer eat-buffer-name))
                     (eat-buffer-window (get-buffer-window buf)))
           (delete-window eat-buffer-window)))

       (defvar ,keymap-name
         (let ((map (make-sparse-keymap)))
           (define-key map "s" #',start-func-name)
           (define-key map "r" #',send-region-func-name)
           (define-key map "R" #',source-region-func-name)
           (define-key map "e" #',send-string-func-name)
           (define-key map "h" #',hide-window-func-name)
           map)
         ,(format "Keymap for %s REPL commands." repl-name)))))

(defun termint--make-tmp-file (str &optional keep-file)
  "Create a temporary file with STR.
Delete the temp file afterwards unless KEEP-FILE is non-nil."
  ;; disable output to message buffer and minibuffer.
  (let ((inhibit-message t)
        (message-log-max nil)
        file)
    (setq file (make-temp-file "" nil "_termint" str))
    (unless keep-file (run-with-idle-timer 5 nil #'delete-file file))
    file))

(defvar termint-python-source-syntax-template
  "exec(compile(open(\"{{file}}\", \"r\").read(), \"{{file}}\", \"exec\"))"
  "The template syntax used to source code content into the Python REPL.
Be aware that if you intend to use PDB with functions sourced from a
temporary file, you should avoid deleting the temporary file.  In such
scenarios, use `:source-func' instead of `:source-syntax', as
`:source-syntax' will delete the temporary file.")

(defvar termint-ipython-source-syntax-template
  "%run -i \"{{file}}\""
  "The template syntax used to source code content into the iPython REPL.
Be aware that if you intend to use PDB with functions sourced from a
temporary file, you should avoid deleting the temporary file.  In such
scenarios, use `:source-func' instead of `:source-syntax', as
`:source-syntax' will delete the temporary file.")

(defvar termint-R-source-syntax-template
  "eval(parse(text = readr::read_file(\"{{file}}\")))"
  "The template syntax used to source code content into the R REPL.")

(defvar termint-bash-source-syntax-template "source {{file}}"
  "The template syntax used to source code content into the Bash REPL.")

(provide 'termint)
;;; termint.el ends here
