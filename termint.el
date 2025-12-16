;;; termint.el --- Run REPLs in a terminal backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Milan Glacier <dev@milanglacier.com>
;; Maintainer: Milan Glacier <dev@milanglacier.com>
;; Version: 0.2
;; URL: https://github.com/milanglacier/termint.el
;; Package-Requires: ((emacs "29.1"))

;; This file is part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

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

;; termint provides a flexible way to define and manage interactions
;; with REPLs and CLI apps running inside a terminal emulator backend
;; within Emacs.  It allows you to easily configure how Emacs
;; communicates with different REPLs, leveraging the capabilities of
;; fully-featured terminal emulators like `term', `vterm', or `eat'.

;; Instead of relying on Emacs’s built-in “dumb” terminal
;; (`comint-mode'), termint runs REPLs in a full terminal emulator,
;; enabling features like bracketed paste mode, proper rendering, and
;; potentially advanced interaction for complex terminal applications.

;; It facilitates the creation of custom REPL commands
;; tailored to each defined REPL, with features including starting
;; session, sending code, and hiding REPL windows.  This is useful for
;; integrating terminal-based REPLs with Emacs efficiently.

;;; Code:

(require 'cl-lib)

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

(defgroup termint nil
  "Group for termint."
  :group 'tools)

(defcustom termint-backend 'term
  "The backend to use for REPL sessions.
Supported backends include `eat', `vterm', and the built-in `term'.
Note that `eat' and `vterm' must be installed separately."
  :type '(choice (const :tag "eat" eat)
                 (const :tag "vterm" vterm)
                 (const :tag "term" term)))

(defface termint-source-command-hint-face
  '((t :inherit font-lock-comment-face))
  "Face used for displaying hint of source command.")

(defcustom termint-region-dispatchers
  '(("paragraph" . termint--dispatch-paragraph)
    ("buffer" . termint--dispatch-buffer)
    ("defun" . termint--dispatch-defun))
  "Alist mapping dispatchable region types to dispatcher functions.
Each element is of the form (NAME . DISPATCHER).  The NAME is appended
to generated command names (e.g., `termint-ipython-send-NAME' and
`termint-ipython-source-NAME' for an ipython schema), while DISPATCHER
should be a function returning a cons cell (BEG . END) for the
corresponding region.  Customize this before calling `termint-define'
to generate additional region commands automatically for each REPL
schema."
  :type '(alist :key-type string :value-type function))

(defcustom termint-schema-custom-commands nil
  "Alist of extra commands generated for each REPL schema.
Each element takes the form (SUFFIX . FN).  SUFFIX is a string
appended to `termint-REPL-NAME-` to create the final command name for
every schema defined via `termint-define'.  FN is the function invoked
by the generated command.  It receives two arguments: REPL-NAME and
SESSION.  See `termint--hide-window' for a reference implementation of
the expected function signature.

For example, to generate a command named `termint-ipython-hide-window',
add the entry `(\"hide-window\" . termint--hide-window)' to this list.

Configure this variable before
invoking `termint-define' to ensure the custom commands are
generated."
  :type '(alist :key-type string :value-type function))

(defcustom termint-mode-map-additional-keys
  '(("f" . "send-defun")
    ("F" . "source-defun")
    ("b" . "send-buffer")
    ("B" . "source-buffer")
    ("p" . "send-paragraph")
    ("P" . "source-paragraph"))
  "Alist of keys and command suffixes to add to generated REPL keymaps.
Each element is a cons cell of the form (KEY . SUFFIX).  KEY is the
string sequence to bind.  SUFFIX is a string appended to
`termint-REPL-NAME-' to resolve the actual command symbol (e.g.,
\"send-defun\" becomes `termint-ipython-send-defun' when defining an
ipython REPL).  This variable should be modified before calling
`termint-define' to affect the generated keymaps."
  :type '(alist :key-type string :value-type string))



(defun termint--get-session-suffix (session)
  "Return the session suffix from SESSION if interactive spec is \"P\"."
  (cond
   ((eq 0 session) nil)
   ((numberp session) (abs session))
   ((numberp (car session)) (floor (log (car session) 4)))
   (t nil)))

(defun termint--start (repl-name repl-cmd session)
  "Start a REPL.
REPL-NAME is used to determine the buffer name, REPL-CMD is used to
determine the shell command.  SESSION is a numeric suffix for the
buffer name."
  (let ((repl-buffer-name (format "*%s*" repl-name))
        (repl-shell (if (functionp repl-cmd)
                        (funcall repl-cmd)
                      repl-cmd)))
    (pcase-exhaustive termint-backend
      ('eat (termint--start-eat-backend repl-buffer-name repl-shell session))
      ('vterm (termint--start-vterm-backend repl-buffer-name repl-shell session))
      ('term
       (termint--start-term-backend repl-buffer-name repl-shell session)))))

(defun termint--rearrange-session-on-buffer-exit ()
  "Renumber sibling REPL sessions.
This function is called after one session is killed to maintain
consecutive ordering of REPL sessions.  The base buffer `*repl*`
without a number is considered as session 0."
  (interactive)
  (when-let* ((buffer-name (prog1 (buffer-name)
                             (rename-buffer (concat (buffer-name) "--tmp"))))
              (repl-name (and (string-match "^\\*\\(.*\\)\\*" buffer-name)
                              (match-string 1 buffer-name)))
              (sessions (seq-filter
                         #'get-buffer
                         (cons (format "*%s*" repl-name)
                               (mapcar
                                (lambda (x) (format "*%s*<%d>" repl-name x))
                                (number-sequence 1 9))))))
    (cl-loop for session in sessions
             for idx from 0
             do (with-current-buffer session
                  (rename-buffer
                   (if (eq 0 idx)
                       (rename-buffer (format "*%s*" repl-name))
                     (rename-buffer (format "*%s*<%d>" repl-name idx))))))))

(defun termint--start-term-backend (repl-buffer-name repl-shell session)
  "Start REPL-SHELL in REPL-BUFFER-NAME with numeric SESSION with term backend."
  (require 'term)
  (setq session (termint--get-session-suffix session))
  (let ((term-buffer-name
         (if session
             (format "%s<%d>" repl-buffer-name session)
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
          (term-char-mode)
          (termint-mode 1))
        (pop-to-buffer term-buffer)))))

(defun termint--start-eat-backend (repl-buffer-name repl-shell session)
  "Start REPL-SHELL in REPL-BUFFER-NAME with numeric SESSION with eat backend."
  (require 'eat)
  (setq session (termint--get-session-suffix session))
  (let* ((eat-buffer-name repl-buffer-name)
         (eat-shell repl-shell)
         (buffer (eat eat-shell session)))
    (with-current-buffer buffer
      (termint-mode 1))
    buffer))

(defun termint--start-vterm-backend (repl-buffer-name repl-shell session)
  "Start REPL-SHELL in REPL-BUFFER-NAME with numeric SESSION using vterm."
  (require 'vterm)
  (setq session (termint--get-session-suffix session))
  (let* ((vterm-buffer-name repl-buffer-name)
         (vterm-shell repl-shell)
         (buffer (vterm session)))
    (with-current-buffer buffer
      (termint-mode 1))
    buffer))

(define-minor-mode termint-mode
  "Minor mode for REPL buffers managed by termint."
  :init-value nil
  (if termint-mode
      (add-hook 'kill-buffer-hook
                #'termint--rearrange-session-on-buffer-exit
                nil t)
    (remove-hook 'kill-buffer-hook
                 #'termint--rearrange-session-on-buffer-exit
                 t)))



(defun termint--send-string
    (string
     repl-name
     session
     start-pattern
     end-pattern
     bracketed-paste-p
     str-process-func
     send-delayed-final-ret)
  "Send STRING to a REPL.
The target REPL buffer is specified by REPL-NAME and SESSION.
Additional parameters—START-PATTERN, END-PATTERN, BRACKETED-PASTE-P,
STR-PROCESS-FUNC, and SEND-DELAYED-FINAL-RET—are variables associated
with REPL-NAME, initialized during each `termint-define' call."
  (setq session (termint--get-session-suffix session))
  (let* ((repl-buffer-name
          (if session
              (format "*%s*<%d>" repl-name session)
            (format "*%s*" repl-name)))
         (send-string
          (pcase termint-backend
            ('eat #'termint--send-string-eat-backend)
            ('vterm #'termint--send-string-vterm-backend)
            ('term #'termint--send-string-term-backend)))
         (multi-lines-p (string-match-p "\n" string))
         (bracketed-paste-start "\e[200~")
         (bracketed-paste-end "\e[201~")
         (string (funcall str-process-func string))
         (start-pattern (cond ((stringp start-pattern) start-pattern)
                              (multi-lines-p (plist-get start-pattern :multi-lines))
                              (t (plist-get start-pattern :single-line))))
         (end-pattern (cond ((stringp end-pattern) end-pattern)
                            (multi-lines-p (plist-get end-pattern :multi-lines))
                            (t (plist-get end-pattern :single-line))))
         (final-string
          (if multi-lines-p
              (concat start-pattern
                      (and bracketed-paste-p bracketed-paste-start)
                      string
                      (and bracketed-paste-p bracketed-paste-end)
                      end-pattern)
            (concat start-pattern string end-pattern))))
    (funcall send-string repl-buffer-name final-string)
    (when send-delayed-final-ret
      (run-with-timer 0.3 nil
                      (lambda ()
                        (funcall send-string repl-buffer-name "\r"))))))

(defun termint--show-source-command-hint (repl-name session original-content source-command)
  "Display the hint of SOURCE-COMMAND.
The target REPL buffer is specified by REPL-NAME and SESSION.  The
hint of the SOURCE-COMMAND is the first non-empty line of
ORIGINAL-CONTENT.  The hint will be displayed as overlay in the end of
line of that matches SOURCE-COMMAND."
  (when-let*
      ((repl-buffer-name (if session
                             (format "*%s*<%d>" repl-name session)
                           (format "*%s*" repl-name)))
       (first-non-empty-line (lambda (string)
                               (seq-find (lambda (line) (not (string-empty-p (string-trim line))))
                                         (split-string string "\n"))))
       (original-content (funcall first-non-empty-line original-content))
       (original-content (string-trim original-content))
       (source-command (funcall first-non-empty-line source-command))
       (source-command (string-trim source-command)))
    (run-with-idle-timer
     0.4 nil
     (lambda ()
       (with-current-buffer repl-buffer-name
         (save-excursion
           (goto-char (point-max))
           (when (search-backward source-command nil t nil)
             (let ((ov (make-overlay (pos-eol) (pos-eol))))
               (overlay-put ov 'after-string
                            (propertize (format " %s - %s"
                                                (format-time-string "%H:%M:%S")
                                                original-content)
                                        'face 'termint-source-command-hint-face))))))))))

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

(defun termint--dispatch-paragraph ()
  "Return the beginning and end positions of the current paragraph."
  (save-excursion
    (let ((beg (progn (backward-paragraph) (point)))
          (end (progn (forward-paragraph) (point))))
      (cons beg end))))

(defun termint--dispatch-buffer ()
  "Return the beginning and end positions of the current buffer."
  (cons (point-min) (point-max)))

(defun termint--dispatch-defun ()
  "Return the beginning and end positions of the current defun."
  (save-excursion
    (if-let*
        ((beg (and
               (beginning-of-defun)
               (point)))
         (end (progn
                (end-of-defun)
                (point))))
        (cons beg end)
      (message "No defun found at point")
      nil)))

(defun termint--dispatch-region-and-send
    (dispatcher repl-name session source-syntax)
  "Get region via DISPATCHER, optionally transform for sourcing, and send.
DISPATCHER is a function returning a (BEG . END) cons cell for the
code region.  REPL-NAME is the repl name.  SESSION is the number for
the target REPL session.  If SOURCE-SYNTAX is non-nil, transform the
region's text using SOURCE-SYNTAX via `termint--create-source-command'
before sending."
  (if-let*
      ((region (funcall dispatcher))
       (beg (car region))
       (end (cdr region))
       (send-string-func (intern (concat "termint-" repl-name "-send-string")))
       (string (buffer-substring-no-properties beg end)))
      (progn
        (when source-syntax
          (setq string (termint--create-source-command string source-syntax)))
        (funcall send-string-func string session))
    (message "Invalid region from dispatcher - nothing sent to REPL")))

(defun termint--hide-window (repl-name session)
  "Hide the REPL window.
The target REPL buffer is specified by REPL-NAME and SESSION."
  (setq session (termint--get-session-suffix session))
  (when-let* ((buffer-name
               (if session (format "*%s*<%d>" repl-name session)
                 (format "*%s*" repl-name)))
              (buf (get-buffer buffer-name))
              (buffer-window (get-buffer-window buf)))
    (delete-window buffer-window)))

(defun termint--create-source-command (str source-syntax)
  "Create the \"source\" to send to REPL.
STR contains the selected code content to be processed. The
SOURCE-SYNTAX is the `termint-REPL-NAME-source-syntax' associated with
REPL-NAME."
  (if (stringp source-syntax)
      (let ((file (termint--make-tmp-file str)))
        (replace-regexp-in-string "{{file}}" file source-syntax))
    (funcall source-syntax str)))



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

`:bracketed-paste-p' whether send the string with bracketed paste
mode, the default value is nil.  You can change the behavior at run
time by setting the generated variable
`termint-REPL-NAME-use-bracketed-paste-mode'.

`:start-pattern' the first string to send to the REPl before sending
the region.  The default is \"\".  You can change the behavior at run
time by setting the generated variable
`termint-REPL-NAME-start-pattern'.  Additionally, the value can be a
plist with two attributes: `:single-line' for specifying the string in
single-line scenarios.`:multi-lines' for defining the string in
multi-line contexts.

`:end-pattern' the string appended to the code before sending it to
the REPL.  The default value is `(kbd \"RET\")'.  You can change the
behavior at run time by setting the generated variable
`termint-REPL-NAME-end-pattern'.  Additionally, the value can be a
plist with two attributes: `:single-line' for specifying the string in
single-line scenarios.`:multi-lines' for defining the string in
multi-line contexts.

While `(kbd \"RET\")' is generally a good default for `:end-pattern',
some REPLs, when using bracketed paste with multi-lines input,
automatically execute the code without needing a final newline.  In
these cases, set `:end-pattern' to `(:single-line ,(kbd \"RET\")
:multi-lines \"\").

`:str-process-func' the function to process the string before sending
it to the REPL.  The default is `identity'.  You can change the
behavior at run time by setting the generated variable
`termint-REPL-NAME-str-process-func'.

`:source-syntax:' The function or syntax (specified as a string) that
determines how code is sourced into the REPL.

When provided as a function, it takes a string representing the
selected code region and returns a string as the source syntax.  A
common implementation involves writing the input code to a temporary
file and generating a language-specific command to source that file.
By default, `:source-syntax' uses the `identity' function, which
directly sends the input string to the REPL without file operations.

When specified as a string, it should represent the sourcing command
in the target language.  For example, in Python:

exec(compile(open(\"{{file}}\", \"r\").read(), \"{{file}}\",\"exec\"))

Here, `{{file}}` serves as a placeholder for the temporary file path,
which is substituted at runtime.

Use the string format for concise configuration, or the function
variant for greater flexibility and control.

`:show-source-command-hint' Display the first non-empty line of the
code chunk as overlay.  Alongside the source command sent to the REPL,
providing a useful hint about the actual command being executed. the
default value is nil.

`:send-delayed-final-ret' Send the final return with a slight delay.
Some REPLs may not properly recognize when a large chunk of text sent
with bracketed paste mode has finished being input and needs to be
evaluated.  Normally, a final return character signals the REPL to
execute the command, but certain REPLs require a brief delay before
this final return to properly process the bracketed paste input.  This
option should generally remain false (the default), with Claude Code
being a notable exception that requires it to be set to true.
Contributions are welcome if other REPLs are found to need this option
enabled.  The default value is nil."

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
        (source-syntax (or (plist-get args :source-syntax) ''identity))
        (show-source-command-hint (plist-get args :show-source-command-hint))
        (send-delayed-final-ret (plist-get args :send-delayed-final-ret))
        (repl-cmd-name (intern (concat "termint-" repl-name "-cmd")))
        (str-process-func-name (intern (concat "termint-" repl-name "-str-process-func")))
        (source-syntax-name (intern (concat "termint-" repl-name "-source-syntax")))
        (show-source-command-hint-name (intern (concat "termint-" repl-name "-show-source-command-hint")))
        (bracketed-paste-p-name (intern (concat "termint-" repl-name "-use-bracketed-paste-mode")))
        (start-pattern-name (intern (concat "termint-" repl-name "-start-pattern")))
        (end-pattern-name (intern (concat "termint-" repl-name "-end-pattern")))
        (send-delayed-final-ret-name (intern (concat "termint-" repl-name "-send-delayed-final-ret")))
        (region-definitions
         (cl-loop for entry in termint-region-dispatchers
                  for region-name = (car entry)
                  for dispatcher = (cdr entry)
                  for send-func-name = (intern (format "termint-%s-send-%s" repl-name region-name))
                  for source-func-name = (intern (format "termint-%s-source-%s" repl-name region-name))
                  collect (list :name region-name
                                :dispatcher dispatcher
                                :send send-func-name
                                :source source-func-name)))
        (custom-command-definitions
         (cl-loop for entry in termint-schema-custom-commands
                  for name = (car entry)
                  for func = (cdr entry)
                  for command-name = (intern (format "termint-%s-%s" repl-name name))
                  collect (list :name name
                                :func func
                                :command command-name))))

    `(progn

       (defvar ,repl-cmd-name ,repl-cmd
         ,(format "The shell command for the %s REPL." repl-name))

       (defvar ,str-process-func-name ,str-process-func
         ,(format "The function to process the string before sending it to the %s REPL." repl-name))

       (defvar ,source-syntax-name ,source-syntax
         ,(format "The syntax to source the code content for the %s REPL." repl-name))

       (defvar ,show-source-command-hint-name ,show-source-command-hint
         ,(format "Whether display the source command hint in %s REPL." repl-name))

       (defvar ,bracketed-paste-p-name ,bracketed-paste-p
         ,(format "Whether use bracketed paste mode for sending string to the %s REPL." repl-name))

       (defvar ,start-pattern-name ,start-pattern
         ,(format "The first string to send to the %s REPL before sending the text." repl-name))

       (defvar ,end-pattern-name ,end-pattern
         ,(format "The last string to send to the %s REPL after sending the text." repl-name))

       (defvar ,send-delayed-final-ret-name ,send-delayed-final-ret
         ,(format "Whether to send the final return with a slight delay for the %s REPL." repl-name))

       (defun ,start-func-name (&optional session)
         ,(format
           "Create a %s REPL buffer.
Start a new %s session or switch to an already active session. Return
the buffer selected (or created). With a numeric prefix SESSION,
create or switch to the session with that number as a suffix."
           repl-name repl-name)
         (interactive "P")
         (termint--start ,repl-name ,repl-cmd-name session))

       (defun ,send-region-func-name (beg end &optional session)
         ,(format
           "Send the region delimited by BEG and END to %s.
With numeric prefix SESSION, send region to the process associated
with that number." repl-name)
         (interactive "r\nP")
         (let ((str (buffer-substring-no-properties beg end)))
           (,send-string-func-name str session)))

       (defun ,source-region-func-name (beg end &optional session)
         ,(format
           "Source the region delimited by BEG and END to %s.
With numeric prefix SESSION, send region to the process associated
with that number." repl-name)
         (interactive "r\nP")
         (let* ((str (buffer-substring-no-properties beg end))
                (orig-str str)
                (str (termint--create-source-command str ,source-syntax-name)))
           (,send-string-func-name str session)
           (when ,show-source-command-hint-name
             (termint--show-source-command-hint ,repl-name session orig-str str))))

       (defun ,send-string-func-name (string &optional session)
         ,(format
           "Send the STRING to %s.
When invoked interactively, prompt for user input in the minibuffer.
If a numeric prefix SESSION is provided, send the string to the
process with that number."
           repl-name)
         (interactive "sinput your command: \nP")
         (termint--send-string string
                               ,repl-name
                               session
                               ,start-pattern-name
                               ,end-pattern-name
                               ,bracketed-paste-p-name
                               ,str-process-func-name
                               ,send-delayed-final-ret-name))

       ,@(cl-loop for entry in region-definitions append
                  (list
                   `(defun ,(plist-get entry :send) (&optional session)
                      ,(format
                        "Send the current %s to %s.
With numeric prefix SESSION, send %s to the process associated
with that number."
                        (plist-get entry :name) repl-name (plist-get entry :name))
                      (interactive "P")
                      (termint--dispatch-region-and-send
                       #',(plist-get entry :dispatcher) ,repl-name session nil))
                   `(defun ,(plist-get entry :source) (&optional session)
                      ,(format
                        "Source the current %s to %s.
With numeric prefix SESSION, send %s to the process associated
with that number."
                        (plist-get entry :name) repl-name (plist-get entry :name))
                      (interactive "P")
                      (termint--dispatch-region-and-send
                       #',(plist-get entry :dispatcher) ,repl-name
                       session ,source-syntax-name))))

       (when (require 'evil nil t)
         (evil-define-operator ,send-region-operator-name (beg end session)
           ,(format
             "Evil operator for sending text objects, motions, or regions to %s.
With a numeric prefix SESSION, send the region to the %s process
associated with that number" repl-name repl-name)
           :move-point nil
           (interactive "<r>P")
           (,send-region-func-name beg end session))

         (evil-define-operator ,source-region-operator-name (beg end session)
           ,(format
             "Evil operator for sourcing text objects, motions, or regions to %s.
With a numeric prefix SESSION, send the region to the %s process
associated with that number" repl-name repl-name)
           :move-point nil
           (interactive "<r>P")
           (,source-region-func-name beg end session)))

       (defun ,hide-window-func-name (&optional session)
         ,(format
           "hide the %s window.
With numeric prefix SESSION, hide the window with that number as a
suffix." repl-name)
         (interactive "P")
         (termint--hide-window ,repl-name session))

       ,@(cl-loop for entry in custom-command-definitions append
                  (let ((suffix (plist-get entry :name))
                        (fn (plist-get entry :func))
                        (command (plist-get entry :command)))
                    (list
                     `(defun ,command (&optional session)
                        ,(format
                          "Run custom command `%s' for %s.
Generated from `termint-schema-custom-commands'.
Calls `%S' with REPL-NAME and optional SESSION."
                          suffix repl-name fn)
                        (interactive "P")
                        (funcall #',fn ,repl-name session)))))

       (defvar ,keymap-name
         (let ((map (make-sparse-keymap)))
           (define-key map "s" #',start-func-name)
           (define-key map "r" #',send-region-func-name)
           (define-key map "R" #',source-region-func-name)
           (define-key map "e" #',send-string-func-name)
           (define-key map "h" #',hide-window-func-name)
           (when (require 'evil nil t)
             (define-key map "r" #',send-region-operator-name)
             (define-key map "R" #',source-region-operator-name))
           (dolist (binding termint-mode-map-additional-keys)
             (let ((key (car binding))
                   (suffix (cdr binding)))
               (when-let* ((command (intern-soft (format "termint-%s-%s"
                                                         ,repl-name suffix)))
                           ((fboundp command)))
                 (define-key map key command))))
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
If you plan to use PDB with functions sourced from a temporary file,
ensure that the file is not deleted.  In these cases, pass
`:source-syntax' as a function.  When the `:source-syntax' is supplied
as a string, the temporary file will be automatically deleted
afterward.")



(defvar termint-ipython-source-syntax-template
  "%run -i \"{{file}}\""
  "The template syntax used to source code content into the iPython REPL.
If you plan to use PDB with functions sourced from a temporary file,
ensure that the file is not deleted.  In these cases, pass
`:source-syntax' as a function.  When the `:source-syntax' is supplied
as a string, the temporary file will be automatically deleted
afterward.")

(defvar termint-R-source-syntax-template
  "eval(parse(text = readr::read_file(\"{{file}}\")))"
  "The template syntax used to source code content into the R REPL.")

(defvar termint-bash-source-syntax-template "source {{file}}"
  "The template syntax used to source code content into the Bash REPL.")

(provide 'termint)
;;; termint.el ends here
