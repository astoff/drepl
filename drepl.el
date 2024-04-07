;;; drepl.el --- REPL protocol for the dumb terminal   -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Free Software Foundation, Inc.

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: languages, processes
;; URL: https://github.com/astoff/drepl
;; Package-Requires: ((emacs "29.1") (comint-mime "0.3"))
;; Version: 0.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; dREPL is a collection of fully featured language shells for Emacs
;; built on top of Comint.  It can be described as a REPL protocol for
;; the dumb terminal.  This file provides a base protocol
;; implementation as well as the user interface.  The other files in
;; this package define REPLs for specific interpreters.  Each of them
;; is comprised of one Elisp file defining a REPL subclass and one
;; file in the target programming language implementing the server
;; side of the protocol.
;;
;; See the package README for a description of the communication
;; protocol that needs to be implemented on the interpreter side.

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'project)
(eval-when-compile
  (require 'derived)
  (require 'subr-x))

;;; Variables and customization options

(defgroup drepl nil
  "REPL protocol for the dumb terminal."
  :group 'comint
  :link '(url-link "https://github.com/astoff/drepl"))

(defface drepl-prompt-incomplete '((t :inherit (comint-highlight-prompt default)))
  "Face for continuation prompts when input is incomplete but valid.")

(defface drepl-prompt-invalid '((t :inherit (error default)))
  "Face for continuation prompts when input is invalid.")

(defcustom drepl-directory #'drepl--project-directory
  "Directory in which to run the REPL process.
It can be a string (the directory name) or a function returning a
directory name.  The function should accept one argument ASK
which determines whether to ask or return nil when in doubt."
  :type '(choice string function))

(defvar-local drepl--current nil
  "The dREPL associated to the current buffer.
In dREPL buffers, this is the dREPL object itself.  In all other
buffers, this is a dREPL buffer or nil.")

(defvar drepl--log-buffer nil
  "Name of the event log buffer, or nil to disable logging.")

;;; Basic definitions

(cl-defstruct (drepl-base
               (:constructor nil)
               (:copier nil)
               (:conc-name drepl--))
  "Base dREPL structure, to be inherited by implementations."
  (buffer nil :read-only t :documentation "\
The buffer where the REPL is running.")
  (status nil :documentation "\
The last reported interpreter status.")
  (last-id 0 :documentation "\
The id of the last request sent.")
  (callbacks nil :documentation "\
Alist of (ID . CALLBACK) keeping track of requests sent but not
yet replied to.")
  (pending nil :documentation "\
List of requests pending to be sent."))

(defun drepl--process (repl)
  "The underlying process of dREPL object REPL."
  (get-buffer-process (drepl--buffer repl)))

(cl-defmacro drepl--define (name &key display-name docstring extra-slots)
  "Define a REPL type.
NAME is a symbol to name the struct type identifying the REPL as
well as the interactive command to launch it.

DISPLAY-NAME is a string used to generate the default buffer name
of REPL instances, among other things.

DOCSTRING is the docstring of the command NAME.

EXTRA-SLOTS is a list of slots passed to `cl-defstruct' in
addition to those of `drepl-base'."
  (let* ((display-name (or display-name (symbol-name name)))
         (conc-name (intern (format "%s--" name))))
    `(progn
       (defun ,name ()
         ,(or docstring
              (format "Start the %s interpreter." display-name))
         (interactive)
         (pop-to-buffer (drepl--get-buffer-create ',name t)
                        display-comint-buffer-action))
       (cl-defstruct (,name
                      (:include drepl-base)
                      (:copier nil)
                      (:constructor nil)
                      (:constructor ,(intern (format "%screate" conc-name)))
                      (:conc-name ,conc-name))
         ,(format "Structure keeping the state of a %s REPL." display-name)
         ,@extra-slots)
       (put ',name 'drepl--display-name ,display-name))))

(defun drepl--log-message-1 (&rest args)
  "Helper function for `drepl--log-message'.
ARGS is the entire argument list of `drepl--log-message'."
  (with-current-buffer (get-buffer-create drepl--log-buffer)
    (goto-char (point-max))
    (when-let ((w (get-buffer-window)))
      (set-window-point w (point)))
    (insert (propertize (format-time-string "[%T] ") 'face 'error)
            (apply #'format args)
            ?\n)))

(defmacro drepl--log-message (string &rest args)
  "Write a message to buffer pointed by `drepl--log-buffer', if non-nil.
The message is formed by calling `format' with STRING and ARGS."
  `(when drepl--log-buffer (drepl--log-message-1 ,string ,@args)))

;;; Communication protocol

(defalias 'drepl--json-decode
  (if (json-available-p)
      (lambda (s)
        (json-parse-string s :object-type 'alist :null-object nil))
    (error "Not implemented")))

(defalias 'drepl--json-encode
  (if (json-available-p)
      (lambda (s) (json-serialize s :null-object nil))
    (error "Not implemented")))

(defconst drepl--state-transitions
  '((eval . rawio))
  "Alist mapping a REPL operation to a new REPL state.
The default new state is busy.")

(cl-defgeneric drepl--send-request (repl data)
  "Send request data to REPL.
REPL must be in `ready' state and transitions to `busy' state.
DATA is a plist containing the request arguments, as well as :op
and :id entries."
  (setf (drepl--status repl) (alist-get (plist-get data :op)
                                        drepl--state-transitions
                                        'busy nil #'string-equal))
  (let* ((proc (drepl--process repl))
         (maxlen (when (process-tty-name proc)
                   (- comint-max-line-length 3))))
    (named-let recur ((last t)
                      (s (drepl--json-encode data)))
      (if (and maxlen (< maxlen (string-bytes s)))
          (let ((i (/ (length s) 2)))
            (recur nil (substring s 0 i))
            (recur last (substring s i)))
        (drepl--log-message "send msg %s" s)
        (process-send-string proc (format "\e%s%s\n" (if last "=" "+") s))))))

(defun drepl--communicate (repl callback op &rest args)
  "Send a request to REPL.

OP is the operation name as as symbol and ARGS is a plist of
arguments for that operation.

When REPL responds, CALLBACK is called with one argument, the
response data.  This functions returns immediately with the id
number of the request.

CALLBACK may also be the symbol `sync' to make a synchronous
request.  In this case, the REPL must be in the state `ready',
and this function returns the response data directly."
  (if (eq callback 'sync)
      (progn (unless (eq (drepl--status repl) 'ready)
               (user-error "%s is busy" repl))
             (let* ((result :pending)
                    (cb (lambda (data) (setq result data))))
               (apply #'drepl--communicate repl cb op args)
               (while (eq result :pending) (accept-process-output))
               result))
    (let* ((id (cl-incf (drepl--last-id repl)))
           (data `(:id ,id :op ,(symbol-name op) ,@args)))
      (push (cons id callback) (if-let ((reqs (drepl--callbacks repl)))
                                   (cdr reqs)
                                 (drepl--callbacks repl)))
      (if (eq 'ready (drepl--status repl))
          (drepl--send-request repl data)
        (push (cons id data) (drepl--pending repl)))
      id)))

(defun drepl--osc-handler (_cmd text)
  "Function intended for use as an entry of `ansi-osc-handlers'.
TEXT is a still unparsed message received from the interpreter."
  (drepl--log-message "read %s" text)
  (drepl--handle-response drepl--current (drepl--json-decode text)))

(defun drepl--handle-response (repl data)
  "React to message DATA coming from the REPL process."
  (let* ((id (alist-get 'id data))
         (callback (if id
                       (prog1
                           (alist-get id (drepl--callbacks repl))
                         (setf (alist-get id (drepl--callbacks repl)
                                          nil 'remove)
                               nil))
                     (apply-partially #'drepl--handle-notification
                                      repl))))
    (when-let ((nextreq (and (eq (drepl--status repl) 'ready)
                             (pop (drepl--pending repl)))))
      (drepl--send-request repl nextreq))
    (when callback
      (funcall callback data))))

(cl-defgeneric drepl--handle-notification (repl data)
  "Method called when REPL sends a notification.
DATA is the content of the message."
  (pcase (alist-get 'op data)
    ("status" (setf (drepl--status repl)
                    (intern (alist-get 'status data))))
    ("getoptions"
     (setf (drepl--status repl) 'ready)
     (drepl--set-options repl data))
    ("log" (drepl--log-message "log:%s: %s"
                               (buffer-name)
                               (alist-get 'text data)))))

;;; Buffer association

(defun drepl--get-repl (&optional status ensure)
  "Return a REPL associated to the current buffer, or nil if none.

This is either the REPL pointed by `drepl--current' or a REPL
running in some parent of `default-directory' if there is exactly
one such choice.

If STATUS is non-nil, additionally check that the REPL is alive
and has the given status.

If ENSURE is non-nil, produce an error if there is no REPL
associated to the current buffer."
  (let ((repl (cond
               ((drepl-base-p drepl--current) drepl--current)
               ((buffer-live-p drepl--current)
                (buffer-local-value 'drepl--current drepl--current))
               (t (let* ((dir default-directory)
                         (buffers (seq-filter
                                   (lambda (buffer)
                                     (with-current-buffer buffer
                                       (and (derived-mode-p 'drepl-mode)
                                            (file-in-directory-p
                                             dir default-directory))))
                                   (buffer-list))))
                  (when (length= buffers 1)
                    (buffer-local-value 'drepl--current (car buffers))))))))
    (when (and ensure (not repl))
      (user-error (substitute-command-keys
                   "No default REPL, use \\[drepl-associate] to choose one")))
    (when (or (not status)
              (and repl
                   (drepl--process repl)
                   (memq (process-status (drepl--process repl))
                         '(run open))
                   (eq status (drepl--status repl))))
      repl)))

(defun drepl--read-buffer (prompt)
  "Read the name of a REPL buffer using PROMPT."
  (read-buffer prompt
               (when (buffer-live-p drepl--current)
                 drepl--current)
               t
               (lambda (b)
                 (with-current-buffer (car b)
                   (derived-mode-p 'drepl-mode)))))

(defun drepl-associate ()
  "Associate a REPL to the current buffer.

Commands like `drepl-eval' will then target the selected REPL.

If a target REPL has not yet been chosen explicitly through this
command, the target REPL is selected automatically as long as
there is exactly one REPL buffer running in the current directory
or one of its parents.

With a prefix argument, remove an explicit REPL association."
  (interactive)
  (when (derived-mode-p 'drepl-mode)
    (user-error "Can't associate another REPL buffer to a REPL"))
  (let ((buffer (unless current-prefix-arg
                  (drepl--read-buffer "Associate REPL to this buffer: "))))
    (setq drepl--current (and buffer (get-buffer buffer)))))

(defun drepl-pop-to-repl (ask)
  "Pop to the REPL associated to the current buffer.
With a prefix argument or non-nil ASK argument, choose a REPL
interactively."
  (interactive "P")
  (pop-to-buffer
   (if ask
       (drepl--read-buffer "Pop to REPL: ")
     (drepl--buffer (drepl--get-repl nil t)))
   display-comint-buffer-action))

;;; Complete operation

(defun drepl--capf-annotate (cand)
  "Return an annotation for completion candidate CAND."
  (get-text-property 0 'drepl--annot cand))

(cl-defgeneric drepl--completion-bounds (_repl)
  "Return the start and end of completion region as a cons cell."
  (or (bounds-of-thing-at-point 'symbol)
      (cons (point) (point))))

(defun drepl--completion-cadidates (repl code pos)
  "Ask REPL for possible completions of CODE with point at POS."
  (let ((response (while-no-input
                    (drepl--communicate repl 'sync 'complete
                                        :code code
                                        :pos pos))))
    (when (listp response)
      (mapcar (lambda (c)
                (let-alist c
                  (propertize .text 'drepl--annot .annot)))
              (alist-get 'candidates response)))))

(defun drepl--complete ()
  "Function intended for use as a member of `completion-at-point-functions'."
  (when-let ((repl (when (derived-mode-p 'drepl-mode)
                     (drepl--get-repl 'ready)))
             (bounds (drepl--completion-bounds repl))
             (code (buffer-substring-no-properties
                    (cdr comint-last-prompt)
                    (point-max)))
             (pos (- (point) (cdr comint-last-prompt)))
             (cands (when (>= pos 0)
                      (drepl--completion-cadidates repl code pos)))
             (metadata '(metadata
                         (category . drepl)
                         (annotation-function . drepl--capf-annotate)))
             (coll (lambda (string predicate action)
                     (if (eq action 'metadata)
                         metadata
                       (complete-with-action action cands string predicate)))))
    `(,(car bounds) ,(cdr bounds) ,coll)))

;;; Eval operation

(cl-defgeneric drepl--eval (repl code)
  "Send an eval request to REPL with CODE as argument."
  (drepl--communicate repl #'ignore 'eval :code code))

(defun drepl--send-string (proc string)
  "Like `comint-send-string', but check the REPL status first.
If it is `rawio', then simply send the raw STRING to process PROC.
Otherwise, make an eval request."
  (let ((repl (with-current-buffer
                  (if proc (process-buffer proc) (current-buffer))
                (drepl--get-repl nil t))))
    (if (not (eq (drepl--status repl) 'rawio))
        (drepl--eval repl string)
      (drepl--log-message "send raw %s" string)
      (comint-simple-send proc string))))

(defun drepl-eval (code)
  "Evaluate CODE string in the current buffer's REPL."
  (interactive (list (read-from-minibuffer "Evaluate: ")))
  (drepl--eval (drepl--get-repl nil t) code))

(defun drepl-eval-region (start end)
  "Evaluate region in the current buffer's REPL.
START and END are the region bounds."
  (interactive "r")
  (drepl-eval (buffer-substring-no-properties start end)))

(defun drepl-eval-buffer (&optional buffer)
  "Evaluate BUFFER in the REPL associated to the current buffer.
By default, BUFFER is the current buffer.  Interactively, select a
buffer first if a prefix argument is given or the current buffer
is a dREPL buffer."
  (interactive (list (when (or current-prefix-arg
                               (derived-mode-p 'drepl-mode))
                       (read-buffer "Evaluate buffer: "))))
  (with-current-buffer (or buffer (current-buffer))
    (drepl-eval-region (point-min) (point-max))))

(defun drepl-send-input-maybe (&optional force) ;Change this to `newline', with opposite logic
  "Like `comint-send-input', but first check if input is complete.
If the input is incomplete or invalid code and FORCE is nil,
insert start a continuation line instead."
  (interactive "P")
  (unless (derived-mode-p 'drepl-mode)
    (user-error "Can't run this command here"))
  (let-alist (when-let ((repl (unless force (drepl--get-repl 'ready)))
                        (pmark (process-mark (drepl--process repl)))
                        (code (and (>= (point) pmark)
                                   (buffer-substring-no-properties
                                    pmark (field-end)))))
               (drepl--communicate drepl--current 'sync 'checkinput
                                   :code code))
    (pcase-exhaustive .status
      ((or (and "incomplete" (let face 'drepl-prompt-incomplete))
           (and "invalid" (let face 'drepl-prompt-invalid)))
       (let ((prompt (propertize (or .prompt "") 'font-lock-face face)))
         (insert (propertize "\n" 'display (concat " \n" prompt))
                 (or .indent ""))))
      ((or "complete" 'nil)
       (comint-send-input)))))

;;; Describe operation

(cl-defgeneric drepl--format-eldoc (repl data)
  "Format REPL response DATA to a `describe' operation.
The return value is passed directly to an Eldoc callback.  See
`eldoc-documentation-functions' for details."
  (ignore repl)
  (let-alist data
    (list
     (with-temp-buffer
       (when .type
        (insert .type))
       (insert "\n\n")
       (when .text (insert .text))
       (when .file
         (goto-char (point-min))
         (unless (search-forward .file nil t)
           (goto-char (point-max))
           (insert "\nDefined in " .file))
         (buttonize-region (- (point) (length .file)) (point)
                           #'find-file .file))
       (ansi-color-apply (buffer-string)))
     :thing .name)))

(defun drepl--eldoc-function (callback &rest _)
  "Function intended for use as a member of `eldoc-documentation-functions'.
See that variable's docstring for a description of CALLBACK."
  (when-let ((repl (when (derived-mode-p 'drepl-mode)
                     (drepl--get-repl 'ready)))
             (start (cdr comint-last-prompt))
             (pos (- (point) start))
             (code (when (>= pos 0)
                     (buffer-substring-no-properties start (point-max))))
             (cb (lambda (data)
                   (apply callback (drepl--format-eldoc repl data)))))
    (drepl--communicate repl cb 'describe :code code :pos pos)))

;;; REPL restart

(cl-defgeneric drepl--restart (repl hard)
  "Generic method to restart a REPL.
HARD should be as described in `drepl-restart', but it not used
in the default implementation."
  (ignore hard)
  (with-current-buffer (drepl--buffer repl)
    (when-let ((proc (drepl--process repl)))
      (kill-process proc)
      (while (accept-process-output proc)))
    (drepl--get-buffer-create (type-of repl) nil)))

(defun drepl-restart (&optional hard)
  "Restart the current REPL.
Some REPLs by default perform a soft reset by deleting all user
variables without killing the interpreter.  In those cases, a
prefix argument or non-nil HARD argument can be used to force a
hard reset."
  (interactive "P")
  (drepl--restart (drepl--get-repl nil t) hard))

;;; REPL initialization

(defun drepl--project-directory (may-prompt)
  "Return the current project root directory.
If it can be determined and MAY-PROMPT is non-nil, ask for a
project; otherwise fall back to `default-directory'."
  (if-let ((proj (project-current may-prompt)))
      (project-root proj)
    default-directory))

(defun drepl--buffer-name (type directory)
  "Buffer name for a REPL of the given TYPE running in DIRECTORY."
  (format "%s/*%s*"
          (file-name-nondirectory
           (directory-file-name directory))
          (or (get type 'drepl--display-name) type)))

(defun drepl--get-buffer-create (type may-prompt)
  "Get a buffer running a REPL of the given TYPE, creating one if needed.
The directory of the buffer is determined by `drepl-directory'.
If MAY-PROMPT is non-nil, allow an interactive query if needed."
  (let ((buffer
         (if (eq type (type-of drepl--current))
             (drepl--buffer drepl--current)
           (let ((default-directory (if (stringp drepl-directory)
                                        drepl-directory
                                      (funcall drepl-directory may-prompt))))
             (get-buffer-create (drepl--buffer-name type default-directory))))))
    (unless (comint-check-proc buffer)
      (with-current-buffer buffer
        (let ((repl (funcall (intern (format "%s--create" type))
                             :buffer buffer)))
          (drepl--log-message "starting %s" buffer)
          (drepl-mode)
          (drepl--init repl)
          (setq drepl--current repl))))
    buffer))

(cl-defgeneric drepl--command (repl)
  "The command to start the REPL interpreter, as a list of strings.")

(cl-defgeneric drepl--init (repl)
  "Initialization code for REPL.
This function is called in the REPL buffer after `drepl-mode' has been
activated.  It should start and initialize a Comint process."
  (pcase-let ((`(,program . ,switches) (drepl--command repl))
              (buffer (drepl--buffer repl)))
    (apply #'make-comint-in-buffer
           (buffer-name buffer)
           buffer
           program nil switches)))

(cl-defgeneric drepl--set-options (repl data)
  "Implementation of the `setoptions' operation.
This method is called when the REPL sends a `getoptions'
notification.  The REPL is in `ready' state when this happens.
The notification message is passed as DATA.")

(defun drepl--adapt-comint-to-mode (mode)
  "Set up editing in a Comint buffer to resemble major MODE.

Specifically:
- Set `comint-indirect-setup-function' to MODE.
- Set syntax table to MODE-syntax-table.

MODE can be a major mode symbol or a string used to look up an
appropriate mode using `auto-mode-alist'."
  (when (stringp mode)
    (setq mode (cdr (assoc mode auto-mode-alist #'string-match-p))))
  (when (functionp mode)
    (setq mode (alist-get mode major-mode-remap-alist mode))
    (when (autoloadp (symbol-function mode))
      (autoload-do-load (symbol-function mode) mode))
    (setq-local comint-indirect-setup-function mode)
    (when-let ((syntbl-sym (derived-mode-syntax-table-name mode))
               (syntbl-val (when (boundp syntbl-sym)
                             (symbol-value syntbl-sym))))
      (when (syntax-table-p syntbl-val)
        (set-syntax-table syntbl-val)))))

;;; Base major mode

(defvar-keymap drepl-mode-map
  :doc "Keymap for `drepl-mode'."
  :parent comint-mode-map
  "<remap> <comint-send-input>" #'drepl-send-input-maybe
  "C-c M-:" #'drepl-eval
  "C-c C-b" #'drepl-eval-buffer
  "C-c C-n" #'drepl-restart)

(define-derived-mode drepl-mode comint-mode "dREPL"
  "Major mode for the dREPL buffers."
  :interactive nil
  (add-hook 'completion-at-point-functions #'drepl--complete nil t)
  (add-hook 'eldoc-documentation-functions #'drepl--eldoc-function nil t)
  (add-hook 'comint-output-filter-functions #'comint-osc-process-output nil t)
  (push '("5161" . drepl--osc-handler) ansi-osc-handlers)
  (setq-local comint-input-sender #'drepl--send-string)
  (setq-local indent-line-function #'comint-indent-input-line-default)
  (setq-local list-buffers-directory default-directory))

(provide 'drepl)

; LocalWords:  dREPL drepl
;;; drepl.el ends here
