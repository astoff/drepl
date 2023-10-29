;;; drepl.el --- REPL protocol for the dumb terminal   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Augusto Stoffel

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: languages, processes

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
;; the dumb terminal.  This file defines the user interface and a base
;; EIEIO class for client code.  The other files in this package
;; define REPLs for specific interpreters.  Each of them is comprised
;; of one Elisp file defining a REPL subclass and one file in the
;; target programming language implementing the server side of the
;; protocol.

;; See the README for a description of the communication protocol.

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'comint)
(require 'project)
(eval-when-compile (require 'subr-x))

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
buffers, this is the dREPL buffer or nil.")

(defvar drepl--verbose nil)

;;; Basic definitions

(defclass drepl-base ()
  ((buffer :initarg :buffer :reader drepl--buffer)
   (status :initform nil :accessor drepl--status)
   (last-request-id :initform 0)
   (requests :initform nil)
   (pending :initform nil))
  :abstract t
  :documentation "Base dREPL class.")

(defun drepl--process (repl)
  "The underlying process of dREPL object REPL."
  (get-buffer-process (drepl--buffer repl)))

(defmacro drepl--message (fmtstring &rest args)
  "Write a message to *drepl-log* buffer if `drepl--verbose' is non-nil.
The message is formed by calling `format' with FMTSTRING and ARGS."
  (let ((msg (lambda (&rest args)
               (with-current-buffer (get-buffer-create "*drepl-log*")
                 (goto-char (point-max))
                 (when-let ((w (get-buffer-window)))
                   (set-window-point w (point)))
                 (insert (propertize (format-time-string "[%T] ") 'face 'error)
                         (apply #'format args)
                         ?\n)))))
    `(when drepl--verbose (funcall ,msg ,fmtstring ,@args))))

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

(cl-defgeneric drepl--send-request (repl data)
  "Send request data to REPL.
REPL must be in `ready' state and transitions to `busy' state.
DATA is a plist containing the request arguments, as well as :op
and :id entries."
  (setf (drepl--status repl) 'busy)
  (let ((encoded (drepl--json-encode data)))
    (drepl--message "send %s" encoded)
    (process-send-string (drepl--process repl)
                         (format "\e%%%s\n" encoded))))

(cl-defgeneric drepl--communicate (repl callback op &rest args)
  (if (eq callback 'sync)
      (progn (unless (eq (drepl--status repl) 'ready)
               (user-error "%s is busy" repl))
             (let* ((result :pending)
                    (cb (lambda (data) (setq result data))))
               (apply #'drepl--communicate repl cb op args)
               (while (eq result :pending) (accept-process-output))
               result))
    (let* ((id (cl-incf (oref repl last-request-id)))
           (data `(:id ,id :op ,(symbol-name op) ,@args)))
      (push (cons id callback) (if-let ((reqs (oref repl requests)))
                                   (cdr reqs)
                                 (oref repl requests)))
      (if (eq 'ready (drepl--status repl))
          (drepl--send-request repl data)
        (push (cons id data) (oref repl pending)))
      id)))

(cl-defgeneric drepl--handle-notification (repl data)
  (pcase (alist-get 'op data)
    ("status" (setf (drepl--status repl)
                    (intern (alist-get 'status data))))
    ("getoptions"
     (setf (drepl--status repl) 'ready)
     (drepl--set-options repl data))
    ("log" (drepl--message "log:%s: %s"
                           (buffer-name)
                           (alist-get 'text data)))))

(defun drepl--osc-handler (_cmd text)
  (drepl--message "read %s" text)
  (let* ((data (drepl--json-decode text))
         (id (alist-get 'id data))
         (callback (if id
                       (prog1
                           (alist-get id (oref drepl--current requests))
                         (setf (alist-get id (oref drepl--current requests)
                                          nil 'remove)
                               nil))
                     (apply-partially #'drepl--handle-notification
                                      drepl--current))))
    (when-let ((nextreq (and (eq (drepl--status drepl--current) 'ready)
                             (pop (oref drepl--current pending)))))
      (drepl--send-request drepl--current nextreq))
    (when callback
      (funcall callback data))))

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
               ((recordp drepl--current) drepl--current)
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
                   "No REPL, use \\[drepl-associate] to choose one")))
    (when (or (not status)
              (and repl
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
     (drepl--buffer (drepl--get-repl nil t)))))

;;; Complete operation

(defun drepl--capf-annotate (cand)
  "Return an annotation for completion candidate CAND."
  (get-text-property 0 'drepl--annot cand))

(cl-defgeneric drepl--completion-bounds (_repl)
  (bounds-of-thing-at-point 'symbol))

(cl-defgeneric drepl--completion-cadidates (repl code offset)
  (let ((response (while-no-input
                    (drepl--communicate repl 'sync 'complete
                                        :code code
                                        :offset offset))))
    (mapcar (lambda (c)
              (let-alist c
                (propertize .text 'drepl--annot .annot)))
            (alist-get 'candidates response))))

(defun drepl--complete ()
  (when-let ((repl (when (derived-mode-p 'drepl-mode)
                     (drepl--get-repl 'ready)))
             (bounds (drepl--completion-bounds repl))
             (code (buffer-substring-no-properties
                    (cdr comint-last-prompt)
                    (point-max)))
             (offset (- (point) (cdr comint-last-prompt)))
             (cands (when (>= offset 0)
                      (drepl--completion-cadidates repl code offset)))
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
  (drepl--communicate repl #'ignore 'eval :code code))

(defun drepl--send-string (proc string)
  "Like `comint-send-string', but check whether PROC's status is `ready'.
If it is, then make an eval request, otherwise just send the raw
STRING to the process."
  (let ((repl (with-current-buffer
                  (if proc (process-buffer proc) (current-buffer))
                (drepl--get-repl 'ready))))
    (if repl
        (drepl--eval repl string)
      (drepl--message "send %s" string)
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
       (let* ((prompt (thread-first
                       .prompt
                       (or "")
                       (propertize 'font-lock-face face))))
         (insert (propertize "\n" 'display (concat " \n" prompt))
                 .indent)))
      ((or "complete" 'nil)
       (comint-send-input)))))

;;; Describe operation

(cl-defgeneric drepl--call-eldoc (repl callback)
  "Compute help on thing at point and pass it to Eldoc's CALLBACK function."
  (when-let ((offset (- (point) (cdr comint-last-prompt)))
             (code (when (>= offset 0)
                     (buffer-substring-no-properties
                      (cdr comint-last-prompt)
                      (point-max))))
             (cb (lambda (data) (apply callback (drepl--format-eldoc repl data)))))
    (drepl--communicate repl cb 'describe :code code :offset offset)))

(cl-defgeneric drepl--format-eldoc (repl data)
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
  "Function intended to be a member of `eldoc-documentation-functions'."
  (when-let ((repl (drepl--get-repl 'ready)))
    (drepl--call-eldoc repl callback)))

;;; Initialization and restart

(defun drepl--project-directory (ask)
  "Return the current project root directory.
If it can be determined and ASK is non-nil, ask for a project;
otherwise fall back to `default-directory'."
  (if-let ((proj (project-current ask)))
      (project-root proj)
    default-directory))

(defun drepl--buffer-name (class directory)
  "Buffer name for a REPL of the given CLASS running in DIRECTORY."
  (format "%s/*%s*"
          (file-name-nondirectory
           (directory-file-name directory))
          (or (get class 'drepl--buffer-name) class)))

(defun drepl--get-buffer-create (class ask)
  "Get or create a dREPL buffer of the given CLASS.
The directory of the buffer is determined by
`drepl-directory'.  If ASK is non-nil, allow an interactive query
if needed."
  (if (eq (type-of drepl--current) class)
      (drepl--buffer drepl--current)
    (let ((default-directory (if (stringp drepl-directory)
                                 drepl-directory
                               (funcall drepl-directory ask))))
      (get-buffer-create (drepl--buffer-name class default-directory)))))

(cl-defgeneric drepl--command (repl)
  "The command to start the REPL interpreter as a list of strings."
  (ignore repl)
  (error "This needs an implementation"))

(cl-defgeneric drepl--init (repl)
  "Initialization code for REPL.
This function is called in the REPL buffer after a Comint has
been started in it.  It should call `drepl-mode' or a derived
mode and perform all other desired initialization procedures."
  (ignore repl)
  (error "This needs an implementation"))

(cl-defgeneric drepl--set-options (repl data)
  "Implementation of the `setoptions' operation.
This method is called when the REPL sends a `getoptions'
notification.  The REPL is in `ready' state when this happens.
The notification message is passed as DATA."
  (ignore repl data)
  (error "This needs an implementation"))

(cl-defgeneric drepl--restart (repl hard)
  "Generic method to restart a REPL.
HARD should be as described in `drepl-restart', but it not used
in the default implementation."
  (ignore hard)
  (with-current-buffer (drepl--buffer repl)
    (kill-process (drepl--process repl))
    (while (accept-process-output (drepl--process repl)))
    (drepl--run (type-of repl) nil)))

(defun drepl-restart (&optional hard)
  "Restart the current REPL.
Some REPLs by default perform a soft reset by deleting all user
variables without killing the interpreter.  In those cases, a
prefix argument or non-nil HARD argument can be used to force a
hard reset."
  (interactive "P")
  (drepl--restart (drepl--get-repl nil t) hard))

(defun drepl--run (class may-prompt)
  (let ((buffer (drepl--get-buffer-create class may-prompt)))
    (unless (comint-check-proc buffer)
      (cl-letf* (((default-value 'process-environment) process-environment)
                 ((default-value 'exec-path) exec-path)
                 (repl (make-instance class :buffer buffer))
                 (command (drepl--command repl)))
        (with-current-buffer buffer
          (drepl--message "starting %s" buffer)
          (apply #'make-comint-in-buffer
               (buffer-name buffer) buffer
               (car command) nil
               (cdr command))
          (drepl--init repl)
          (setq drepl--current repl))))
    (pop-to-buffer buffer display-comint-buffer-action)))

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
  (add-hook 'completion-at-point-functions 'drepl--complete nil t)
  (add-hook 'eldoc-documentation-functions #'drepl--eldoc-function nil t)
  (add-hook 'comint-output-filter-functions 'comint-osc-process-output nil t)
  (push '("5161" . drepl--osc-handler) ansi-osc-handlers)
  (setq-local comint-input-sender #'drepl--send-string)
  (setq-local indent-line-function #'comint-indent-input-line-default)
  (setq-local list-buffers-directory default-directory))

(provide 'drepl)

; LocalWords:  dREPL
;;; drepl.el ends here
