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

;; 

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
  (drepl--eval (drepl--get-repl) code))

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

(cl-defgeneric drepl--describe (repl callback)
  (when-let ((offset (- (point) (cdr comint-last-prompt)))
             (code (when (>= offset 0)
                     (buffer-substring-no-properties
                      (cdr comint-last-prompt)
                      (point-max)))))
    (drepl--communicate repl callback 'describe :code code :offset offset)))

(defun drepl--make-help-buffer (data &optional interactive)
  (let-alist data
    (help-setup-xref (list #'drepl--make-help-buffer data) interactive)
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (when (stringp .name)
          (insert .name)
          (when (stringp .type) (insert " is a " .type))
          (when (stringp .file) (insert " defined in " (buttonize .file #'find-file .file)))
          (insert ".\n\n"))
        (when (stringp .text)
          (insert (ansi-color-apply .text)))))))

(defun drepl-describe-thing-at-point ()
  "Pop up help on the thing at point."
  (interactive)
  (when-let ((repl (when (derived-mode-p 'drepl-mode)
                     (drepl--get-repl 'ready))))
    (drepl--describe repl #'drepl--make-help-buffer)))

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
  (drepl--restart (or (drepl--get-repl)
                      (user-error "No REPL"))
                  hard))

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
  "<remap> <display-local-help>" #'drepl-describe-thing-at-point
  "C-c C-n" #'drepl-restart)

(define-derived-mode drepl-mode comint-mode "dREPL"
  "Major mode for the dREPL buffers."
  :interactive nil
  (add-hook 'comint-output-filter-functions 'comint-osc-process-output)
  (push '("5161" . drepl--osc-handler) ansi-osc-handlers)
  (setq-local comint-input-sender #'drepl--send-string)
  (setq-local indent-line-function #'comint-indent-input-line-default)
  (setq-local list-buffers-directory default-directory)
  (add-hook 'completion-at-point-functions 'drepl--complete nil t))

(provide 'drepl)

; LocalWords:  dREPL
;;; drepl.el ends here
