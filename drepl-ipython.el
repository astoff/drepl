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

(require 'comint-mime)
(require 'drepl)
(require 'python)

;;; Customization options

(defgroup drepl-ipython nil
  "IPython shell implemented via dREPL"
  :group 'drepl
  :group 'python
  :link '(url-link "https://github.com/astoff/drepl"))

(defcustom drepl-ipython-prompts
  ["In [{}]: " "...: " "\e[31mOut[{}]:\e[0m " "\n" ""]
  "Prompts of the Python dREPL.

This should be a vector of 5 string: primary prompt, continuation
prompt, output prefix, input separator, output separator.  The
substring \"{}\" is replaced by the execution count."
  :type '(vector (string :tag "Primary prompt")
                 (string :tag "Continuation prompt")
                 (string :tag "Output prompt")
                 (string :tag "Input separator")
                 (string :tag "Output separator")))

(defvar drepl-ipython--start-file
  (expand-file-name "drepl-ipython.py"
                    (if load-file-name
                        (file-name-directory load-file-name)
                      default-directory))
  "File name of the startup script.")

(define-derived-mode drepl-ipython-mode drepl-mode "IPython"
  "Major mode for the IPython shell.

\\<drepl-ipython-mode-map>"
  :syntax-table python-mode-syntax-table
  :interactive nil
  (setq-local comint-indirect-setup-function #'python-mode)
  (push '("5151" . comint-mime-osc-handler) ansi-osc-handlers))

(defclass drepl-ipython (drepl-base) nil)

;;;###autoload
(defun drepl-run-ipython ()
  (interactive)
  (drepl--run 'drepl-ipython t))

(cl-defmethod drepl--command ((_ drepl-ipython))
  `(,python-interpreter "-c"
    "import sys; exec(''.join(sys.stdin)); DRepl.instance().mainloop()"))

(cl-defmethod drepl--init ((_ drepl-ipython))
  (drepl-ipython-mode)
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (insert-file-contents drepl-ipython--start-file)
      (process-send-string buffer (buffer-string))
      (process-send-eof buffer))))

(cl-defmethod drepl--set-options ((repl drepl-ipython) _)
  (drepl--communicate repl #'ignore 'setoptions
                      :prompts drepl-ipython-prompts))

(cl-defmethod drepl--restart :around ((repl drepl-ipython) hard)
  (if (and (not hard) (eq (drepl--status repl) 'ready))
      (with-current-buffer (drepl--buffer repl)
        (save-excursion)
        (goto-char (process-mark (drepl--process repl)))
        (insert-before-markers "%reset -f")
        (drepl--eval repl "%reset -f"))
    (cl-call-next-method)))

(provide 'drepl-ipython)

;;; drepl-ipython.el ends here
