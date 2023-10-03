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

;;; Customization options
(require 'comint-mime)
(require 'drepl)
(require 'python)

(defgroup drepl-ipython nil
  "IPython shell implemented via dREPL"
  :group 'drepl
  :group 'python
  :link '(url-link "https://github.com/astoff/drepl"))

(defcustom drepl-ipython-buffer-name "*IPython*"
  "Name of IPython shell buffer."
  :type 'string)

(defvar drepl-ipython--start-file
  (expand-file-name "drepl-ipython.py"
                    (if load-file-name
                        (file-name-directory load-file-name)
                      default-directory))
  "File name of the startup script.")

(cl-defstruct (drepl-ipython (:include drepl)))

(cl-defmethod drepl--restart :around ((repl drepl-ipython))
  (if (eq (drepl-status repl) 'ready)
      (with-current-buffer (drepl-buffer repl)
        (save-excursion)
        (goto-char (process-mark (drepl-process repl)))
        (insert-before-markers "%reset -f")
        (drepl--eval repl "%reset -f"))
    (cl-call-next-method)
    (drepl-ipython)))

(define-derived-mode drepl-ipython-mode drepl-mode "IPython"
  "Major mode for the IPython shell.

\\<drepl-ipython-mode-map>"
  :syntax-table python-mode-syntax-table
  :interactive nil
  (setq-local comint-indirect-setup-function #'python-mode)
  (push '("5151" . comint-mime-osc-handler) ansi-osc-handlers))

;;;###autoload
(defun drepl-ipython ()
  "Run the IPython interpreter in an inferior process."
  (interactive)
  (let* ((buffer (get-buffer-create drepl-ipython-buffer-name)))
    (unless (comint-check-proc buffer)
      (make-comint-in-buffer
       (buffer-name buffer)
       buffer
       python-interpreter nil
       "-c"
       "import sys; exec(''.join(sys.stdin)); DRepl.instance().mainloop()")
      (with-current-buffer buffer
        (drepl-ipython-mode)
        (setq-local drepl--current (make-drepl-ipython :buffer buffer))
        (with-temp-buffer
          (insert-file-contents drepl-ipython--start-file)
          (process-send-string buffer (buffer-string))
          (process-send-eof buffer))))
    (pop-to-buffer buffer display-comint-buffer-action)))

(provide 'drepl-ipython)

;;; drepl-ipython.el ends here
