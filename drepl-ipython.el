;;; drepl-ipython.el --- Python shell based on IPython  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Free Software Foundation, Inc.

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: languages, processes
;; URL: https://github.com/astoff/drepl

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

;; This file defines a Python shell based on the IPython package.  The
;; dependencies can be installed with `pip install ipython'.

;;; Code:

(require 'comint-mime)
(require 'drepl)
(require 'python)                       ;For `python-interpreter' only

;;; Customization options

(defgroup drepl-ipython nil
  "IPython shell implemented via dREPL."
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

;;;###autoload (autoload 'drepl-ipython "drepl-ipython" nil t)
(drepl--define drepl-ipython :display-name "IPython")

(cl-defmethod drepl--command ((_ drepl-ipython))
  `(,python-interpreter "-c" "\
from sys import stdin; \
exec(stdin.read(int(stdin.readline())))"))

(cl-defmethod drepl--init ((repl drepl-ipython))
  (cl-call-next-method repl)
  (drepl--adapt-comint-to-mode ".py")
  (push '("5151" . comint-mime-osc-handler) ansi-osc-handlers)
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (insert-file-contents drepl-ipython--start-file)
      (process-send-string buffer (format "%s\n" (buffer-size)))
      (process-send-string buffer (buffer-string)))))

(cl-defmethod drepl--set-options ((repl drepl-ipython) _)
  (drepl--communicate repl #'ignore 'setoptions
                      :prompts drepl-ipython-prompts))

(provide 'drepl-ipython)

;;; drepl-ipython.el ends here
