;;; drepl-usql.el --- SQL shell based on the usql program  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

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

;; This file defines a shell for various SQL and NoSQL databases.  It
;; is based on the usql program and requires a Go compiler.

;;; Code:

;;; Customization options
(require 'drepl)
(require 'comint-mime)

(defgroup drepl-usql nil
  "SQL shell implemented via dREPL."
  :group 'drepl
  :group 'SQL
  :link '(url-link "https://github.com/astoff/drepl"))

(defvar drepl-usql--directory
  (expand-file-name "drepl-usql/" (file-name-directory
                                   (or load-file-name default-directory)))
  "Directory containing the `drepl-usql' source code.")

(defvar drepl-usql--connection-history nil
  "History list of database connections.")

(defcustom drepl-usql-program
  (expand-file-name "drepl-usql" drepl-usql--directory)
  "Name of the drepl-usql executable."
  :type 'string)

(defun drepl-usql-build ()
  "Build the `drepl-usql' executable."
  (interactive)
  (let ((default-directory drepl-usql--directory)
        (gocmd (or (bound-and-true-p go-command) "go")))
    (compile (format "%s build -v drepl-usql.go"
                     (shell-quote-argument gocmd)))))

;;;###autoload (autoload 'drepl-usql "drepl-usql" nil t)
(drepl--define drepl-usql :display-name "usql")

(cl-defmethod drepl--command ((_ drepl-usql))
  (if-let ((prog (executable-find drepl-usql-program)))
      (list prog (read-from-minibuffer "Connect to database: "
                                       nil nil nil
                                       'drepl-usql--connection-history))
    (lwarn 'drepl-usql :error
           "`%s' not found, built it with %s"
           drepl-usql-program
           (propertize "M-x drepl-usql-build" 'face 'help-key-binding))
    (user-error "%s not found" drepl-usql-program)))

(cl-defmethod drepl--init ((repl drepl-usql))
  (cl-call-next-method repl)
  (setf (drepl--status repl) 'rawio)
  (push '("5151" . comint-mime-osc-handler) ansi-osc-handlers)
  (drepl--adapt-comint-to-mode ".sql"))

(provide 'drepl-usql)

;;; drepl-usql.el ends here
