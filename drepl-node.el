;;; drepl-node.el --- Node.js shell implemented via dREPL  -*- lexical-binding: t; -*-

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

;; This file defines a shell for Node.js.  It uses Node.js's built-in
;; REPL library.

;;; Code:

;;; Customization options
(require 'drepl)

(defgroup drepl-node nil
  "Node.js shell implemented via dREPL."
  :group 'drepl
  :group 'lua
  :link '(url-link "https://github.com/astoff/drepl"))

(defcustom drepl-node-program "node"
  "Name of the Node.js executable."
  :type 'string)

(defvar drepl-node--start-file
  (expand-file-name "drepl-node.js"
                    (if load-file-name
                        (file-name-directory load-file-name)
                      default-directory))
  "File name of the startup script.")

;;;###autoload (autoload 'drepl-node "drepl-node" nil t)
(drepl--define drepl-node :display-name "Node.js")

(cl-defmethod drepl--command ((_ drepl-node))
  `(,drepl-node-program ,drepl-node--start-file))

(cl-defmethod drepl--init ((repl drepl-node))
  (cl-call-next-method repl)
  (drepl--adapt-comint-to-mode ".js"))

(provide 'drepl-node)

;;; drepl-node.el ends here
