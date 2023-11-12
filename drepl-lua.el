;;; drepl.el --- REPL protocol for the dumb terminal   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

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

;; This file defines a REPL for Lua.  The required Lua packages can be
;; installed with `luarocks install dkjson luarepl'.

;;; Code:

;;; Customization options
(require 'drepl)

(defgroup drepl-lua nil
  "Lua shell implemented via dREPL."
  :group 'drepl
  :group 'lua
  :link '(url-link "https://github.com/astoff/drepl"))

(defcustom drepl-lua-program "lua"
  "Name of the Lua executable."
  :type 'string)

(defvar drepl-lua--start-file
  (expand-file-name "drepl-lua.lua"
                    (if load-file-name
                        (file-name-directory load-file-name)
                      default-directory))
  "File name of the startup script.")

;;;###autoload (autoload 'drepl-lua "drepl-lua" nil t)
(drepl--define drepl-lua :display-name "Lua")

(cl-defmethod drepl--command ((_ drepl-lua))
  `(,drepl-lua-program "-v" "-e" "loadfile()():main()"))

(cl-defmethod drepl--init ((_ drepl-lua))
  (drepl-mode)
  (drepl--adapt-comint-to-mode ".lua")
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (insert-file-contents drepl-lua--start-file)
      (process-send-string buffer (buffer-string))
      (process-send-eof buffer))))

(provide 'drepl-lua)

;;; drepl-lua.el ends here
