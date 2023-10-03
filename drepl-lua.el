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
(require 'drepl)
(require 'lua-mode)

(defgroup drepl-lua nil
  "Lua shell implemented via dREPL"
  :group 'drepl
  :group 'lua
  :link '(url-link "https://github.com/astoff/drepl"))

(defcustom drepl-lua-buffer-name "*Lua*"
  "Name of Lua shell buffer."
  :type 'string)

(defvar drepl-lua--start-file
  (expand-file-name "drepl-lua.lua"
                    (if load-file-name
                        (file-name-directory load-file-name)
                      default-directory))
  "File name of the startup script.")

(cl-defstruct (drepl-lua (:include drepl)))

(cl-defmethod drepl--restart :after ((_repl drepl-lua))
  (drepl-lua))

(define-derived-mode drepl-lua-mode drepl-mode "Lua"
  "Major mode for the Lua shell.

\\<drepl-lua-mode-map>"
  :syntax-table lua-mode-syntax-table
  :interactive nil
  (setq-local comint-indirect-setup-function #'lua-mode))

;;; User commands

;;;###autoload
(defun drepl-lua ()
  "Run the Lua interpreter in an inferior process."
  (interactive)
  (cl-letf* ((buffer (get-buffer-create drepl-lua-buffer-name))
             ((default-value 'process-environment) process-environment)
             ((default-value 'exec-path) exec-path))
    (unless (comint-check-proc buffer)
      (make-comint-in-buffer
       (buffer-name buffer)
       buffer
       "lua" nil "-v" "-e" "loadfile()():main()")
      (with-current-buffer buffer
        (drepl-lua-mode)
        (setq-local process-environment process-environment) ;FIXME
        (setq-local exec-path exec-path)
        (setq-local drepl--current (make-drepl-lua :buffer buffer))
        (with-temp-buffer
          (insert-file-contents drepl-lua--start-file)
          (process-send-string buffer (buffer-string))
          (process-send-eof buffer))))
    (pop-to-buffer buffer display-comint-buffer-action)))

(provide 'drepl-lua)

;;; drepl-lua.el ends here
