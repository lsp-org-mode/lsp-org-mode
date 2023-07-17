;;; lsp-org-mode.el --- Lsp server for org-mode document  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Keywords: convenience

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

;; This package provides lsp server for org-mode document.

;;; Code:

(defun lsp-org-mode--jsonrpc (request)
  "Parse and execute jsonrpc REQUEST."
  (let ((args (json-parse-string request :object-type 'plist)))
    (message "lsp-org-mode: %s" args)))

(defun lsp-org-mode--cli ()
  "Entrypoint of lsp-org-mode binary."
  (message "lsp-org-mode--cli")
  (let (inpt)
    (while (setq inpt (ignore-errors (read-string "")))
      (message inpt)
      (cond
       ((string-empty-p inpt))
       ((string-prefix-p "Content-Length" inpt))
       (t
        (condition-case err
            (lsp-org-mode--jsonrpc inpt)
          (error (message "lsp-org-mode--request error: %s" err)))))))
  (message "lsp-org-mode--cli end"))

(provide 'lsp-org-mode)
;;; lsp-org-mode.el ends here

