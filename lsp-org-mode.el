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

(require 'json)
(require 'lsp-org-mode-method)

(defun lsp-org-mode--jsonrpc (request)
  "Parse and execute jsonrpc REQUEST."
  (let* ((args (json-parse-string request :object-type 'plist))
         (id (plist-get args :id))
         (method (plist-get args :method))
         (params (plist-get args :params))
         fn res)
    (message "===")
    (message "method: %s, params: %s" method params)
    (setq fn (plist-get lsp-org-mode-method--plist method 'equal))
    (unless fn
      (error "No such method: %s" method))
    (setq res (funcall fn params))
    (let ((result (plist-get res :result))
          (error (plist-get res :error)))
      (message "result: %s" result)
      (when res
        (if error
            `(:jsonrpc "2.0" :id ,id :error ,error)
          `(:jsonrpc "2.0" :id ,id :result ,result))))))

(defun lsp-org-mode--cli ()
  "Entrypoint of lsp-org-mode binary."
  (message "lsp-org-mode--cli: start")
  (let (inpt)
    (while (setq inpt (ignore-errors (read-string "")))
      (cond
       ((string-empty-p inpt))
       (t
        (condition-case err
            (let* ((res (lsp-org-mode--jsonrpc inpt))
                   (res-str (json-encode res)))
              (when res
                (message "response: %s" res-str)
                (princ (format "Content-Length: %d\r\n\r\n%s" (length res-str) res-str))))
          (error (message "lsp-org-mode--request error: %s" err)))))))
  (message "lsp-org-mode--cli: end"))

(provide 'lsp-org-mode)
;;; lsp-org-mode.el ends here

