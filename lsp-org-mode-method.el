;;; lsp-org-mode-method.el --- Lsp server for org-mode document  -*- lexical-binding: t; -*-

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

(require 'lsp-org-mode-var)

(defun lsp-org-mode-method--initialize (params)
  "Method `initialize` with PARAMS."
  (message "lsp-org-mode-method--initialize")
  (setq lsp-org-mode-var--process-id (plist-get params :processId))
  (setq lsp-org-mode-var--client-info (plist-get params :clientInfo))
  (setq lsp-org-mode-var--locale (plist-get params :locale))
  (setq lsp-org-mode-var--root-path (plist-get params :rootPath))
  (setq lsp-org-mode-var--root-uri (plist-get params :rootUri))
  (setq lsp-org-mode-var--initialization-options (plist-get params :initializationOptions))
  (setq lsp-org-mode-var--capabilities (plist-get params :capabilities))
  (setq lsp-org-mode-var--trace (plist-get params :trace))
  (setq lsp-org-mode-var--workspace-folders (plist-get params :workspaceFolders))
  `( :result
     ( :capabilities
       ( :textDocumentSync 2  ; Incremental
         :completionProvider
         ( :resolveProvider t)))))

(defvar lsp-org-mode-method--plist
  (list
   "initialize" 'lsp-org-mode-method--initialize))

(provide 'lsp-org-mode-method)
;;; lsp-org-mode-method.el ends here
