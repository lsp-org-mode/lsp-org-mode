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

(require 'cl-lib)
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
       ( :textDocumentSync 2            ; Incremental
         :completionProvider
         ( :resolveProvider t)))))

(defun lsp-org-mode-method--initialized (_params)
  "Method `initialized` with PARAMS."
  nil)

(defun lsp-org-mode-method--$/setTrace (params)
  "Method `$/setTrace` with PARAMS."
  (setq lsp-org-mode-var--trace (plist-get params :value))
  nil)

(defun lsp-org-mode-method--textDocument/didOpen (params)
  "Method `textDocument/didOpen` with PARAMS."
  (add-to-list 'lsp-org-mode-var--documents (plist-get params :textDocument))
  nil)

(defun lsp-org-mode-method--textDocument/didChange (params)
  "Method `textDocument/didChange` with PARAMS."
  (let ((_document (plist-get params :textDocument))
        (_content-changes (plist-get params :contentChanges)))
    nil)                                ; TODO
  nil)

(defun lsp-org-mode-method--textDocument/willSave (_params)
  "Method `textDocument/willSave` with PARAMS."
  nil)

(defun lsp-org-mode-method--textDocument/willSaveWaitUntil (_params)
  "Method `textDocument/willSaveWaitUntil` with PARAMS."
  nil)

(defun lsp-org-mode-method--textDocument/didSave (_params)
  "Method `textDocument/didSave` with PARAMS."
  nil)

(defun lsp-org-mode-method--textDocument/didClose (params)
  "Method `textDocument/didClose` with PARAMS."
  (let ((uri (plist-get params :uri)))
    (cl-delete-if
     (lambda (elm) (string= uri (plist-get elm :uri)))
     lsp-org-mode-var--documents))
  nil)

(defvar lsp-org-mode-method--plist
  (list
   "initialize" 'lsp-org-mode-method--initialize
   "initialized" 'lsp-org-mode-method--initialized
   "$/setTrace" 'lsp-org-mode-method--$/setTrace
   "textDocument/didOpen" 'lsp-org-mode-method--textDocument/didOpen
   "textDocument/didChange" 'lsp-org-mode-method--textDocument/didChange
   "textDocument/willSave" 'lsp-org-mode-method--textDocument/willSave
   "textDocument/willSaveWaitUntil" 'lsp-org-mode-method--textDocument/willSaveWaitUntil
   "textDocument/didSave" 'lsp-org-mode-method--textDocument/didSave
   "textDocument/didClose" 'lsp-org-mode-method--textDocument/didClose))

(provide 'lsp-org-mode-method)
;;; lsp-org-mode-method.el ends here
