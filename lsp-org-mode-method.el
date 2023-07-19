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
  (let* ((text-document (plist-get params :textDocument))
         (uri (plist-get text-document :uri))
         (text (plist-get text-document :text))
         (buf (generate-new-buffer (format "*lsp-org-mode* - %s" uri))))
    (with-current-buffer buf
      (insert text))
    (setf (plist-get lsp-org-mode-var--buffers-plist uri 'string=) buf))
  nil)

(defun lsp-org-mode-method--textDocument/didChange (params)
  "Method `textDocument/didChange` with PARAMS."
  (let* ((text-document (plist-get params :textDocument))
         (uri (plist-get text-document :uri))
         (content-changes (plist-get params :contentChanges))
         (buf (plist-get lsp-org-mode-var--buffers-plist uri 'string=)))
    (unless buf
      (error "Buffer not found for uri: %s" uri))
    (dolist (change content-changes)
      (let* ((range (plist-get change :range))
             (range-start (plist-get range :start))
             (range-start-line (plist-get range-start :line))
             (range-start-character (plist-get range-start :character))
             (range-length (plist-get change :rangeLength))
             (text (plist-get change :text)))
        (with-current-buffer buf
          (save-excursion
            (goto-char (point-min))
            (forward-line range-start-line)
            (forward-char range-start-character)
            (delete-char range-length)
            (insert text)))))
    (message
     (with-current-buffer buf
       (buffer-substring-no-properties (point-min) (point-max)))))
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
    (setf (plist-get lsp-org-mode-var--buffers-plist uri 'string=) nil))
  nil)

(defun lsp-org-mode-method--textDocument/completion (_params)
  "Method `textDocument/completion` with PARAMS."
  `( :result
     ( :isIncomplete :json-false
       :items
       (( :label "test1"
          :kind 1)
        ( :label "test2"
          :kind 1)))))

(defvar lsp-org-mode-method--plist
  (list
   ;; Lifecycle Messages
   "initialize" 'lsp-org-mode-method--initialize
   "initialized" 'lsp-org-mode-method--initialized
   "$/setTrace" 'lsp-org-mode-method--$/setTrace

   ;; Document Synchronization
   "textDocument/didOpen" 'lsp-org-mode-method--textDocument/didOpen
   "textDocument/didChange" 'lsp-org-mode-method--textDocument/didChange
   "textDocument/willSave" 'lsp-org-mode-method--textDocument/willSave
   "textDocument/willSaveWaitUntil" 'lsp-org-mode-method--textDocument/willSaveWaitUntil
   "textDocument/didSave" 'lsp-org-mode-method--textDocument/didSave
   "textDocument/didClose" 'lsp-org-mode-method--textDocument/didClose

   ;; Language Features
   "textDocument/completion" 'lsp-org-mode-method--textDocument/completion))

(provide 'lsp-org-mode-method)
;;; lsp-org-mode-method.el ends here
