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
(require 'lsp-org-mode-subr)

(defun lsp-org-mode-method--initialize (params)
  "Method `initialize` with PARAMS."
  (setq lsp-org-mode-var--initialize-params params)

  (setq lsp-org-mode-var--semantic-tokens
        (lsp-org-mode-subr--plist-get params
          (:capabilities :textDocument :semanticTokens :tokenTypes)))

  `( :result
     ( :capabilities
       ( :textDocumentSync 2            ; Incremental
         :completionProvider :json-empty-object
         :semanticTokensProvider
         ( :legend
           ( :tokenTypes ,lsp-org-mode-var--semantic-tokens)
           :range :json-false
           :full t)))))

(defun lsp-org-mode-method--initialized (_params)
  "Method `initialized` with PARAMS."
  nil)

(defun lsp-org-mode-method--$/setTrace (params)
  "Method `$/setTrace` with PARAMS."
  (setf (plist-get lsp-org-mode-var--initialize-params :tarce) (plist-get params :value))
  nil)

(defun lsp-org-mode-method--shutdown (_params)
  "Method `shutdown` with PARAMS."
  `( :result nil))

(defun lsp-org-mode-method--exit (_params)
  "Method `exit` with PARAMS."
  (kill-emacs)
  nil)

(defun lsp-org-mode-method--textDocument/didOpen (params)
  "Method `textDocument/didOpen` with PARAMS."
  (let* ((text-document (plist-get params :textDocument))
         (uri (plist-get text-document :uri))
         (text (plist-get text-document :text))
         (buf (generate-new-buffer (format "*lsp-org-mode* - %s" uri))))
    (with-current-buffer buf
      (org-mode)
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

(defun lsp-org-mode-method--textDocument/semanticTokens/full (params)
  "Method `textDocument/semanticTokens/full` with PARAMS."
  (let* ((uri (lsp-org-mode-subr--plist-get params (:textDocument :uri)))
         (buf (plist-get lsp-org-mode-var--buffers-plist uri 'string=))
         (tokens (with-current-buffer buf
                   (lsp-org-mode-subr--ensure-fontified)
                   (lsp-org-mode-subr--encode-tokens
                    (lsp-org-mode-subr--buffer-tokens)))))
    `( :result
       ( :data
         ,(mapcan
           (lambda (elm)
             (list (nth 1 elm) (nth 2 elm) (nth 3 elm) 0 0))
           tokens)))))

(defun lsp-org-mode-method--textDocument/completion (_params)
  "Method `textDocument/completion` with PARAMS."
  `( :result
     ( :isIncomplete :json-false
       :items
       (( :label "test1"
          :kind 1)
        ( :label "test2"
          :kind 1)))))

(defun lsp-org-mode-method--completionItem/resolve (params)
  "Method `completionItem/resolve` with PARAMS."
  `( :result
     ,params))

(provide 'lsp-org-mode-method)
;;; lsp-org-mode-method.el ends here
