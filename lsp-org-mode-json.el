;;; lsp-org-mode-json.el --- Lsp server for org-mode document  -*- lexical-binding: t; -*-

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

(defun lsp-org-mode-json-encode--atom (obj)
  "Encode atom OBJ as JSON."
  (cond
   ((eq obj t) "true")
   ((eq obj :json-false) "false")
   ((eq obj nil) "null")
   ((eq obj :json-empty-array) "[]")
   ((eq obj :json-empty-object) "{}")
   ((keywordp obj) (json-encode obj))
   ((numberp obj) (json-encode obj))
   ((stringp obj) (json-encode-string obj))
   (t (error "Cannot encode %s as JSON" obj))))

(defun lsp-org-mode-json-encode--list (obj)
  "Encode list OBJ as JSON."
  (if (keywordp (car obj))
      (let (pairs)
        (while obj
          (push (cons (json-encode (pop obj))
                      (lsp-org-mode-json-encode (pop obj)))
                pairs))
        (format "{%s}"
                (mapconcat
                 (lambda (elm) (format "%s:%s" (car elm) (cdr elm)))
                 (nreverse pairs)
                 ",")))
    (format "[%s]" (mapconcat 'lsp-org-mode-json-encode obj ","))))

(defun lsp-org-mode-json-encode (obj)
  "Encode OBJ to JSON string."
  (cond
   ((atom obj) (lsp-org-mode-json-encode--atom obj))
   ((listp obj) (lsp-org-mode-json-encode--list obj))))

(provide 'lsp-org-mode-json)
;;; lsp-org-mode-json.el ends here

