;;; lsp-org-mode-subr.el --- Lsp server for org-mode document  -*- lexical-binding: t; -*-

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

;; This file contains below code;
;; - htmlize [GPL2 or later]; Hrvoje Niksic <hniksic@gmail.com>

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'lsp-org-mode-var)

(defmacro lsp-org-mode-subr--plist-get (plist keys &optional pred)
  "Get value from PLIST by KEYS with PRED."
  (declare (indent 1))
  (let ((key (car keys))
        (rest (cdr keys)))
    (if rest
        `(lsp-org-mode-subr--plist-get (plist-get ,plist ,key ,pred) ,rest ,pred)
      `(plist-get ,plist ,key ,pred))))

(defun lsp-org-mode-subr--semantic-token-inx (key)
  "Get semantic token index from KEY."
  (cl-position key lsp-org-mode-var--semantic-tokens :test #'string=))

(defun lsp-org-mode-subr--ensure-fontified ()
  "Forked `htmlize-ensure-fontified'."
  ;; If font-lock is being used, ensure that the "support" modes
  ;; actually fontify the buffer.  If font-lock is not in use, we
  ;; don't care because, except in htmlize-file, we don't force
  ;; font-lock on the user.
  (if (fboundp 'font-lock-ensure)
      (font-lock-ensure)
    ;; Emacs prior to 25.1
    (with-no-warnings
      (font-lock-mode 1)
      (font-lock-fontify-buffer))))

(defun lsp-org-mode-subr--overlay-faces-at (pos)
  "Forked `htmlize-overlay-faces-at' POS."
  (delq nil (mapcar (lambda (o) (overlay-get o 'face)) (overlays-at pos))))

(defun lsp-org-mode-subr--next-face-change (pos &optional limit)
  "Forked `lsp-org-mode-subr--next-face-change' POS, LIMIT."
  ;; (htmlize-next-change pos 'face limit) would skip over entire
  ;; overlays that specify the `face' property, even when they
  ;; contain smaller text properties that also specify `face'.
  ;; Emacs display engine merges those faces, and so must we.
  (or limit
      (setq limit (point-max)))
  (let ((next-prop (next-single-property-change pos 'face nil limit))
        (overlay-faces (lsp-org-mode-subr--overlay-faces-at pos)))
    (while (progn
             (setq pos (next-overlay-change pos))
             (and (< pos next-prop)
                  (equal overlay-faces (lsp-org-mode-subr--overlay-faces-at pos)))))
    (setq pos (min pos next-prop))
    ;; Additionally, we include the entire region that specifies the
    ;; `display' property.
    (when (get-char-property pos 'display)
      (setq pos (next-single-char-property-change pos 'display nil limit)))
    pos))

(defun lsp-org-mode-subr--decode-face-prop (prop)
  "Forked `htmlize-decode-face-prop' PROP."
  ;; PROP can be a symbol naming a face, a string naming such a
  ;; symbol, a cons (foreground-color . COLOR) or (background-color
  ;; COLOR), a property list (:attr1 val1 :attr2 val2 ...), or a list
  ;; of any of those.
  ;;
  ;; (htmlize-decode-face-prop 'face) -> (face)
  ;; (htmlize-decode-face-prop '(face1 face2)) -> (face1 face2)
  ;; (htmlize-decode-face-prop '(:attr "val")) -> ((:attr "val"))
  ;; (htmlize-decode-face-prop '((:attr "val") face (foreground-color "red")))
  ;;   -> ((:attr "val") face (foreground-color "red"))
  ;;
  ;; Unrecognized atoms or non-face symbols/strings are silently
  ;; stripped away.
  (cond ((null prop)
         nil)
        ((symbolp prop)
         (and (facep prop)
              (list prop)))
        ((stringp prop)
         (and (facep (intern-soft prop))
              (list prop)))
        ((atom prop)
         nil)
        ((and (symbolp (car prop))
              (eq ?: (aref (symbol-name (car prop)) 0)))
         (list prop))
        ((or (eq (car prop) 'foreground-color)
             (eq (car prop) 'background-color))
         (list prop))
        (t
         (apply #'nconc (mapcar #'lsp-org-mode-subr--decode-face-prop prop)))))

(defun lsp-org-mode-subr--faces-at-point ()
  "Forked `htmlize-faces-at-point'."
  (let (all-faces)
    ;; Faces from text properties.
    (let ((face-prop (get-text-property (point) 'face)))
      ;; we need to reverse the `face' prop because we want
      ;; more specific faces to come later
      (setq all-faces (nreverse (lsp-org-mode-subr--decode-face-prop face-prop))))
    ;; Faces from overlays.
    (let ((overlays
           ;; Collect overlays at point that specify `face'.
           (cl-delete-if-not (lambda (o)
                               (overlay-get o 'face))
                             (nreverse (overlays-at (point) t))))
          list face-prop)
      (dolist (overlay overlays)
        (setq face-prop (overlay-get overlay 'face)
              list (nconc (lsp-org-mode-subr--decode-face-prop face-prop) list)))
      ;; Under "Merging Faces" the manual explicitly states
      ;; that faces specified by overlays take precedence over
      ;; faces specified by text properties.
      (setq all-faces (nconc all-faces list)))
    all-faces))

(defun lsp-org-mode-subr--buffer-tokens (&optional buf)
  "Get tokens from BUF."
  (let (res next-change face-list)
    (with-current-buffer (or buf (current-buffer))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (setq next-change (lsp-org-mode-subr--next-face-change (point)))
          (setq face-list (lsp-org-mode-subr--faces-at-point))
          (when face-list
            (push (list
                   face-list
                   (line-number-at-pos)
                   (current-column)
                   (- next-change (point)))
                  res))
          (goto-char next-change))))
    (nreverse res)))

(defun lsp-org-mode-subr--encode-tokens (tokens)
  "Encode TOKENS."
  (let (res prev)
    (setq prev '(nil 1 0 0))
    (dolist (cur tokens)
      (push (list
             (nth 0 cur)
             (- (nth 1 cur) (nth 1 prev))
             (if (= 0 (- (nth 1 cur) (nth 1 prev)))
                 (- (nth 2 cur) (nth 2 prev))
               (nth 2 cur))
             (nth 3 cur))
            res)
      (setq prev cur))
    (nreverse res)))

(defun lsp-org-mode-subr--buffer-tree-range (&optional buf)
  "Get tree range from BUF."
  (let (res)
    (with-current-buffer (or buf (current-buffer))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (org-next-visible-heading 1)
          (push (list
                 (org-current-level)
                 (line-number-at-pos)
                 (save-excursion         ; see `org-cycle-internal-local'
		           (org-end-of-subtree t t)
		           (unless (eobp) (forward-char -1))
		           (line-number-at-pos)))
                res))))
    (pop res)                            ; remove last garbage element
    (nreverse res)))

(defun lsp-org-mode-subr--move-line-col (line col)
  "Move to LINE and COL."
  (goto-char (point-min))
  (forward-line line)
  (forward-char col))

(defun lsp-org-mode-subr--point-to-line-col (pnt)
  "Get line and col from PNT."
  (save-excursion
    (goto-char pnt)
    (list (line-number-at-pos)
          (current-column))))

(defun lsp-org-mode-subr--encode-range (start end)
  "Encode START and END."
  (let ((e-start (lsp-org-mode-subr--point-to-line-col start))
        (e-end (lsp-org-mode-subr--point-to-line-col end)))
    `( :start (:line ,(1- (nth 0 e-start)) :character ,(nth 1 e-start))
       :end (:line ,(1- (nth 0 e-end)) :character ,(nth 1 e-end)))))

(provide 'lsp-org-mode-subr)
;;; lsp-org-mode-subr.el ends here
