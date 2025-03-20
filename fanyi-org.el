;;; fanyi-org.el --- Org mode integration for fanyi -*- lexical-binding: t; -*-

;; Copyright (C) 2025 DarkSun

;; Author: DarkSun <lujun9972@gmail.com>
;; Maintainer: DarkSun <lujun9972@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (fanyi "0.1.0") (org "9.0"))
;; URL: https://github.com/yourname/fanyi.el
;; Keywords: translation, org, tools

;;; Commentary:
;; This package provides Org mode integration for the fanyi translation toolkit.
;;
;; Features:
;; - Translate entire Org buffers preserving structure
;; - Support for Org elements:
;;   * Headlines
;;   * Comments
;;   * Footnotes
;;   * Plain text
;;
;; Usage:
;;   M-x fanyi-org-current-buffer  ; Translate current buffer
;;   M-x fanyi-org-file   ; Translate Org file
;;
;; Configuration:
;;   (with-eval-after-load 'org
;;     (require 'fanyi-org))
;;
;;; Code:

(require 'fanyi)
(require 'org-element)

(defun fanyi-org-node--plain-text (node)
  (org-element-set-element node (fanyi-content node)))

(defun fanyi-org-node--fanyi-property (node property)
  (let ((value (org-element-property property node)))
    (when (stringp  value)
      (org-element-put-property node property (fanyi-content value)))))

(defun fanyi-org-node--comment (node)
  (fanyi-org-node--fanyi-property node :value))

(defalias 'fanyi-org-node--comment-block 'fanyi-org-node--comment)

(defun fanyi-org-node--footnote-definition (node)
  (fanyi-org-node--fanyi-property node :label))

(defalias 'fanyi-org-node--footnote-reference 'fanyi-org-node--footnote-definition)

(defun fanyi-org-node--headline (node)
  (let* ((value (org-element-property :raw-value node))
         (raw-value (fanyi-content value))
         (title (org-element-parse-secondary-string raw-value (org-element-restriction 'headline))))
    (org-element-put-property node :raw-value raw-value)
    (org-element-put-property node :title title))
  (message "headline-node:(%s)" node)
  node)

(defalias 'fanyi-org-node--inlinetask 'fanyi-org-node--headline)

(defun fanyi-org-node (node)
  "Translate NODE in Org mode parse tree."
  (let* ((type (org-element-type node))
         (children (org-element-contents node))
         (fanyi-fn (cl-case type
                     (plain-text #'fanyi-org-node--plain-text)
                     (comment #'fanyi-org-node--comment)
                     (comment-block #'fanyi-org-node--comment)
                     (footnote-definition #'fanyi-org-node--footnote-definition)
                     (footnote-reference #'fanyi-org-node--footnote-reference)
                     (headline #'fanyi-org-node--headline)
                     (inlinetask #'fanyi-org-node--inlinetask))))
    (when fanyi-fn
      (funcall fanyi-fn node))
    (when children
      (org-element-set-contents children (mapc #'fanyi-org-node children))))
  node)

;;;###autoload
(defun fanyi-org-file (file &optional save-file)
  "Translate Org FILE and save to SAVE-FILE."
  (interactive "fFile: ")
  (let ((save-file (or save-file (concat file ".translated"))))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (fanyi-current-buffer)
      (write-region (point-min) (point-max) save-file))))

;;;###autoload
(defun fanyi-org-current-buffer ()
  "Translate current Org mode buffer."
  (interactive)
  (let* ((tree (org-element-parse-buffer))
         (tree (fanyi-org-node tree))
         (_ (setq _tree tree)))
    (erase-buffer)
    (insert (org-element-interpret-data tree))))

(provide 'fanyi-org)
;;; fanyi-org.el ends here
