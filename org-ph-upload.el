;;; org-ph-upload.el --- -*- lexical-binding: t -*-
;;
;; Filename: org-ph-upload.el
;; Description:
;; Author: Renat Galimov
;; Maintainer:
;; Created: Сб мая 29 08:38:05 2021 (+0300)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 160
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'org-element)

(defun org-ph--set-file-id (element id)
  "Set phabricator file ID to ELEMENT." (let* (
         (attr-remarkup (org-export-read-attribute :attr_remarkup element))
         (element-copy (org-element-copy element))
         (element-begin (org-element-property :begin element))
         (element-text (buffer-substring element-begin (org-element-property :end element)))
         (indent-level (progn (string-match "^ +" element-text) (match-end 0))))
    (setq attr-remarkup (plist-put attr-remarkup :id id))
    (setq element-copy (org-element-put-property element-copy :attr_remarkup `(,(substring (format "%s" attr-remarkup) 1 -1))))

    (delete-region element-begin (org-element-property :post-affiliated element))
    (save-excursion
      (goto-char element-begin)
      (insert
       (replace-regexp-in-string "^" (make-string indent-level ? ) (string-trim (org-element-interpret-data element-copy))))
      (insert "\n"))))


;;;###autoload
(defun org-ph-upload-file (element)
  "Upload ELEMENT at point as a phabricator file."

  (interactive (list (org-element-context)))
  (let ((element-type (org-element-type element)))
    (unless (eq element-type 'link) (error "Wrong element type %s" element-type)))
  (let* ((file-parent (org-element-property :parent element))
         (attr-remarkup (org-export-read-attribute :attr_remarkup file-parent))
         (file-id (plist-get attr-remarkup :id)))

    (if (> (length file-id) 0)
        (message "Element already has file id: %s" file-id)
      (let* ((raw-link (org-element-property :raw-link (org-element-context)))
             (path (org-element-property :path element))
             (link (if (string-prefix-p "attachment:" raw-link)
                       (org-attach-expand path)
                     path)))
        (org-ph--set-file-id file-parent (org-ph-fetch-upload-file link))))))


(provide 'org-ph-upload)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-ph-upload.el ends here
