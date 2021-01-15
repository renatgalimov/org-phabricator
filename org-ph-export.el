;;; org-ph-export.el ---
;;
;; Filename: org-ph-export.el
;; Description:
;; Author: Renat Galimov
;; Maintainer:
;; Created: Пт янв 15 20:26:21 2021 (+0300)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Пт янв 15 20:32:58 2021 (+0300)
;;           By: Renat Galimov
;;     Update #: 11
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



(org-export-define-derived-backend 'remarkup 'md
  :filters-alist '((:filter-final-output . org-ph-remarkup-final-function))
  :menu-entry
  '(?r "Export to ReMarkup"
       ((?R "To temporary buffer"
	        (lambda (a s v b) (org-ph-remarkup-export-as-remarkup a s v)))))
  :translate-alist '(
                     ;;                     (clock . org-md-verbatim)
                     (src-block . org-ph-remarkup-src-block)
                     (example-block . org-ph-remarkup-src-block)
                     (item . org-ph-remarkup-item)
                     (timestamp . org-ph-remarkup-timestamp)
                     ))

(defun org-ph--src-block-header (src-block)
  "Build a remarkup soruce code header for SRC-BLOCK."
  (let* ((src-lang (org-element-property :language src-block))
         (name (org-element-property :name src-block))
         (src-lang-str (when src-lang (format "lang=%s" src-lang)))
         (name-str (when name (format "name=\"%s\"" name))))
    (mapconcat
     'identity
     `(,src-lang-str ,name-str)
     ",")))

(defun org-ph-remarkup-src-block (src-block _contents info)
  "Transcode SRC-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (replace-regexp-in-string
   "^" "\"
   (concat
    "```"
    (org-ph--src-block-header src-block)
    "\n"
    (org-remove-indentation (org-export-format-code-default src-block info))
    "```")))


;;;; Timestamp

(defun org-ph-remarkup-timestamp (timestamp _contents info)
  "Transcode a TIMESTAMP object from Org to Remarkup.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-html-plain-text (org-timestamp-translate timestamp) info)))
    (format "`%s`"
	    (replace-regexp-in-string "--" "–" value))))


(defun org-ph-remarkup-item (item contents info)
  "Transcode ITEM element into Markdown format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
	     (struct (org-element-property :structure item))
	     (bullet (if (not (eq type 'ordered)) "-"
		           (concat (number-to-string
			                (car (last (org-list-get-item-number
					                    (org-element-property :begin item)
					                    struct
					                    (org-list-prevs-alist struct)
					                    (org-list-parents-alist struct)))))
			               "."))))
    (concat bullet " "
	        (pcase (org-element-property :checkbox item)
	          (`on "[X] ")
	          (`trans "[-] ")
	          (`off "[ ] "))
	        (let ((tag (org-element-property :tag item)))
	          (and tag (format "**%s:** "(org-export-data tag info))))
	        (and contents
		         (org-trim (replace-regexp-in-string "^\\([^\^?]\\)" "    \\1" contents))))))

(defun org-ph-remarkup-final-function (contents _backend info)
  "Clean CONTENTS from control characters.  INFO is a plist used as a communication channel."
  (replace-regexp-in-string "^[ \t]+$\\|\^?" "" contents))



;;; Interactive function

;;;###autoload
(defun org-ph-remarkup-export-as-remarkup (&optional async subtreep visible-only)
  "Export current buffer to a Remarkup buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org Remarkup Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'remarkup "*Org Remarkup Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-ph-export.el ends here