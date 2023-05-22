;;; org-ph-export.el --- -*- lexical-binding: t -*-
;;
;; Filename: org-ph-export.el
;; Description:
;; Author: Renat Galimov
;; Maintainer:
;; Created: Пт янв 15 20:26:21 2021 (+0300)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Fri May 12 11:43:42 2023 (+0300)
;;           By: Renat Galimov
;;     Update #: 384
;; URL: https://github.com/renatgalimov/org-phabricator
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

(require 'org)
(require 'ox)
(require 'ox-md)
(require 'seq)


(defgroup org-phabricator-export nil
  "Configure exporting org nodes to Remarkup."
  :tag "Org-Phabricator Export"
  :group 'org-phabricator)


(defcustom org-ph-export-tag-render-style nil
  "Define how to render tags."
  :tag "Tag render style"
  :type '(choice
          (const :tag "Wrap the tag with `tag`." nil)
          (const :tag "Wrap the tag with !!{icon tag}tag!!." highlight))
  :group 'org-phabricator-export)

(defcustom org-ph-export-todo-render-style nil
  "Define how to render TODO-like statuses."
  :tag "ToDo render style"
  :type '(choice
          (const :tag "Leave them as a text." nil)
          (const :tag "Wrap TODOs with !!TODO!!." highlight)
          (const :tag "Replace TODO with an appropriate icon from `org-ph-export-todo-icon-alist'" icon))
  :group 'org-phabricator-export)

(defcustom org-ph-export-todo-icon-alist '(("TODO" . "clipboard") ("DONE" . "check"))
  "Replace item statuses with {icon <icon-name>} on export."
  :tag "ToDo icon alist"
  :type '(alist :key-type (string :tag "ToDo") :value-type (string :tag "Icon name")
  :group 'org-phabricator-export))

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
                     (fixed-width . org-ph-remarkup-src-block)
                     (item . org-ph-remarkup-item)
                     (inner-template . org-ph-inner-template)
                     (timestamp . org-ph-remarkup-timestamp)
                     (headline . org-ph-headline)
                     (strike-through . org-ph-strike-through)
                     (italic . org-ph-italic)
                     (link . org-ph-link)
                     (table . org-ph--remarkup-table)
                     (table-row . org-ph--remarkup-table-row)
                     (table-cell . org-ph--remarkup-table-cell)
                     ))

(defun org-ph--remap-languages (language)
  "Remap org-src LANGUAGE to Remarkup language."
  (if (string= language "sqlite")
      "sql"
    language))

(defun org-ph--src-block-header (src-block)
  "Build a remarkup soruce code header for SRC-BLOCK."
  (let* ((src-lang (org-element-property :language src-block))
         (caption (or (car (car (car (org-element-property :caption src-block)))) ""))
         (name (or (org-element-property :name src-block) ""))
         (src-lang-str (when src-lang (format "lang=%s" (org-ph--remap-languages src-lang))))
         (name-str (cond ((and (not (string-blank-p caption)) (not (string-blank-p name))) (format "%s (%s)" caption name))
                         ((not (string-blank-p caption)) caption)
                         ((not (string-blank-p name)) name)
                         (t nil)))
         (name-part (when name-str (format "name=\"%s\"" name-str))))
    (mapconcat
     'identity
     (seq-filter 'identity `(,src-lang-str ,name-part "lines=12"))
     ",")))

(defun org-ph--remarkup-table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (if (eq (org-element-property :type table) 'table.el)
      ;; "table.el" table.  Convert it using appropriate tools.
      (org-html-table--table.el-table table info)
    ;; Standard table.
    (format "<table>\n%s\n</table>"
	          contents)))


(defun org-ph--remarkup-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to HTML.
CONTENTS is the contents of the row.  INFO is a plist used as a
communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
	(concat "\n"
		    "<tr>"
		    contents
		    "\n"
		    "</tr>")))



(defun org-ph--is-first-group (table-row info)
  (= 1 (org-export-table-row-group table-row info)))


(defun org-ph--remarkup-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((table-row (org-export-get-parent table-cell))
	     (table (org-export-get-parent-table table-cell))
	     (cell-attrs
	      (if (not (plist-get info :html-table-align-individual-fields)) ""
	        (format (if (and (boundp 'org-html-format-table-no-css)
			                 org-html-format-table-no-css)
			            " align=\"%s\"" " class=\"org-%s\"")
		            (org-export-table-cell-alignment table-cell info)))))
    (when (or (not contents) (string= "" (org-trim contents)))
      (setq contents ""))
    (cond
     ((and (org-export-table-has-header-p table info)
	       (= 1 (org-export-table-row-group table-row info)))
	  (concat "\n<th>" contents "</th>"))
     ((and (plist-get info :html-table-use-header-tags-for-first-column)
	       (zerop (cdr (org-export-table-cell-address table-cell info))))
      (concat "\n<th>" contents"</th>"))
     (t (let ((data-tags (plist-get info :html-table-data-tags)))
          (concat "\n<td>" contents"</td>"))))))


(defun org-ph-italic (_italic contents _info)
  "Transcode ITALIC object into Remarkup format.
CONTENTS is the text within italic markup.  INFO is a plist used
as a communication channel."
  (format "//%s//" contents))


(defun org-ph-strike-through (_strike-through contents _info)
  "Transcode strike-throught object into Remarkup format.
CONTENTS is the text within strike-through markup.  INFO is a plist used as
a communication channel."
  (format "~~%s~~" contents))


(defun org-ph-remarkup-src-block (src-block _contents info)
  "Transcode SRC-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel.

This also prepends every line of the code block with the `\^?'
escape sequence which will be stripped out in final filter."
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

  (replace-regexp-in-string "^[ \t]+$\\| *\^?" "" contents))



(defun org-ph--build-toc (info &optional n _keyword scope)
  "Return a table of contents.

INFO is a plist used as a communication channel.

Optional argument N, when non-nil, is an integer specifying the
depth of the table.

When optional argument SCOPE is non-nil, build a table of
contents according to the specified element."
  (concat
   (unless scope
     (let ((style (plist-get info :md-headline-style))
	       (title (org-html--translate "Table of Contents" info)))
       (org-md--headline-title style 1 title nil)))
   (mapconcat
    (lambda (headline)
      (let* ((indentation
	          (make-string
	           (* 4 (1- (org-export-get-relative-level headline info)))
	           ?\s))
	         (bullet
	          (if (not (org-export-numbered-headline-p headline info)) "-   "
		        (let ((prefix
		               (format "%d." (org-last (org-export-get-headline-number
						                        headline info)))))
		          (concat prefix (make-string (max 1 (- 4 (length prefix)))
					                          ?\s)))))
	         (title
		      (org-export-data-with-backend
		       (org-export-get-alt-title headline info)
		       (org-export-toc-entry-backend 'md)
		       info))
	         (tags (concat "  " (and (plist-get info :with-tags)
			            (not (eq 'not-in-toc (plist-get info :with-tags)))
			            (org-make-tag-string
			             (org-export-get-tags headline info))))))
	    (concat indentation bullet title tags)))
    (org-export-collect-headlines info n scope) "\n")
   "\n"))

(defun org-ph-inner-template (contents info)
  "Return body of document after converting it to Remarkup syntax.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  ;; Make sure CONTENTS is separated from table of contents and
  ;; footnotes with at least a blank line.
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth
       (concat (org-ph--build-toc info (and (wholenump depth) depth)) "\n")))
   ;; Document contents.
   contents
   "\n"
   ;; Footnotes section.
   (org-md--footnote-section info)))


;;;; Headline

(defun org-ph--highlight-tag (tag)
  "Make!!{icon tag}TAG!"
  (when tag
    (cond ((eq org-ph-export-tag-render-style 'highlight)
           (format "!!{icon tag}%s!!" tag))
          (t (format "`%s`" tag)))))

(defun org-ph--todo-icon (todo)
  "Get an {icon} for given TODO."
  (let ((icon (alist-get todo org-ph-export-todo-icon-alist nil nil 'equal)))
    (when icon
        (format "{icon %s}" icon))))


(defun org-ph--todo (todo)
  "Format TODO according to `org-ph-export-todo-render-style'."
  (cond ((eq org-ph-export-todo-render-style 'highlight)
         (format "!!%s!!" todo))
        ((eq org-ph-export-todo-render-style 'icon)
         (or (org-ph--todo-icon todo) todo))
        (t todo)))

(defun org-ph-headline (headline contents info)
  "Transcode HEADLINE element into Markdown format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
	       (title (org-export-data (org-element-property :title headline) info))
	       (todo (and (plist-get info :with-todo-keywords)
		              (let ((todo (org-element-property :todo-keyword
							                            headline)))
			            (and todo (concat (org-ph--todo (org-export-data todo info)) " ")))))
	       (tags (and (plist-get info :with-tags)
                      (concat "    " (mapconcat 'org-ph--highlight-tag (org-export-get-tags headline info) " "))))
	       (priority
	        (and (plist-get info :with-priority)
		         (let ((char (org-element-property :priority headline)))
		           (and char (format "[#%c] " char)))))
	       ;; Headline text without tags.
	       (heading (concat todo priority title))
	       (style (plist-get info :md-headline-style)))
      (cond
       ;; Cannot create a headline.  Fall-back to a list.
       ((or (org-export-low-level-p headline info)
	        (not (memq style '(atx setext)))
	        (and (eq style 'atx) (> level 6))
	        (and (eq style 'setext) (> level 2)))
	    (let ((bullet
	           (if (not (org-export-numbered-headline-p headline info)) "-"
		         (concat (number-to-string
			              (car (last (org-export-get-headline-number
				                      headline info))))
			             "."))))
	      (concat bullet (make-string (- 4 (length bullet)) ?\s) heading tags "\n\n"
		          (and contents (replace-regexp-in-string "^" "    " contents)))))
       (t
	    (concat (org-md--headline-title style level heading nil tags) contents))))))


(defun org-ph-link (link desc info)
  "Transcode LINK object into Markdown format.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((link-org-files-as-md
	      (lambda (raw-path)
	        ;; Treat links to `file.org' as links to `file.md'.
	        (if (string= ".org" (downcase (file-name-extension raw-path ".")))
		        (concat (file-name-sans-extension raw-path) ".md")
	          raw-path)))
	     (type (org-element-property :type link))
	     (raw-path (org-element-property :path link))
         (attr-remarkup (org-export-read-attribute :attr_remarkup (org-element-property :parent link)))
         (ph-id (plist-get attr-remarkup :id))
	     (path (cond
		        ((member type '("http" "https" "ftp" "mailto"))
		         (concat type ":" raw-path))
		        ((string-equal  type "file")
		         (org-export-file-uri (funcall link-org-files-as-md raw-path)))
		        (t raw-path))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'md info))
     ((> (length ph-id) 0) (format "{%s}" ph-id))
     ((member type '("custom-id" "id" "fuzzy"))
      (let ((destination (if (string= type "fuzzy")
			                 (org-export-resolve-fuzzy-link link info)
			               (org-export-resolve-id-link link info))))
	    (pcase (org-element-type destination)
	      (`plain-text			; External file.
	       (let ((path (funcall link-org-files-as-md destination)))
	         (if (not desc) (format "<%s>" path)
	           (format "[%s](%s)" desc path))))
	      (`headline
	       (format
	        "[%s](#%s)"
	        ;; Description.
	        (cond ((org-string-nw-p desc))
		          ((org-export-numbered-headline-p destination info)
		           (mapconcat #'number-to-string
			                  (org-export-get-headline-number destination info)
			                  "."))
		          (t (org-export-data (org-element-property :title destination)
				                      info)))
	        ;; Reference.
	        (or (org-element-property :CUSTOM_ID destination)
		        (org-export-get-reference destination info))))
	      (_
	       (let ((description
		          (or (org-string-nw-p desc)
		              (let ((number (org-export-get-ordinal destination info)))
			            (cond
			             ((not number) nil)
			             ((atom number) (number-to-string number))
			             (t (mapconcat #'number-to-string number ".")))))))
	         (when description
	           (format "[%s](#%s)"
		               description
		               (org-export-get-reference destination info))))))))
     ;; Link is an image.
     ((org-export-inline-image-p link org-html-inline-image-rules)
      (let ((path (cond ((not (string-equal type "file"))
			             (concat type ":" raw-path))
			            ((not (file-name-absolute-p raw-path)) raw-path)
			            (t (expand-file-name raw-path))))
	        (caption (org-export-data
		              (org-export-get-caption
		               (org-export-get-parent-element link))
		              info)))
        (format "![img](%s)"
		        (if (not (org-string-nw-p caption)) path
		          (format "%s \"%s\"" path caption)))))
     ((string= type "coderef")
      (format (org-export-get-coderef-format path desc)
	          (org-export-resolve-coderef path info)))
     ((equal type "radio") desc)
     (t (if (not desc) (format "<%s>" path)
	      (format "[%s](%s)" desc path))))))


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


(provide 'org-ph-export)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-ph-export.el ends here
