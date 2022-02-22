;; -*- lexical-binding: t -*-
;;; org-ph-fetch.el --- Allow fetching Phabricator tasks as org-mode TODOs
;;
;; Filename: org-ph-fetch.el
;; Description:
;; Author: Renat Galimov
;; Maintainer:
;; Created: Вс янв 17 11:50:40 2021 (+0300)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Tue Feb 22 03:29:00 2022 (+0300)
;;           By: Renat Galimov
;;     Update #: 143
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


(require 'url)
(require 'request)
(require 'generator)
(require 'org)

(defgroup org-phabricator-fetch nil
  "Configure importing Phabricator tickets as `org-mode' nodes."
  :tag "Org-Phabricator Fetch"
  :group 'org-phabricator)


(defcustom org-ph-fetch-api-url "https://secure.phabricator.com/api"
  "URL which all `org-phabricator' functions will use.
You can get it from the examples section in any API documentation page."
  :tag "API URL"
  :type 'string
  :group 'org-phabricator)

(defcustom org-ph-fetch-api-token ""
  "Token used to authenticate API requests.
Get it at \"https://phabricator.example.com/conduit/login\"."
  :tag "API Token"
  :type 'string
  :group 'org-phabricator
  )

(defcustom org-ph-fetch-user-id ""
  "Your user id in phabricator.
Is't how others mention you in ph.
Don't include the leading \"@\" when setting it up here."
  :tag "User ID"
  :type 'string
  :group 'org-phabricator)

(defvar org-ph-fetch-see-tasks-buffer-name "*Phabricator*")

;;
(defun org-ph-fetch-endpoint-url (endpoint)
  "Make phabricator URL for the `ENDPOINT`."
  (let* ((url (url-generic-parse-url org-ph-fetch-api-url))
         (filename (expand-file-name  endpoint (url-filename url))))
    (setf (url-filename url) filename)
    (url-recreate-url url)))


(defun org-ph-fetch--tasks-constraints (&rest args)
  "Create constratints argument for a query function.

ARGS -
    :ids      - an integer list of task ids
    :statuses - a string list of possible statuses
    :assigned - a string list of assignee PHIDs
    :phids    - a string list of phids"
  (let ((i 0)
        (constraints '())
        (ids (or (plist-get args :ids) '()))
        (phids (or (plist-get args :phids) '()))
        (statuses (or (plist-get args :statuses) '()))
        (assigned (or (plist-get args :assigned) '())))
    (if (not (sequencep ids))
        (error ":ids should be of a sequence type"))
    (if (not (sequencep phids))
        (error ":phids should be of a sequence type"))
    (if (not (sequencep statuses))
        (error ":statuses should be of a sequence type"))
    (if (not (sequencep assigned))
        (error ":assigned should be of a sequence type"))

    (dolist (id ids)
      (let ((query-arg (format "constraints[ids][%d]" i)))
        (push `(,query-arg . ,id) constraints))
      (setq i (1+ i)))

    (setq i 0)
    (dolist (phid phids)
      (let ((query-arg (format "constraints[phids][%d]" i)))
        (push `(,query-arg . ,phid) constraints))
      (setq i (1+ i)))

    (setq i 0)
    (dolist (user-phid assigned)
      (let ((query-arg (format "constraints[assigned][%d]" i)))
        (push `(,query-arg . ,user-phid) constraints))
      (setq i (1+ i)))

    (setq i 0)
    (dolist (status statuses)
      (let ((query-arg (format "constraints[statuses][%d]" i)))
        (push `(,query-arg . ,status) constraints))
      (setq i (1+ i)))

    constraints
    ))


(defun org-ph-fetch-upload-file (filename)
  "Upload a file from FILENAME.
Returns phabricator file id like FXXXX"
  (let* ((file-content
	      (with-temp-buffer
	        (insert-file-contents-literally filename)
	        (base64-encode-region (point-min) (point-max) t)
	        (buffer-string)))
         (data `(("api.token" . ,org-ph-fetch-api-token)
                 ("data_base64" . ,file-content)
                 ("name" .  ,(file-name-nondirectory filename))))
         (phid nil)
         (error-info nil)
         )

    (request (org-ph-fetch-endpoint-url "file.upload")
      :parser 'json-read
      :type "POST"
      :data data
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (when data
                    (setq error-info (alist-get 'error_info data))
                    (when (not (string-blank-p (or error-info "")))
                      (error "Cannot upload file: %s" error-info))
                    (setq phid (alist-get 'result data)))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (error "Can't upload file: %S" (alist-get 'result error-thrown)))))

    (let ((data `(("api.token" . ,org-ph-fetch-api-token)
                  ("constraints[phids][0]" . ,phid)))
          (file-id nil))
      (request (org-ph-fetch-endpoint-url "file.search")
        :parser 'json-read
        :type "POST"
        :data data
        :sync t
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (when data
                      (setq error-info (alist-get 'error_info data))
                      (when (not (string-blank-p (or error-info "")))
                        (error "Cannot upload file: %s" error-info))
                      (setq file-id (format "F%s" (alist-get 'id (elt (alist-get 'data (alist-get 'result data)) 0)))))))
        :error (cl-function
                (lambda (&key error-thrown &allow-other-keys)
                  (error "Can't upload file: %S" (alist-get 'result error-thrown)))))
      file-id)))


(iter-defun org-ph-fetch-iter-tasks (&rest args)
  "Fetch all tasks in open state that are assigned to me"

  (let* ((tasks [])
         (i 0)
         (error-info nil)
         (after (plist-get args :after))
         (constraints (apply 'org-ph-fetch--tasks-constraints args))
         (data `(("api.token" . ,org-ph-fetch-api-token)
                 ("limit" . "100"))))
    (if after
        (push `(after . ,after) data)
      )

    (dolist (constraint constraints)
      (push constraint data)
      )

    (request (org-ph-fetch-endpoint-url "maniphest.search")
      :parser 'json-read
      :type "POST"
      :data data
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (when data
                    (setq error-info (alist-get 'error_info data))
                    (when (not error-info)
                      (setq after (alist-get 'after (alist-get 'cursor (alist-get 'result data))))
                      (setq tasks (alist-get 'data (alist-get 'result data)))))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (error "Can't fetch tasks: %S" (alist-get 'result error-thrown)))))

    (when error-info (error "Can't fetch tasks: %s" error-info))

    (while (< i (length tasks))
      (iter-yield (elt tasks i))
      (setq i (1+ i))
      )
    (when after
      (iter-yield-from (apply 'org-ph-fetch-iter-tasks (plist-put args :after after))))))


(defun org-ph-fetch--indent (input size)
  "Indent `INPUT` with `SIZE` spaces."
  (if (not (integerp size))
      (error "Size should be an integer.  Got: %s" (type-of size))
    )

  (if (not (stringp input))
      (error "Input should be a string.  Got: %s" (type-of input))
    )
  (mapconcat (lambda (line) (concat (make-string size ? ) line)) (split-string input "\n") "\n"))


(defun org-ph-fetch-format-task (task)
  "Formats the `TASK` into `org-mode` entry."
  (let* ((fields (alist-get 'fields task))
         (description (alist-get 'raw (alist-get 'description fields)))
         (task-id (alist-get 'id task))
         (name (alist-get 'name fields)))
    (mapconcat
     'identity
     `(,(concat "* TODO " name)
       "  :PROPERTIES:"
       ,(concat "  :PHABRICATOR_ID: T" (number-to-string task-id))
       "  :END:"
       "  :PHABRICATOR_DESCRIPTION:"
       ,(org-ph-fetch--indent description 2)
       "  :END:")
     "\n")))

(defun org-ph-fetch-see-tasks (&rest args)
  "Fetched phabricator tasks into `org-mode` formatted buffer.
Fetches all tasks assigned to `org-ph-fetch-user-id`
into `org-ph-fetch-see-tasks-buffer-name`.
Notice that the function cleans the buffer before updating it.

Supported ARGS:

`exclude`: exclude given tasks from the output."
  (interactive)
  (let (
        (exclude (or (plist-get args :exclude) '()))
        (tasks (apply 'org-ph-fetch-iter-tasks args))
        )
    (with-current-buffer (get-buffer-create "*Phabricator*")
      (erase-buffer)
      (iter-do (task tasks)
        (let ((task-id (format "T%s" (alist-get 'id task))))
          (when (not (member task-id exclude))
            (insert (format "%s\n" (org-ph-fetch-format-task task))))))
      (org-mode)
      (pop-to-buffer (current-buffer)))))


(defun org-ph-fetch-get-closed-task-ids ()
  "Get id's of tasks defined in `org-agenda-buffers` and having closed status."
  (let* ((all-task-ids (org-ph-fetch--id-to-int (org-ph-fetch--get-existing-task-ids)))
         (tasks (org-ph-fetch-iter-tasks :ids all-task-ids :statuses '(resolved wontfix spite)))
         (result '()))
    (iter-do (task tasks)
      (push (alist-get 'id task) result))
    result))

(defun org-ph-fetch-update-task-statuses ()
  "Update status of closed tasks to DONE."
  (interactive)
  (let ((total-updated 0)
        (done-task-ids (org-ph-fetch-get-closed-task-ids)))
    (org-map-entries
     (lambda ()
       (let* ((current-task-id (assoc-default "PHABRICATOR_ID" (org-entry-properties)))
              (current-task-int-id (string-to-number (substring current-task-id 1 nil))))
         (when (member current-task-int-id done-task-ids)
           (org-todo "DONE")
           (setq total-updated (1+ total-updated)))))
     "+TODO=\"TODO\"&-PHABRICATOR_ID=\"\""
     'agenda)
    (message "Total updated: %s" total-updated)))

(defun org-ph-fetch--id-to-int (ids)
  "Convert IDS from user-friendly string to integer."
  (mapcar
   (lambda (id)
     (string-to-number (substring id 1 nil)))
   ids))


(defun org-ph-fetch--get-existing-task-ids ()
  "Return a list of phabricator tasks."
  (org-map-entries
   (lambda () (assoc-default "PHABRICATOR_ID" (org-entry-properties)))
   "-PHABRICATOR_ID=\"\""
   'agenda))

(defun org-ph-fetch-see-new-tasks()
  "Pop a buffer with new tasks assigned to me."
  (interactive)
  (cond
   ((or (not org-ph-fetch-user-id) (string-empty-p org-ph-fetch-user-id))
    (error "Variable org-ph-fetch-user-id is empty or not defined"))
   (t
    (org-ph-fetch-see-tasks
     :statuses '(open)
     :assigned `(,org-ph-fetch-user-id)
     :exclude (org-ph-fetch--get-existing-task-ids)))))

(provide 'org-ph-fetch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-ph-fetch.el ends here
