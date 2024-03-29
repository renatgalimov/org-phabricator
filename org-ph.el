;;; org-ph.el --- Phabricator interaction with Org-mode -*- lexical-binding: t -*-
;;
;; Filename: org-ph.el
;; Description:
;; Author: Renat Galimov
;; Maintainer:
;; Created: Пт янв 15 20:33:19 2021 (+0300)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Tue Feb 22 04:23:57 2022 (+0300)
;;           By: Renat Galimov
;;     Update #: 39
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

;;;; Dependencies

(require 'org)

(defgroup org-phabricator nil
  "Emacs integration with Phabricator tool."
  :tag "Org-Phabricator"
  :group 'org)

;;;; Features
(require 'org-ph-export)
(require 'org-ph-fetch)
(require 'org-ph-upload)
(provide 'org-ph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-ph.el ends here
