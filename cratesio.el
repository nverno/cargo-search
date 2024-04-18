;;; cratesio.el --- Search crates.io -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/cratesio-search
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (dash "2.19"))
;; Created: 27 March 2024
;; Keywords:

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Search crates on crates.io.
;; Jump to crate's homepage, repository, documentation.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'dash))
(require 'transient)

(defvar cratesio-search-fields
  '("homepage" "repository" "documentation" "description"
    "max_version" "max_stable_version")
  "Default data to search for crate.")

(defvar cratesio-search-cache (make-hash-table :test #'equal))

(defvar cratesio-search-debug nil)

(defsubst cratesio-search--read-crate (&optional prompt)
  (completing-read (or prompt "Crate: ") (hash-table-keys cratesio-search-cache)))

(defsubst cratesio-search--read-kind (&optional prompt)
  (completing-read (or prompt "Lookup: ") cratesio-search-fields))

(defsubst cratesio-search-msg (interactive msg &rest args)
  (when interactive
    (let ((message-log-max nil))
      (apply #'message msg args))))

(defun cratesio-search--create-callback (callback)
  (lambda (p m)
    (require 'json)
    (let* ((buf (process-buffer p))
           (live-p (buffer-live-p buf)))
      (unwind-protect
          (if (and live-p (eq (process-status p) 'exit)
                   (zerop (process-exit-status p)))
              (with-current-buffer buf
                (goto-char (point-min))
                (funcall callback
                         (seq-map
                          (lambda (h) (puthash (gethash "name" h) h cratesio-search-cache))
                          (json-parse-buffer
                           :null-object ':null
                           :false-object nil))))
            (user-error "%s: %d" m (process-exit-status p)))
        (unless (or (not live-p) cratesio-search-debug)
          (kill-buffer buf))))))

(defun cratesio-search--exact (crate kind)
  (cratesio-search crate (lambda (_hash)
                                 (--when-let (gethash crate cratesio-search-cache)
                                   (let ((res (gethash kind it)))
                                     (or res ':null))))
                         "--per-page=1"))

(defun cratesio-search--callback-action (crate kind &optional action)
  (lambda (_h)
    (--if-let (gethash crate cratesio-search-cache)
        (let ((ref (gethash kind it)))
          (if (memq ref '(:null nil))
              (user-error "%s has no '%s'" crate kind)
            (funcall (or action #'browse-url) ref)))
      (user-error "%s not found" crate))))

;;;###autoload
(defun cratesio-search (query &optional callback &rest args)
  "Search QUERY using crates.io.
Query: <search> [OPTIONS]."
  (interactive (list (cratesio-search--read-crate "Crates.io: ") nil
                     (transient-args transient-current-command)))
  (or callback (setq callback #'cratesio--show-results))
  (setq args (cons (format "--query=%s" query) (flatten-tree args)))
  (if-let ((crate (gethash query cratesio-search-cache)))
      (funcall callback crate)
    (set-process-sentinel
     (apply #'start-process
            "cratesio" (generate-new-buffer-name "*cratesio*") "cratesio" args)
     (cratesio-search--create-callback callback))))

;;;###autoload
(defun cratesio-browse-documentation (crate)
  (interactive (list (cratesio-search--read-crate)))
  (cratesio-search
   crate (cratesio-search--callback-action crate "documentation") "-c1"))

;;;###autoload
(defun cratesio-browse-homepage (crate)
  (interactive (list (cratesio-search--read-crate)))
  (cratesio-search
   crate (cratesio-search--callback-action crate "homepage") "-c1"))

;;;###autoload
(defun cratesio-browse-repository (crate)
  (interactive (list (cratesio-search--read-crate)))
  (cratesio-search
   crate (cratesio-search--callback-action crate "repository") "-c1"))

;; -------------------------------------------------------------------
;;; Results

(defvar cratesio-results-buffer "*cratesio-search-results*")

(defcustom cratesio-results-columns
  '("name" "description"
    "homepage" "repository" "documentation"
    "max_version" "max_stable_version")
  "Columns to display in crates result."
  :type '(repeat string)
  :group 'cratesio)

(defun cratesio--show-results (results)
  (or (listp results) (setq results (list results)))
  (let (entries)
    (dolist (h results)
      (let ((arr (make-vector (length cratesio-results-columns) "")))
        (--each-indexed cratesio-results-columns
          (let ((v (gethash it h "")))
            (aset arr it-index (if (eq v ':null) "" v))))
        (push (list (gethash "name" h) arr) entries)))
    (if (null entries)
        (message "No results")
      (with-current-buffer (get-buffer-create cratesio-results-buffer)
        (let ((inhibit-read-only t))
          (erase-buffer))
        (setq-local tabulated-list-entries entries)
        (cratesio-results-mode)
        (tabulated-list-print)
        (pop-to-buffer (current-buffer))))))

(defun cratesio-results-browse (kind)
  "Browse url KIND of crate at point.
KIND is one homepage, documentation, or repository."
  (interactive
   (list (completing-read
          "Browse: " '("homepage" "documentation" "repository") nil t)))
  (let* ((e (tabulated-list-get-entry))
         (idx (- (length cratesio-results-columns)
                 (length (member kind cratesio-results-columns))))
         (url (aref e idx)))
    (when (equal "" url)
      (user-error "%s has no '%s'" (aref e 0) kind))
    (browse-url url)))

(defvar-keymap cratesio-results-mode-map
  :doc "Keymap for cratesio results"
  "w" #'cratesio-results-browse)

(define-derived-mode cratesio-results-mode tabulated-list-mode "Crates"
  "List crates from search results.

Commands:
\\<cratesio-results-mode-map>"
  (setq tabulated-list-format [("Name" 18 t)
                               ("Description" 50 nil)
                               ("Latest" 8 nil)
                               ("Stable" 8 nil)
                               ("Homepage" 15 nil)
                               ("Repository" 15 nil)
                               ("Documentation" 15 nil)])
  (tabulated-list-init-header)
  (setq tabulated-list-sort-key '("Name" . nil)))

;; -------------------------------------------------------------------
;;; Menu

(defvar cratesio-search-defaults
  '("--page=1" "--per-page=5" "--sort=relevance" "--fields=default"))

;;;###autoload(autoload 'cratesio-menu "cratesio" nil t)
(transient-define-prefix cratesio-menu ()
  :value cratesio-search-defaults
  [["Browse crate"
    ("d" "Documentation" cratesio-browse-documentation)
    ("h" "Homepage" cratesio-browse-homepage)
    ("r" "Repository" cratesio-browse-repository)]
   ["Search"
    ("s" "Search crates.io" cratesio-search)]
   ["Options"
    ("-p" "Page number" ("-p" "--page=") :class transient-option)
    ("-c" "Results per page" ("-c" "--per-page=") :class transient-option)
    ("-s" "Sort results" ("-s" "--sort=")
     :choices
     ("alpha" "relevance" "downloads" "recent-downloads" "recent-updates" "new")
     :class transient-option)
    ("-f" "Data fields to keep" ("-f" "--fields=")
     :choices
     ("default" "description" "documentation" "exact_match" "homepage" "id"
      "max_stable_version" "max_version" "name" "repository")
     :class transient-option)
    ("-r" "Dont serialize output as JSON" ("-r" "--raw"))]])

(provide 'cratesio)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; cratesio.el ends here
