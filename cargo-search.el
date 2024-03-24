;;; cargo-search.el --- Interface to search and add cargo dependencies -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/cargo-search
;; Package-Requires: ((emacs "25.3") (dash "2.19"))
;; Version: 0.1.0

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
;; Search cargo crate.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'dash))


(defvar cargo-search-cache (make-hash-table :test #'equal))

(defvar cargo-search-debug nil)

(defvar cargo-search-kinds
  '("homepage" "repository" "documentation" "description"
    "max_version" "max_stable_version")
  "Default data to search for crate.")

(defun cargo-search--create-callback (callback)
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
                          (lambda (h) (puthash (gethash "name" h) h cargo-search-cache))
                          (json-parse-buffer
                           :null-object nil
                           :false-object nil))))
            (user-error "%s: %d" m (process-exit-status p)))
        (unless (or (not live-p) cargo-search-debug)
          (kill-buffer buf))))))

(defun cargo-search--read-crate (&optional prompt)
  (completing-read (or prompt "Crate: ") (hash-table-keys cargo-search-cache)))

(defun cargo-search--read-kind (&optional prompt)
  (completing-read (or prompt "Lookup: ") cargo-search-kinds))

(defun cargo-search-cratesio (query &optional callback)
  "Search QUERY using crates.io.
Query: <search> [OPTIONS]."
  (interactive (list (read-string "Crates.io: ")))
  (let* ((args (split-string-and-unquote query))
         (query (car args)))
    (if (string-match-p (rx (or "-q" "--query")) (car args))
        (setq query (cadr args))
      (push "-q" args))
    (if-let ((crate (gethash query cargo-search-cache)))
        (if callback (funcall callback crate) crate)
      (ignore
       (set-process-sentinel
        (apply #'start-process "cratesio"
               (generate-new-buffer-name "*cratesio*") "cratesio" args)
        (cargo-search--create-callback (or callback #'ignore)))))))

(defun cargo-search--exact (crate kind)
  (cargo-search-cratesio
   (concat crate " -c1")
   (lambda (_hash)
     (--when-let (gethash crate cargo-search-cache)
       (let ((res (gethash kind it)))
         (or res ':null))))))

(defsubst cargo-search-msg (interactive msg &rest args)
  (when interactive
    (let ((message-log-max nil))
      (apply #'message msg args))))

;;;###autoload
(defun cargo-search (&optional query crate kind interactive)
  "With prefix QUERY crates.io.
Otherwise, lookup data KIND for CRATE.
When call is INTERACTIVE and KIND is a website, browse the result."
  (interactive
   (if current-prefix-arg (list t nil nil t)
     (list nil (cargo-search--read-crate) (cargo-search--read-kind) t)))
  (if (or query (not crate))
      (call-interactively #'cargo-search-cratesio)
    (let ((it (cargo-search--exact crate kind)))
      (pcase it
        (':null (cargo-search-msg interactive "%s has no '%s'" crate kind))
        ((pred null)
         (cargo-search-msg interactive "fetching %s for %s..." kind crate))
        ((or "documentation" "homepage" "repository")
         (if interactive (browse-url it) it))
        (_ (if interactive (message it) it))))))

(defun cargo-search-documentation (crate &optional interactive)
  (interactive (list (cargo-search--read-crate) t))
  (cargo-search nil crate "documentation" interactive))


;; ------------------------------------------------------------

;; process buffer for cargo search
(defvar cargo-search-buffer "*cargo-search*")

(defvar cargo-search-limit 30)

;;;###autoload
(defun cargo-search-crates (query)
  "Start \\='cargo search QUERY\\=' process in the background.
When it completes, the result is converted into a tabulated list in
`cargo-search-mode' and that buffer is brought into focus."
  (interactive (list (read-string "Search for: ")))
  (with-current-buffer (get-buffer-create cargo-search-buffer)
    (let ((inhibit-read-only t)) (erase-buffer)))
  (set-process-sentinel
   (start-process "cargo-search" cargo-search-buffer "cargo"
                  "search" query "--limit" (number-to-string cargo-search-limit))
   #'cargo-search-process-result))

;; regexp to match results
(defvar cargo-search-re
  "\\(\\S-+\\) = \"\\([0-9.]+\\)\"\\s-*\\([^\n]*\\)")
;; "\\(^[a-zA-Z][^ ]+\\)\\s-*(\\([.0-9]+\\))\\s-*\\([^\n]*\\)"
  
;; structure to hold crate info
(cl-defstruct (cargo-search-crate
               (:constructor cargo-search-crate--create))
  name version desc)

;; create entries for crates in table
(defun cargo-search--table-entry (item)
  (cl-destructuring-bind (name . crate) item
    (list name (vector (cargo-search-crate-name crate)
                       (cargo-search-crate-version crate)
                       (cargo-search-crate-desc crate)))))

(defun cargo-search--table-entries (crates)
  (nreverse (mapcar 'cargo-search--table-entry crates)))

;; munge cargo search results
(defun cargo-search-process-result (p _m)
  (when (zerop (process-exit-status p))
    (let ((inhibit-read-only t)
          crates)
      (with-current-buffer cargo-search-buffer
        (goto-char (point-min))
        (while (not (looking-at-p "^[^ \t]"))
          (forward-line 1))
        ;; make crates
        (while (looking-at cargo-search-re)
          (push (cons (match-string 1)
                      (cargo-search-crate--create
                       :name (match-string 1)
                       :version (match-string 2)
                       :desc (match-string 3)))
                crates)
          (forward-line 1))
        ;; create tabulated list interface
        (setq tabulated-list-entries (cargo-search--table-entries crates))
        (erase-buffer)
        (cargo-search-mode)
        (tabulated-list-print)
        (pop-to-buffer (current-buffer))))))

;;; Cargo search mode functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; last known project root
(defvar-local cargo-search-project-root nil)

;; find project's Cargo.toml, or prompt if unable
(defun cargo-search-root ()
  (if-let ((root (locate-dominating-file
                    (or default-directory buffer-file-name)
                    "Cargo.toml")))
      (expand-file-name "Cargo.toml" (setq cargo-search-project-root
                                           root))
    (let ((default-directory
            (read-directory-name
             (format "Can't find Cargo.toml, %s: "
                     (if cargo-search-project-root
                         "use previous or reselect"
                       "choose directory"))
             (or cargo-search-project-root default-directory))))
      (cargo-search-root))))

;; check if dependency is already listed, return version
;; and line position if so
(defun cargo-search-check-dep (name)
  (goto-char (point-min))
  (when (search-forward name nil t)
    (and (looking-at ".*\"\\([0-9.]+\\)\"")
         (cons (match-string 1) (line-beginning-position)))))

;; open Cargo.toml, insert dependency
(defun cargo-search-add-crate (crate &optional no-prompt)
  "Add CRATE to cargo.toml.
Dont prompt if NO-PROMPT is non-nil."
  (interactive (list (ignore-errors (tabulated-list-get-entry))))
  (unless crate (user-error "No crate at point."))
  ;; flash selected line
  (cargo-search-blink-region (line-beginning-position)
                             (line-end-position))
  (when-let ((toml (cargo-search-root))
             (do-it t))
    (cl-destructuring-bind (name version _desc) (append crate ())
      (and (or no-prompt
               (y-or-n-p (format "Add dependency for %s %s? " name version)))
           (with-current-buffer (find-file-noselect toml t)
             (when-let ((ver (cargo-search-check-dep name)))
               (setq do-it
                     (or no-prompt
                         (y-or-n-p
                          (format "%s was found with version %s. Overwrite? "
                                  name (car ver)))))
               (when do-it
                 ;; remove offending line
                 (goto-char (cdr ver))
                 (delete-region (1- (point)) (line-end-position))))
             (if (not do-it)
                 (message "No dependency added.")
               ;; find location to insert new crate
               (goto-char (point-min))
               (unless (search-forward "[dependencies]" nil t)
                 (goto-char (point-max))
                 (insert "\n[dependencies]"))
               ;; insert dependency
               (insert (format "\n%s = \"%s\"" name version)))
             (write-file toml nil)
             (kill-buffer (current-buffer)))))))

;;; Indicator: blink crate when adding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cargo-search-blink-delay 0.4)

(defvar cargo-search-current-region-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face 'highlight)
    overlay))

(defun cargo-search-blink-region (start end)
  (move-overlay cargo-search-current-region-overlay start end)
  (run-with-timer cargo-search-blink-delay nil
                  #'(lambda ()
                      (delete-overlay
                       cargo-search-current-region-overlay))))

;;; Cargo search mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cargo-search-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "RET") 'cargo-search-add-crate)
    km))

(define-derived-mode cargo-search-mode tabulated-list-mode "CargoSearch"
  "Crates found by cargo search.

Commands:
\\<cargo-search-mode-map>"
  (setq tabulated-list-format [("name" 15 nil)
                               ("version" 7 nil)
                               ("description" 60 nil)])
  (tabulated-list-init-header))

(provide 'cargo-search)
;;; cargo-search.el ends here
