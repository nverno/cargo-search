(require 'ert)
(require 'cargo-search)

(defvar cargo--test-file
  (expand-file-name "Cargo.toml" temporary-file-directory))

(defmacro cargo--should-respond (init expect reply &rest body)
  "BODY should return IT to be compared with EXPECT. REPLY is keys to respond
to requested input."
  `(progn
     (ignore-errors (delete-file cargo--test-file))
     (with-temp-buffer
       (insert ,init)
       (write-file cargo--test-file nil))
     (let ((default-directory temporary-file-directory)
           (unread-command-events
            ,(when reply `(listify-key-sequence (kbd ,reply)))))
       ,@body
       (should (string= it ,expect)))))

(defmacro cargo--should-insert (init expect reply &rest body)
  "Insert INIT into temporary Cargo.toml, evaluate BODY in that 
directory, then compare EXPECT to contents of Cargo.toml."
  `(progn
     (ignore-errors (delete-file cargo--test-file))
     (with-temp-buffer
       (insert ,init)
       (write-file cargo--test-file nil))
     (let ((default-directory temporary-file-directory)
           (unread-command-events
            ,(when reply `(listify-key-sequence (kbd ,reply)))))
       ,@body
       (with-temp-buffer
         (insert-file-contents cargo--test-file)
         (should (string=
                  (buffer-substring-no-properties (point-min)
                                                  (point-max))
                  ,expect))))))

;; ------------------------------------------------------------

(ert-deftest cargo--test-root ()
  "cargo search should find root"
  (cargo--should-respond
   "" cargo--test-file nil
   (setq it (cargo-search-root))))

(ert-deftest cargo--test-check-dep-1 ()
  "cargo search should find deps"
  (cargo--should-respond
   "[dependencies]
regex = \"0.0.1\""
   "0.0.1" nil
   (with-temp-buffer
     (insert-file-contents cargo--test-file)
     (setq it (car (cargo-search-check-dep "regex"))))))

(ert-deftest cargo--test-check-dep-2 ()
  ""
  (cargo--should-respond
   "[dependencies]"
   nil nil
   (with-temp-buffer
     (insert-file-contents cargo--test-file)
     (setq it (car (cargo-search-check-dep "regex"))))))

(ert-deftest cargo--test-add-crate-1 ()
  "adds new crate"
  (cargo--should-insert
   "[dependencies]"
   "[dependencies]
regex = \"1.1.1\""
   "y RET"
   (cargo-search-add-crate ["regex" "1.1.1" "ababb"] t)))

(ert-deftest cargo--test-add-crate-2 ()
  "overwrite crate"
  (cargo--should-insert
   "[dependencies]
regex = \"1.0.0\""
   "[dependencies]
regex = \"1.1.1\""
   "y RET"
   (cargo-search-add-crate ["regex" "1.1.1" "ababb"] t)))

(ert-deftest cargo--test-add-crate-3 ()
  "overwrite crate"
  (cargo--should-insert
   "[dependencies]
blah = \"1.1.1\""
   "[dependencies]
regex = \"1.1.1\"
blah = \"1.1.1\""
   "y RET"
   (cargo-search-add-crate ["regex" "1.1.1" "ababb"] t)))

(defun cargo--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "rust--test")
    (message "Can't run tests without ert.")))

(provide 'cargo-search-tests)
