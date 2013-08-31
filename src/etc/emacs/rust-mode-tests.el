;;; rust-mode-tests.el --- ERT tests for rust-mode.el

(require 'rust-mode)
(require 'ert)
(require 'cl)

;; Creating tests based on the rust code files in indentation-tests/.  See the README there.
(loop for filename in (directory-files 
                       (expand-file-name "indentation-tests"
                                         (file-name-directory (or load-file-name ".")))
                       t "\\.rs$")
      do (let ((test-name (intern (file-name-sans-extension (file-name-nondirectory filename)))))
           (eval
            `(ert-deftest ,test-name ()
               (with-temp-buffer
                 (rust-mode)
                 (insert-file-contents-literally ,filename)
                 (let ((original-contents (buffer-string)))
                   (replace-regexp "^[[:space:]]*" "") ; Remove all existing indentation
                   (replace-regexp "^" "      ") ; Add some extra space to make sure it's taken away
                   (indent-region 1 (buffer-size))
                   (should (equal original-contents (buffer-string)))
                   ))))))
