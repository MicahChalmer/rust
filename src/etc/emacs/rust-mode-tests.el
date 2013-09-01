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

(defun test-paragraph-fill (unfilled position expected-result)
  (with-temp-buffer
    (rust-mode)
    (insert unfilled)
    (goto-char position)
    (let ((fill-column 32)) (fill-paragraph))
    (should (equal expected-result (buffer-string)))))

(ert-deftest fill-paragraph-top-level-multi-line-style-doc-comment-second-line ()
  (test-paragraph-fill 
   "/** 
 * This is a very very very very very very very long string
 */"
   1
   "/** 
 * This is a very very very very
 * very very very long string
 */"))

(ert-deftest fill-paragraph-top-level-multi-line-style-doc-comment-first-line ()
  (test-paragraph-fill
   "/** This is a very very very very very very very long string
 */"
   1
   "/** This is a very very very
 * very very very very long
 * string
 */"))

(ert-deftest fill-paragraph-multi-paragraph-multi-line-style-doc-comment ()
  (let
      ((multi-paragraph-unfilled
        "/**
 * This is the first really really really really really really really long paragraph
 *
 * This is the second really really really really really really long paragraph
 */"))
    (test-paragraph-fill
     multi-paragraph-unfilled
     7
     "/**
 * This is the first really
 * really really really really
 * really really long paragraph
 *
 * This is the second really really really really really really long paragraph
 */")
    (test-paragraph-fill
     multi-paragraph-unfilled
     98
     "/**
 * This is the first really really really really really really really long paragraph
 *
 * This is the second really
 * really really really really
 * really long paragraph
 */")))

(ert-deftest fill-paragraph-multi-paragraph-single-line-style-doc-comment ()
  (let
      ((multi-paragraph-unfilled
        "/// This is the first really really really really really really really long paragraph
///
/// This is the second really really really really really really long paragraph"))
    (test-paragraph-fill
     multi-paragraph-unfilled
     7
     "/// This is the first really
/// really really really really
/// really really long paragraph
///
/// This is the second really really really really really really long paragraph")
    (test-paragraph-fill
     multi-paragraph-unfilled
     98
     "/// This is the first really really really really really really really long paragraph
///
/// This is the second really
/// really really really really
/// really long paragraph")))

(ert-deftest fill-paragraph-multi-line-style-inner-doc-comment ()
  (test-paragraph-fill
   "/*! This is a very very very very very very very long string
 */"
   1
   "/*! This is a very very very
 * very very very very long
 * string
 */"))

(ert-deftest fill-paragraph-single-line-style-inner-doc-comment ()
  (test-paragraph-fill
   "//! This is a very very very very very very very long string"
   1
   "//! This is a very very very
//! very very very very long
//! string"))

(ert-deftest fill-paragraph-prefixless-multi-line-doc-comment ()
  (test-paragraph-fill
   "/**

This is my summary. Blah blah blah blah blah.

This is some more text.  Fee fie fo fum.  Humpty dumpty sat on a wall.
*/"
   4
   "/**
This is my summary. Blah blah
blah blah blah.

This is some more text.  Fee fie fo fum.  Humpty dumpty sat on a wall.
*/"))

(defun test-paragraph-auto-fill (initial position inserted expected-result)
  (with-temp-buffer
    (auto-fill-mode)
    (rust-mode)
    (let ((fill-column 32))
      (insert initial)
      (goto-char position)
      (insert inserted)
      (funcall (or auto-fill-function (function do-auto-fill)))
      (should (equal expected-result (buffer-string))))))

(ert-deftest auto-fill-multi-line-doc-comment ()
  (test-paragraph-auto-fill
   "/**
 * 
 */"
   7
   "This is a very very very very very very very long string"
   "/** 
 * This is a very very very very
 * very very very long string
 */"))

(ert-deftest auto-fill-single-line-doc-comment ()
  (test-paragraph-auto-fill
   "/// This is the first really
/// really really really really
/// really really long paragraph
///
/// "
   103
   "This is the second really really really really really really long paragraph"
    "/// This is the first really
/// really really really really
/// really really long paragraph
///
/// This is the second really
/// really really really really
/// really long paragraph"
    ))
