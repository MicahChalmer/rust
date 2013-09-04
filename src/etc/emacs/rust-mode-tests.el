;;; rust-mode-tests.el --- ERT tests for rust-mode.el

(require 'rust-mode)
(require 'ert)
(require 'cl)

(defun rust-compare-code-after-manip (original point-pos manip-func expected got)
  (equal expected got))

(defun rust-test-explain-bad-manip (original point-pos manip-func expected got)
  (if (equal expected got)
      nil
    (list 
     ;; The (goto-char) and (insert) business here is just for
     ;; convenience--after an error, you can copy-paste that into emacs eval to
     ;; insert the bare strings into a buffer
     "Rust code was manipulated wrong after:"
     `(insert ,original)
     `(goto-char ,point-pos)
     `(expected (insert ,expected))
     `(got (insert ,got))
     (list 'first-difference-at
           (loop for i from 0
                 for oi across original
                 for ei across expected
                 do (when (not (= oi ei))
                      (return (format 
                               "(goto-char %d) (expected %S, got %S)"
                               i ei oi))))))))
(put 'rust-compare-code-after-manip 'ert-explainer 'rust-test-explain-bad-manip)

(defun rust-test-manip-code (original point-pos manip-func expected)
  (with-temp-buffer
    (rust-mode)
    (insert original)
    (goto-char point-pos)
    (funcall manip-func)
    (should (rust-compare-code-after-manip
             original point-pos manip-func expected (buffer-string)))))

;; Creating tests based on the rust code files in indentation-tests/.  See the README there.
(loop for filename in (directory-files 
                       (expand-file-name "indentation-tests"
                                         (file-name-directory (or load-file-name ".")))
                       t "\\.rs$")
      do (let ((test-name (intern (file-name-sans-extension (file-name-nondirectory filename)))))
           (eval
            `(ert-deftest ,test-name ()
               (with-temp-buffer
                 (insert-file-contents-literally ,filename)
                 (let* ((original-contents (buffer-string))
                        (deindented-contents
                         (progn
                           (goto-char 1)
                           (replace-regexp "^[[:blank:]]*" "")
                           (goto-char 1)
                           (replace-regexp "^" "      ")
                           (buffer-string))))
                   (rust-test-manip-code
                    deindented-contents 1
                    (lambda () (indent-region 1 (buffer-size)))
                    original-contents)))))))

(setq rust-test-fill-column 32)

(defun test-fill-paragraph (unfilled expected &optional start-pos end-pos)
  "We're going to run through many scenarios here--the point should be able to be anywhere from the start-pos (defaults to 1) through end-pos (defaults to the length of what was passed in) and (fill-paragraph) should return the same result.

Also, the result should be the same regardless of whether the code is at the beginning or end of the file.  (If you're not careful, that can make a difference.)  So we test each position given above with the passed code at the beginning, the end, neither and both.  So we do this a total of (end-pos - start-pos)*4 times.  Oy."
  (let* ((start-pos (or start-pos 1))
         (end-pos (or end-pos (length unfilled)))
         (padding "\n     \n")
         (padding-len (length padding)))
    (loop
     for pad-at-beginning from 0 to 1
     for pad-at-end from 0 to 1
     with padding-beginning = (if (= 0 pad-at-beginning) "" padding)
     with padding-end = (if (= 0 pad-at-end) "" padding)
     with padding-adjust = (* padding-len pad-at-beginning)
     with padding-beginning = (if (= 0 pad-at-beginning) "" padding)
     with padding-end = (if (= 0 pad-at-end) "" padding)
     for pos from (if (= 1 start-pos) 1 (+ padding-adjust start-pos)) to (+ end-pos padding-adjust)
     do (rust-test-manip-code
           (concat padding-beginning unfilled padding-end)
           pos
           (lambda () 
             (let ((fill-column rust-test-fill-column))
               (fill-paragraph)))
           (concat padding-beginning expected padding-end)))))

(ert-deftest fill-paragraph-top-level-multi-line-style-doc-comment-second-line ()
  (test-fill-paragraph 
   "/**
 * This is a very very very very very very very long string
 */"
   "/**
 * This is a very very very very
 * very very very long string
 */"))


(ert-deftest fill-paragraph-top-level-multi-line-style-doc-comment-first-line ()
  (test-fill-paragraph
   "/** This is a very very very very very very very long string
 */"
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
    (test-fill-paragraph
     multi-paragraph-unfilled
     "/**
 * This is the first really
 * really really really really
 * really really long paragraph
 *
 * This is the second really really really really really really long paragraph
 */"
     1 89)
    (test-fill-paragraph
     multi-paragraph-unfilled
     "/**
 * This is the first really really really really really really really long paragraph
 *
 * This is the second really
 * really really really really
 * really long paragraph
 */"
     90)))

(ert-deftest fill-paragraph-multi-paragraph-single-line-style-doc-comment ()
  (let
      ((multi-paragraph-unfilled
        "/// This is the first really really really really really really really long paragraph
///
/// This is the second really really really really really really long paragraph"))
    (test-fill-paragraph
     multi-paragraph-unfilled
     "/// This is the first really
/// really really really really
/// really really long paragraph
///
/// This is the second really really really really really really long paragraph"
     1 86)
    (test-fill-paragraph
     multi-paragraph-unfilled
     "/// This is the first really really really really really really really long paragraph
///
/// This is the second really
/// really really really really
/// really long paragraph"
     87)))

(ert-deftest fill-paragraph-multi-paragraph-single-line-style-indented ()
  (test-fill-paragraph
   "     // This is the first really really really really really really really long paragraph
     //
     // This is the second really really really really really really long paragraph"
   "     // This is the first really
     // really really really
     // really really really
     // long paragraph
     //
     // This is the second really really really really really really long paragraph" 1 89))

(ert-deftest fill-paragraph-multi-line-style-inner-doc-comment ()
  (test-fill-paragraph
   "/*! This is a very very very very very very very long string
 */"
   "/*! This is a very very very
 * very very very very long
 * string
 */"))

(ert-deftest fill-paragraph-single-line-style-inner-doc-comment ()
  (test-fill-paragraph
   "//! This is a very very very very very very very long string"
   "//! This is a very very very
//! very very very very long
//! string"))

(ert-deftest fill-paragraph-prefixless-multi-line-doc-comment ()
  (test-fill-paragraph
   "/**
This is my summary. Blah blah blah blah blah. Dilly dally dilly dally dilly dally doo.

This is some more text.  Fee fie fo fum.  Humpty dumpty sat on a wall.
*/"
   "/**
This is my summary. Blah blah
blah blah blah. Dilly dally
dilly dally dilly dally doo.

This is some more text.  Fee fie fo fum.  Humpty dumpty sat on a wall.
*/" 4 90))

(ert-deftest fill-paragraph-single-line-inline-comment ()
  "I'm not sure what this scenario should even do.  I'm leaving this in here to mark that uncertainty, expecting to fail for now."
  :expected-result :failed
  (test-fill-paragraph
   "other stuff       /* This is a very very very very very very very long string */"
   "other stuff /* This is a very
             * very very very
             * very very very
             * long string */"
   20))

(ert-deftest fill-paragraph-with-no-space-after-star-prefix ()
  (test-fill-paragraph
   "/** 
 *This is a very very very very very very very long string
 */"
   "/** 
 *This is a very very very very
 *very very very long string
 */"))

(defun test-auto-fill (initial position inserted expected)
  (rust-test-manip-code 
   initial
   position
   (lambda ()
     (unwind-protect
         (progn
           (let ((fill-column rust-test-fill-column))
             (auto-fill-mode)
             (goto-char position)
             (insert inserted)
             (syntax-ppss-flush-cache 1)
             (funcall auto-fill-function)))
       (auto-fill-mode t)))
   expected))

(ert-deftest auto-fill-multi-line-doc-comment ()
  (test-auto-fill
   "/**
 * 
 */"
   8
   "This is a very very very very very very very long string"
   "/**
 * This is a very very very very
 * very very very long string
 */"))

(ert-deftest auto-fill-single-line-doc-comment ()
  (test-auto-fill
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

(ert-deftest auto-fill-multi-line-prefixless ()
  (test-auto-fill
   "/*

 */"
   4
   "This is a very very very very very very very long string"
   "/*
This is a very very very very
very very very long string
 */"
   ))
