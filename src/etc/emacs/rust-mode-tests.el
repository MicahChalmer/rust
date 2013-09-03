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
     "Rust code was manipulated wrong after"
     `(goto-char ,point-pos)
     `(original (insert ,original))
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

(defun test-fill-paragraph (unfilled position expected-result)
  (rust-test-manip-code
   unfilled
   position
   (lambda () (let ((fill-column rust-test-fill-column)) (fill-paragraph)))
   expected-result))

(ert-deftest fill-paragraph-top-level-multi-line-style-doc-comment-second-line ()
  (test-fill-paragraph 
   "
/**
 * This is a very very very very very very very long string
 */"
   2
   "
/**
 * This is a very very very very
 * very very very long string
 */"))

(ert-deftest fill-paragraph-top-level-multi-line-style-doc-comment-second-line-top-of-file ()
  (test-fill-paragraph 
   "/**
 * This is a very very very very very very very long string
 */"
   2
   "/**
 * This is a very very very very
 * very very very long string
 */"))

(ert-deftest fill-paragraph-top-level-multi-line-style-doc-comment-first-line ()
  (test-fill-paragraph
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
        "
/**
 * This is the first really really really really really really really long paragraph
 *
 * This is the second really really really really really really long paragraph
 */"))
    (test-fill-paragraph
     multi-paragraph-unfilled
     7
     "
/**
 * This is the first really
 * really really really really
 * really really long paragraph
 *
 * This is the second really really really really really really long paragraph
 */")
    (test-fill-paragraph
     multi-paragraph-unfilled
     98
     "
/**
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
    (test-fill-paragraph
     multi-paragraph-unfilled
     7
     "/// This is the first really
/// really really really really
/// really really long paragraph
///
/// This is the second really really really really really really long paragraph")
    (let ((with-second-paragraph-filled "/// This is the first really really really really really really really long paragraph
///
/// This is the second really
/// really really really really
/// really long paragraph"))
      (test-fill-paragraph
       multi-paragraph-unfilled
       98 ;; Within second paragraph
       with-second-paragraph-filled)
      (test-fill-paragraph
       multi-paragraph-unfilled
       90 ;; End of the separator line in the middle
       with-second-paragraph-filled))))

(ert-deftest fill-paragraph-multi-paragraph-single-line-style-indented ()
  (test-fill-paragraph
   "     // This is the first really really really really really really really long paragraph
     //
     // This is the second really really really really really really long paragraph"
   7 ;; In between the //
   "     // This is the first really
     // really really really
     // really really really
     // long paragraph
     //
     // This is the second really really really really really really long paragraph"))

(ert-deftest fill-paragraph-multi-line-style-inner-doc-comment ()
  (test-fill-paragraph
   "/*! This is a very very very very very very very long string
 */"
   1
   "/*! This is a very very very
 * very very very very long
 * string
 */"))

(ert-deftest fill-paragraph-single-line-style-inner-doc-comment ()
  (test-fill-paragraph
   "//! This is a very very very very very very very long string"
   1
   "//! This is a very very very
//! very very very very long
//! string"))

(ert-deftest fill-paragraph-prefixless-multi-line-doc-comment ()
  (test-fill-paragraph
   "/**
This is my summary. Blah blah blah blah blah. Dilly dally dilly dally dilly dally doo.

This is some more text.  Fee fie fo fum.  Humpty dumpty sat on a wall.
*/"
   4
   "/**
This is my summary. Blah blah
blah blah blah. Dilly dally
dilly dally dilly dally doo.

This is some more text.  Fee fie fo fum.  Humpty dumpty sat on a wall.
*/"))

(ert-deftest fill-paragraph-single-line-inline-comment ()
  "I'm not sure what this scenario should even do.  I'm leaving this in here to mark that uncertainty, expecting to fail for now.")
  :expected-result :failed
  (test-fill-paragraph
   "other stuff       /* This is a very very very very very very very long string */"
   4
"other stuff /* This is a very
             * very very very
             * very very very
             * long string */"))

(ert-deftest fill-paragraph-multi-positions-near-last-line ()
  (let ((test-comment
"
/** 
 * This is a very very very very very very very long string
 */")
        (correctly-filled
"
/** 
 * This is a very very very very
 * very very very long string
 */"))
    (test-fill-paragraph
     test-comment
     66 ;; At end of the text line
     correctly-filled)
    (test-fill-paragraph
     test-comment
     67 ;; At beginning of comment-closing line
     correctly-filled)
))

(ert-deftest fill-paragraph-multi-line-non-doc-comment-first-line ()
  (let ((test-comment 
"/*
 * This is a very very very very very very very long string
 */"
)
    (correctly-filled
"/*
 * This is a very very very very
 * very very very long string
 */"))
    (test-fill-paragraph
     test-comment
     1
     correctly-filled)))

(ert-deftest fill-paragraph-with-no-space-after-star-prefix ()
  (let ((test-comment
"
/** 
 *This is a very very very very very very very long string
 */")
        (correctly-filled
"
/** 
 *This is a very very very very
 *very very very long string
 */"))
    (test-fill-paragraph
     test-comment
     4 ;; in the middle of the /**
     correctly-filled)
    (test-fill-paragraph
     test-comment
     62 ;; On second line of comment
     correctly-filled)
    (test-fill-paragraph
     test-comment
     66 ;; On last line of comment
     correctly-filled)))

(defun test-auto-fill (initial position inserted expected-result)
  (rust-test-manip-code
   initial
   position
   (lambda ()
     (unwind-protect
         (progn
           (let ((fill-column 32))
             (auto-fill-mode)
             (goto-char position)
             (insert inserted)
             (funcall (or auto-fill-function (function do-auto-fill)))
             (auto-fill-mode t)))))
   expected-result))

(ert-deftest auto-fill-multi-line-doc-comment ()
  (test-auto-fill
   "
/**
 * 
 */"
   9
   "This is a very very very very very very very long string"
   "
/**
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
  "Auto-fill does not respect `fill-paragraph-function' and will thus not be able to use our custom prefix detection.  Therefore this won't work..."
  :expected-result :failed
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
