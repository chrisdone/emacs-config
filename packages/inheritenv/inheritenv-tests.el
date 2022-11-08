;;; inheritenv-test.el --- Tests for inheritenv.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ert)
(require 'inheritenv)

(defun inheritenv-test--get-vars-in-temp-buffer (&rest vars)
  "Return a list of the values of the named environment VARS."
  (with-temp-buffer
    (mapcar 'getenv vars)))

(inheritenv-add-advice 'inheritenv-test--get-vars-in-temp-buffer)

(ert-deftest inheritenv-test-propagation ()
  (with-temp-buffer
    (setq-local process-environment '("FOO=BAR"))
    ;; Default behaviour
    (should
     (equal
      nil
      (with-temp-buffer
        (getenv "FOO"))))
    ;; With inheritenv
    (should
     (equal
      "BAR"
      (inheritenv (with-temp-buffer
                    (getenv "FOO")))))
    (should
     (equal
      '("BAR")
      (inheritenv-test--get-vars-in-temp-buffer "FOO")))))



(provide 'inheritenv-test)
;;; inheritenv-test.el ends here
