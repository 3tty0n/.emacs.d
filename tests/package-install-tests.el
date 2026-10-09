;;; package-install-tests.el --- Automatic installation tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'bytecomp)
(load (expand-file-name "../build.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro my-package-test--with-installer (&rest body)
  "Run BODY with the real ensure function and an offline package installer."
  (declare (indent 0))
  `(let ((installed nil)
         (my-lazy-package--aliases (make-hash-table :test #'eq))
         (my-lazy-package--specs (make-hash-table :test #'eq))
         (my-lazy-package--wrapped-autoloads (make-hash-table :test #'eq))
         (my-lazy-package-missing-packages nil)
         (my-lazy-package--attempted nil)
         (package-archive-contents '((my-test-package . nil)))
         (use-package-always-ensure t)
         (use-package-ensure-function #'use-package-ensure-elpa))
     (my-lazy-package-mode -1)
     (cl-letf (((symbol-function 'package-installed-p)
                (lambda (name &rest _) (memq name installed)))
               ((symbol-function 'package-install)
                (lambda (name &rest _) (push name installed)))
               ((symbol-function 'package-refresh-contents)
                (lambda (&rest _) (ert-fail "Unexpected network access"))))
       ,@body)))

(ert-deftest my-package-install-default-ensure ()
  (my-package-test--with-installer
    (eval '(use-package my-test-package :defer t))
    (should (equal installed '(my-test-package)))
    ;; Already installed dependencies must not be reinstalled.
    (eval '(use-package my-test-package :defer t))
    (should (equal installed '(my-test-package)))))

(ert-deftest my-package-install-explicit-opt-out ()
  (my-package-test--with-installer
    (eval '(use-package my-test-local-package :ensure nil :defer t))
    (should-not installed)))

(ert-deftest my-package-install-alias ()
  (my-package-test--with-installer
    (eval '(use-package my-test-feature :ensure my-test-package :defer t))
    (should (equal installed '(my-test-package)))))

(ert-deftest my-package-install-compiled-declaration ()
  (my-package-test--with-installer
    (let (compiled)
      (unwind-protect
          (progn
            (my-lazy-package-mode 1)
            (setq my-lazy-package--startup-p t)
            (let ((byte-compile-current-file "my-test-config.el"))
              (setq compiled
                    (byte-compile
                     '(lambda ()
                        (use-package my-test-package :defer t)))))
            (should-not installed))
        (my-lazy-package-mode -1))
      ;; Like standard use-package, compiled code retains the ensure function
      ;; selected at expansion time, even if the global option changes later.
      (funcall compiled)
      (should-not installed)
      (should (gethash 'my-test-package my-lazy-package--specs)))))

(ert-deftest my-package-install-lazy-startup-offline ()
  (my-package-test--with-installer
    (unwind-protect
        (progn
          (my-lazy-package-mode 1)
          (setq my-lazy-package--startup-p t)
          (eval '(use-package my-test-package :defer t))
          (should-not installed)
          (should-not (my-lazy-package--install 'my-test-package))
          (should-not installed)
          (my-lazy-package--finish-startup)
          (should (my-lazy-package--install 'my-test-package))
          (should (equal installed '(my-test-package))))
      (my-lazy-package-mode -1))))

(ert-deftest my-package-install-lazy-compiled-startup-offline ()
  (my-package-test--with-installer
    (let ((my-lazy-package--aliases (make-hash-table :test #'eq))
          (my-lazy-package--specs (make-hash-table :test #'eq))
          (my-lazy-package-missing-packages nil)
          (my-lazy-package--attempted nil)
          compiled)
      (unwind-protect
          (progn
            (my-lazy-package-mode 1)
            (setq my-lazy-package--startup-p t)
            (let ((byte-compile-current-file "my-test-config.el"))
              (setq compiled
                    (byte-compile
                     '(lambda ()
                        (use-package my-test-package :defer t)))))
            ;; Simulate a fresh startup rather than inheriting build state.
            (clrhash my-lazy-package--aliases)
            (clrhash my-lazy-package--specs)
            (setq my-lazy-package-missing-packages nil)
            (funcall compiled)
            (should-not installed)
            (should (memq 'my-test-package my-lazy-package-missing-packages))
            (should-not (my-lazy-package--install 'my-test-package))
            (my-lazy-package--finish-startup)
            (should (my-lazy-package--install 'my-test-package))
            (should (equal installed '(my-test-package))))
        (my-lazy-package-mode -1)))))

(ert-deftest my-package-install-lazy-standard-autoload ()
  (my-package-test--with-installer
    (let ((file (make-temp-file "my-package-autoload-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert ";;; -*- lexical-binding: t -*-\n"
                      "(defun my-test-command (value) (interactive \"p\") value)\n"))
            (my-lazy-package-mode 1)
            (setq my-lazy-package--startup-p t)
            (eval '(use-package my-test-feature :ensure my-test-package :defer t))
            (autoload 'my-test-command "my-test-feature" nil t)
            (should-not installed)
            ;; Resolve the file only after the install path has run.  This
            ;; exercises the evaluator's real autoload, not a stub for load.
            (cl-letf (((symbol-function 'package-install)
                       (lambda (name &rest _)
                         (push name installed)
                         (autoload 'my-test-command file nil t))))
              (my-lazy-package--finish-startup)
              (should (= 42 (my-test-command 42)))
              (should (equal installed '(my-test-package)))
              (should (= 7 (my-test-command 7)))
              (should (equal installed '(my-test-package)))))
        (my-lazy-package-mode -1)
        (fmakunbound 'my-test-command)
        (delete-file file)))))

(ert-deftest my-package-install-lazy-interactive-autoload ()
  (my-package-test--with-installer
    (let ((file (make-temp-file "my-package-autoload-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert ";;; -*- lexical-binding: t -*-\n"
                      "(defun my-test-command (value) (interactive \"p\") value)\n"))
            (my-lazy-package-mode 1)
            (setq my-lazy-package--startup-p t)
            (eval '(use-package my-test-feature :ensure my-test-package :defer t))
            ;; A preexisting standard autoload must keep its interactive spec.
            (autoload 'my-test-command file nil t)
            (my-lazy-package--wrap-autoload
             'my-test-command file 'my-test-package
             (symbol-function 'my-test-command))
            (my-lazy-package--finish-startup)
            (let ((current-prefix-arg 4))
              (should (= 4 (call-interactively 'my-test-command))))
            (should (equal installed '(my-test-package))))
        (my-lazy-package-mode -1)
        (fmakunbound 'my-test-command)
        (delete-file file)))))

;;; package-install-tests.el ends here
