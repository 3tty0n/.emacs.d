;;; my-lazy-package.el --- Install use-package dependencies on first use -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1") (use-package "2.4"))

;;; Commentary:

;; Prevent `use-package' declarations from contacting package archives during
;; startup.  Missing packages are remembered and installed after startup only
;; when their feature, file, or interactive autoload is actually requested.

;;; Code:

(require 'cl-lib)
(require 'package)
(require 'use-package)

(defvar my-lazy-package-missing-packages nil
  "Packages requested by `use-package' but not installed locally.")

(defvar my-lazy-package--aliases (make-hash-table :test #'eq)
  "Map feature and file names to package names.")

(defvar my-lazy-package--specs (make-hash-table :test #'eq)
  "Map package names to their original use-package ensure specifications.")

(defvar my-lazy-package--wrapped-autoloads (make-hash-table :test #'eq)
  "Map commands to pairs of installed wrappers and original autoloads.")

(defvar my-lazy-package--installing nil
  "Packages currently being installed on demand.")

(defvar my-lazy-package--attempted nil
  "Packages whose on-demand installation has already been attempted.")

(defvar my-lazy-package--startup-p (not after-init-time)
  "Non-nil until the initial Emacs startup has completed.")

(defvar my-lazy-package--saved-ensure-function nil
  "Value of `use-package-ensure-function' before this module was enabled.")

(defvar my-lazy-package--active-p nil
  "Non-nil while this module's hooks and advice are installed.")

(defun my-lazy-package--finish-startup ()
  "Permit package installation in response to subsequent user actions."
  (setq my-lazy-package--startup-p nil))

(defun my-lazy-package--normalize-spec (name ensure)
  "Return the package symbol represented by NAME and ENSURE."
  (cond
   ((eq ensure t) (use-package-as-symbol name))
   ((consp ensure) (car ensure))
   ((symbolp ensure) ensure)))

(defun my-lazy-package--remember (name ensure package)
  "Remember that NAME with ENSURE is provided by PACKAGE."
  (puthash (use-package-as-symbol name) package my-lazy-package--aliases)
  (puthash package package my-lazy-package--aliases)
  (puthash package (cons name ensure) my-lazy-package--specs))

(defun my-lazy-package-ensure (name args _state)
  "Implement a startup-safe `use-package-ensure-function'.
NAME and ARGS have the same meaning as in `use-package-ensure-elpa'."
  (let ((all-installed t))
    (dolist (ensure args all-installed)
      (let ((package (my-lazy-package--normalize-spec name ensure)))
        (when (and package (not (package-installed-p package)))
          (setq all-installed nil)
          (my-lazy-package--remember name ensure package)
          (cl-pushnew package my-lazy-package-missing-packages))))))

(defun my-lazy-package--for-key (key)
  "Return the missing package associated with feature or file KEY."
  (unless my-lazy-package--installing
    (gethash (if (symbolp key) key (intern key)) my-lazy-package--aliases)))

(defun my-lazy-package--for-file (file)
  "Return the missing package associated with FILE."
  (let* ((name (file-name-nondirectory (format "%s" file)))
         (base (file-name-sans-extension name)))
    (when (string-suffix-p ".el" base)
      (setq base (file-name-sans-extension base)))
    (my-lazy-package--for-key base)))

(defun my-lazy-package--install (package)
  "Install PACKAGE once after startup and report whether it is available."
  (when (and package
             (not my-lazy-package--startup-p)
             (not (package-installed-p package))
             (not (memq package my-lazy-package--attempted)))
    (push package my-lazy-package--attempted)
    (let* ((spec (gethash package my-lazy-package--specs))
           (name (or (car spec) package))
           (ensure (or (cdr spec) t))
           (my-lazy-package--installing
            (cons package my-lazy-package--installing)))
      ;; This is the sole path in this module that may contact an archive.
      (use-package-ensure-elpa name (list ensure) nil)))
  (when (and package (package-installed-p package))
    (setq my-lazy-package-missing-packages
          (delq package my-lazy-package-missing-packages))
    t))

(defun my-lazy-package--require (original feature &optional filename noerror)
  "Install a declared package when ORIGINAL cannot require FEATURE."
  (condition-case error-data
      (let ((result (funcall original feature filename noerror)))
        (if (or result (featurep feature))
            result
          (let ((package (my-lazy-package--for-key feature)))
            (if (and package (my-lazy-package--install package))
                (funcall original feature filename noerror)
              result))))
    (file-missing
     (let ((package (my-lazy-package--for-key feature)))
       (if (and package (my-lazy-package--install package))
           (funcall original feature filename noerror)
         (signal (car error-data) (cdr error-data)))))))

(defun my-lazy-package--load (original file &rest args)
  "Install a declared package when ORIGINAL cannot load FILE."
  (condition-case error-data
      (let ((result (apply original file args)))
        (if result
            result
          (let ((package (my-lazy-package--for-file file)))
            (if (and package (my-lazy-package--install package))
                (apply original file args)
              result))))
    (file-missing
     (let ((package (my-lazy-package--for-file file)))
       (if (and package (my-lazy-package--install package))
           (apply original file args)
         (signal (car error-data) (cdr error-data)))))))

(defun my-lazy-package--invoke (command args interactive-p)
  "Invoke COMMAND with ARGS, respecting INTERACTIVE-P."
  (if interactive-p
      (call-interactively command)
    (apply command args)))

(defun my-lazy-package--make-autoload-wrapper
    (command file package original-autoload)
  "Return a wrapper installing PACKAGE before COMMAND loads FILE."
  (lambda (&rest args)
    (interactive)
    (let ((interactive-p (called-interactively-p 'interactive)))
      (remhash command my-lazy-package--wrapped-autoloads)
      (fset command original-autoload)
      (cond
       ((my-lazy-package--install package)
        (my-lazy-package--invoke command args interactive-p))
       (my-lazy-package--startup-p
        (my-lazy-package--wrap-autoload
         command file package original-autoload)
        ;; Preserve the standard missing-file error during startup.
        (autoload-do-load original-autoload command nil))
       (t
        ;; Installation failed; retrying the original autoload preserves its
        ;; usual error instead of silently swallowing the command call.
        (my-lazy-package--invoke command args interactive-p))))))

(defun my-lazy-package--wrap-autoload
    (command file package original-autoload)
  "Replace COMMAND's ORIGINAL-AUTOLOAD with an installer for PACKAGE."
  (let ((wrapper (my-lazy-package--make-autoload-wrapper
                  command file package original-autoload)))
    (puthash command (cons wrapper original-autoload)
             my-lazy-package--wrapped-autoloads)
    (fset command wrapper)))

(defun my-lazy-package--autoload
    (original command file &optional docstring interactive type)
  "Wrap missing command autoloads created by ORIGINAL."
  (let ((result (funcall original command file docstring interactive type)))
    (when (and (symbolp command)
               interactive
               (autoloadp (symbol-function command)))
      (let ((package (my-lazy-package--for-file file)))
        (when package
          (my-lazy-package--wrap-autoload
           command file package (symbol-function command)))))
    result))

(defun my-lazy-package--restore-autoloads ()
  "Restore command autoloads still wrapped by this module."
  (maphash
   (lambda (command pair)
     (when (eq (symbol-function command) (car pair))
       (fset command (cdr pair))))
   my-lazy-package--wrapped-autoloads)
  (clrhash my-lazy-package--wrapped-autoloads))

(define-minor-mode my-lazy-package-mode
  "Install missing `use-package' dependencies only when first used."
  :global t
  :group 'package
  (if my-lazy-package-mode
      (unless my-lazy-package--active-p
        (setq my-lazy-package--startup-p (not after-init-time)
              my-lazy-package--saved-ensure-function
              use-package-ensure-function
              use-package-ensure-function #'my-lazy-package-ensure
              my-lazy-package--active-p t)
        (add-hook 'emacs-startup-hook #'my-lazy-package--finish-startup)
        (advice-add 'require :around #'my-lazy-package--require)
        (advice-add 'load :around #'my-lazy-package--load)
        (advice-add 'autoload :around #'my-lazy-package--autoload))
    (when my-lazy-package--active-p
      (remove-hook 'emacs-startup-hook #'my-lazy-package--finish-startup)
      (advice-remove 'require #'my-lazy-package--require)
      (advice-remove 'load #'my-lazy-package--load)
      (advice-remove 'autoload #'my-lazy-package--autoload)
      (my-lazy-package--restore-autoloads)
      (when (eq use-package-ensure-function #'my-lazy-package-ensure)
        (setq use-package-ensure-function
              my-lazy-package--saved-ensure-function))
      (setq my-lazy-package--active-p nil))))

(provide 'my-lazy-package)

;;; my-lazy-package.el ends here
