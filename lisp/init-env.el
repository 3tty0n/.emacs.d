;;; init-env.el --- Shell environment, server, terminal tweaks -*- lexical-binding: t; -*-

;; Importing PATH from a login+interactive shell costs ~0.4s, so the result is
;; cached and applied instantly; a background shell refreshes the cache once
;; Emacs is idle.

(eval-when-compile (require 'cl-lib))

(defconst my-shell-env-cache
  (expand-file-name ".cache/shell-env.eld" user-emacs-directory)
  "File caching the environment imported from the user's shell.")

(defconst my-shell-env-variables '("PATH" "MANPATH")
  "Environment variables imported from the user's shell.")

(defun my-shell-env--apply (alist)
  "Set the environment variables in ALIST and refresh `exec-path'."
  (pcase-dolist (`(,name . ,value) alist)
    (setenv name value)
    (when (equal name "PATH")
      (setq exec-path (append (split-string value path-separator t)
                              (list exec-directory))))))

(defun my-shell-env--save (alist)
  "Write ALIST to `my-shell-env-cache'."
  (make-directory (file-name-directory my-shell-env-cache) t)
  (with-temp-file my-shell-env-cache
    (prin1 alist (current-buffer))))

(defun my-shell-env--read ()
  "Return the cached environment alist, or nil."
  (when (file-readable-p my-shell-env-cache)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents my-shell-env-cache)
        (read (current-buffer))))))

(defun my-shell-env--parse (output)
  "Parse the marked OUTPUT of `my-shell-env-refresh' into an alist."
  (when (string-match "<<ENV>>\\(.*\\)<<ENV>>" output)
    (let ((values (split-string (match-string 1 output) "\0")))
      (cl-loop for name in my-shell-env-variables
               for value in values
               unless (string-empty-p value) collect (cons name value)))))

(defun my-shell-env-refresh ()
  "Re-read the shell environment in the background and update the cache."
  (interactive)
  (let ((buffer (generate-new-buffer " *shell-env*"))
        (script (concat "printf '<<ENV>>"
                        (mapconcat (lambda (_) "%s") my-shell-env-variables "\\0")
                        "<<ENV>>'"
                        (mapconcat (lambda (name) (format " \"$%s\"" name))
                                   my-shell-env-variables ""))))
    (make-process
     :name "shell-env" :buffer buffer :noquery t
     :command (list (or (getenv "SHELL") "/bin/sh") "-l" "-i" "-c" script)
     :sentinel
     (lambda (process _event)
       (when (eq (process-status process) 'exit)
         (let ((alist (with-current-buffer buffer
                        (my-shell-env--parse (buffer-string)))))
           (when (and alist (not (equal alist (my-shell-env--read))))
             (my-shell-env--save alist)
             (my-shell-env--apply alist))))
       (unless (process-live-p process)
         (kill-buffer buffer))))))

(defun my-shell-env-setup ()
  "Import the shell environment using the cache when available."
  (if-let* ((alist (my-shell-env--read)))
      (progn
        (my-shell-env--apply alist)
        (run-with-idle-timer 3 nil #'my-shell-env-refresh))
    ;; First run: pay the cost once and remember the result.
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)
    (my-shell-env--save
     (mapcar (lambda (name) (cons name (getenv name))) my-shell-env-variables))))

(when (memq window-system '(mac ns x))
  (add-hook 'after-init-hook #'my-shell-env-setup))

;; Keep exec-path-from-shell available for manual use.
(use-package exec-path-from-shell
  :ensure t
  :defer t)

(when window-system
  (add-hook 'emacs-startup-hook
            (lambda ()
              (require 'server)
              (unless (server-running-p)
                (server-start)))))

(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (keymap-global-set "<mouse-4>" #'scroll-down-line)
  (keymap-global-set "<mouse-5>" #'scroll-up-line))

(provide 'init-env)
;;; init-env.el ends here
