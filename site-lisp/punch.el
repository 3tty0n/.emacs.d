;;; punch.el --- punctuation switcher

;; Copyright (C) 2013  Yuto HAYAMIZU

;; Author: Yuto HAYAMIZU <y.hayamizu@gmail.com>
;; Keywords: input method

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defgroup punch nil
  "Punctuation switcher"
  :group 'punch
  :prefix "punch-")

(defun punch-lighter ()
  " Punch")

(defun punch-post-self-insert-hook-function ()
  (when punch-mode

;; debug part
;;    (ignore-errors
;;      (save-excursion
;;  (set-buffer (get-buffer-create "*insert-debug*"))
;;	(end-of-buffer)
;;	(insert (format "%S prefix:%S\n"
;;			last-command-event
;;			last-prefix-arg
;;			))))

    (let* ((numchar (cond ((null last-prefix-arg) 1)
			  ((and (integerp last-prefix-arg)
				(> last-prefix-arg 0))
			   last-prefix-arg)
			  ((and (listp last-prefix-arg)
				(integerp (car last-prefix-arg))
				(> (car last-prefix-arg) 0))
			   (car last-prefix-arg))
			  (t 0)))
	   (insert-begin-point (- (point) numchar))
	   (insert-end-point (point))
	   (new-char (cond ((= ?、 last-command-event) ?，)
			   ((= ?。 last-command-event) ?．)
			   (t nil))))

      (when (and (> numchar 0) (not (null new-char)))
	(save-excursion
	  (delete-region insert-begin-point insert-end-point)
	  (insert (make-string numchar new-char)))
	(goto-char insert-end-point)))
  ))

(define-minor-mode punch-mode
  "Punch mode."
  :global nil
  :lighter (:eval (punch-lighter))

  (if punch-mode
      (add-hook 'post-self-insert-hook
		'punch-post-self-insert-hook-function)
    (remove-hook 'post-self-insert-hook
		 'punch-post-self-insert-hook-function))

  punch-mode)

;; TODO: implement punctuation conversion with advice or something...

(provide 'punch)

;;; punch.el ends here
