;;; minitask.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Lukáš Hozda
;;
;; Author: Lukáš Hozda <luk.hozda@gmail.com>
;; Maintainer: Lukáš Hozda <luk.hozda@gmail.com>
;; Created: září 24, 2024
;; Modified: září 24, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/root/minitask
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  org-minitask provides quick task management for org-mode.
;;  It allows adding tasks to specific sections and filing completed tasks.
;;
;;; Code:

(require 'org)

(defgroup org-minitask nil
  "Quick task management for org-mode."
  :group 'org
  :prefix "org-minitask-")

(defcustom org-minitask-file ""
  "File name of the main Org file."
  :type 'file
  :group 'org-minitask)

(defcustom org-minitask-todo-heading '("TASKS" "OUTSTANDING")
  "Heading path for new TODOs."
  :type '(repeat string)
  :group 'org-minitask)

(defcustom org-minitask-done-file nil
  "File for completed TODOs. If nil, use `org-minitask-file'."
  :type '(choice file (const nil))
  :group 'org-minitask)

(defcustom org-minitask-done-heading nil
  "Heading for completed TODOs. If nil, use `org-minitask-todo-heading'."
  :type '(choice (repeat string) (const nil))
  :group 'org-minitask)

(defcustom org-minitask-use-week-subheadings t
  "Whether to use week subheadings for completed tasks."
  :type 'boolean
  :group 'org-minitask)

(defcustom org-minitask-sunday-starts-week nil
  "Whether Sunday starts a week."
  :type 'boolean
  :group 'org-minitask)

(defun org-minitask--get-week-string ()
  "Get the current week string."
  (let* ((time (current-time))
         (week-start (if org-minitask-sunday-starts-week 0 1))
         (week-end (if org-minitask-sunday-starts-week 6 7))
         (start-date (time-add time (days-to-time (- week-start (nth 6 (decode-time time))))))
         (end-date (time-add start-date (days-to-time 6))))
    (format "%s to %s"
            (format-time-string "%Y-%m-%d" start-date)
            (format-time-string "%Y-%m-%d" end-date))))

(defun org-minitask--ensure-heading (heading-path)
  "Ensure HEADING-PATH exists, creating it if necessary."
  (org-with-point-at 1
    (dolist (heading heading-path)
      (if (re-search-forward (format "^\\*+\\s-+%s\\s-*$" (regexp-quote heading)) nil t)
          (org-end-of-subtree)
        (progn
          (goto-char (point-max))
          (insert (format "\n* %s\n" heading))))
      (org-end-of-subtree))))

(defun org-minitask--file-task (task)
  "File TASK to the appropriate location."
  (with-current-buffer (find-file-noselect (or org-minitask-done-file org-minitask-file))
    (org-minitask--ensure-heading (or org-minitask-done-heading org-minitask-todo-heading))
    (when org-minitask-use-week-subheadings
      (let ((week-heading (org-minitask--get-week-string)))
        (org-minitask--ensure-heading (cons week-heading nil))))
    (goto-char (point-max))
    (insert task "\n")))

;;;###autoload
(defun org-minitask-create ()
  "Create a new task and file it."
  (interactive)
  (let ((task (read-string "Task: ")))
    (with-current-buffer (find-file-noselect org-minitask-file)
      (org-minitask--ensure-heading org-minitask-todo-heading)
      (goto-char (point-max))
      (insert (format "** TODO %s\n" task)))
    (message "Task added: %s" task)))

;;;###autoload
(defun org-minitask-finish-and-file ()
  "Mark current task as DONE and file it away."
  (interactive)
  (with-current-buffer (current-buffer)
    (org-back-to-heading t)
    (let ((task (buffer-substring-no-properties (point) (line-end-position))))
      (unless (string-match-p "^\\*+\\s-+DONE\\s-+" task)
        (org-todo 'done))
      (setq task (buffer-substring-no-properties (point) (line-end-position)))
      (org-minitask--file-task task)
      (org-cut-subtree))
    (message "Task completed and filed.")))


(provide 'org-minitask)
;;; org-minitask.el ends here
