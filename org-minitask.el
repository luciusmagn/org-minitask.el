;;; org-minitask.el --- Quick task management for org-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Lukáš Hozda
;;
;; Author: Lukáš Hozda <luk.hozda@gmail.com>
;; Maintainer: Lukáš Hozda <luk.hozda@gmail.com>
;; Created: září 24, 2024
;; Modified: září 24, 2024
;; Version: 0.0.1
;; Keywords: convenience, files, outlines
;; Homepage: https://github.com/root/minitask
;; Package-Requires: ((emacs "26.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; org-minitask provides quick task management for org-mode.
;; It allows adding tasks to specific sections and filing completed tasks.
;;
;; Create TODOs from anywhere, then file TODOs from anywhere to a single
;; archive, optionally filing under week headings.
;;
;;; Code:

(require 'org)

(defgroup org-minitask nil
  "Quick task management for 'org-mode'."
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

;;; AAAAAAAAAA


;; I have borrowed this from timesheet.el
;; https://github.com/girzel/timesheet.el/blob/master/timesheet.el#L250
(defun org-minitask-find-or-create-olp (segments &optional sortedp)
  "Find the heading corresponding to outline path SEGMENTS.
Each element of SEGMENTS should be a string representing exact
header text.  Creates any headings that do not exist, and returns
a marker pointing to the final header.

When optional SORTEDP is non-nil, any newly-created heading
segments will be inserted in sorted order among their siblings,
if any.

The current value of `case-fold-search' will affect which path
segments are considered to be \"found\"."
  ;; First, assume it exists.
  (let ((found-marker (ignore-errors (org-find-olp segments t)))
        (sorter (when sortedp
                  (if (functionp sortedp) sortedp #'string<)))
        (crawl 0)
        partial segment)
    (save-excursion
      (if found-marker found-marker
        (while segments
          (setq found-marker
                (condition-case nil
                    (org-find-olp
                     (setq partial
                           (append partial
                                   (list (setq segment (pop segments)))))
                     t)
                  (error
                   ;; If there's no found-marker then we're inserting
                   ;; a top-level heading, so stick it at the end of
                   ;; the buffer.
                   (if found-marker
                       (progn
                         (goto-char found-marker)
                         (if (and sorter
                                  (org-goto-first-child))
                             (progn
                               (while (and (funcall
                                            sorter
                                            (nth 4 (org-heading-components))
                                            segment)
                                           (/= crawl (point)))
                                 (setq crawl (point))
                                 (org-forward-heading-same-level 1))
                               ;; Gross.
                               (org-insert-heading (when (= crawl (point))
                                                     '(4))))
                           (end-of-line)
                           (org-insert-subheading '(4))))
                     (goto-char (point-max))
                     (org-insert-heading nil nil 'top-level))
                   (insert segment)
                   (beginning-of-line)
                   (point-marker)))))
        found-marker))))

(defun org-minitask--find-or-create-heading (heading-path)
  "Find or create HEADING-PATH, return point at end of subtree."
  (message "Heading path: %S" heading-path)  ; Added message statement
  (org-minitask-find-or-create-olp heading-path))

;;;###autoload
(defun org-minitask-create ()
  "Create a new task and file it."
  (interactive)
  (let ((task (read-string "Task: ")))
    (with-current-buffer (find-file-noselect org-minitask-file)
      (save-excursion
        (goto-char (org-minitask--find-or-create-heading org-minitask-todo-heading))
        (org-insert-heading-respect-content)
        (insert "TODO " task)
        (org-do-demote)))
    (message "Task added: %s" task)))

;;; fuck me
(defun org-minitask--mark-done-if-not-already ()
  "Mark the current task as DONE if it's not already."
  (org-back-to-heading t)
  (unless (string-match-p "^\\*+\\s-+DONE\\s-+" (thing-at-point 'line t))
    (org-todo 'done)))

(defun org-minitask--cut-subtree ()
  "Cut the current subtree and store it in the kill ring."
  (org-back-to-heading t)
  (org-cut-subtree))

(defun org-minitask--open-target-file ()
  "Open the target file in the background and return the buffer."
  (let ((target-file (or org-minitask-done-file org-minitask-file)))
    (find-file-noselect target-file)))

(defun org-minitask--find-or-create-target-path (buffer)
  "Find or create the target heading path in BUFFER, including week if applicable."
  (with-current-buffer buffer
    (org-with-wide-buffer
     (goto-char (point-min))
     (let* ((target-heading (or org-minitask-done-heading org-minitask-todo-heading))
            (week-heading (when org-minitask-use-week-subheadings
                            (org-minitask--get-week-string)))
            (full-path (if week-heading
                           (append target-heading (list week-heading))
                         target-heading)))
       (org-minitask--find-or-create-heading full-path)))))

(defun org-minitask--paste-subtree-at-target (buffer)
  "Paste the previously cut subtree at the target position in BUFFER."
  (with-current-buffer buffer
    (org-with-wide-buffer
     (let* ((target-pos (org-minitask--find-or-create-target-path buffer))
            (target-level (progn
                            (goto-char target-pos)
                            (org-current-level))))
       (forward-line -1)
       (when (looking-at-p "^$") (delete-char 1))
       (forward-line 1)
       (org-end-of-subtree t nil)
       (when (looking-at-p "^$") (delete-char 1))
       (org-paste-subtree (+ 1 target-level))
       (org-back-to-heading t)
       (forward-line -1)
       (when (looking-at-p "^$") (delete-char 1))))))

;;;###autoload
(defun org-minitask-finish-and-file ()
  "Mark current task as DONE and file it away."
  (interactive)
  (save-excursion
    ;; Mark the task as done
    (org-minitask--mark-done-if-not-already)

    ;; Cut the subtree
    (org-minitask--cut-subtree)

    ;; Open the target file and paste the subtree
    (message "pain4")
    (let ((target-buffer (org-minitask--open-target-file)))
      (org-minitask--paste-subtree-at-target target-buffer)

      ;; Save the target buffer if it's not the current buffer
      (unless (eq target-buffer (current-buffer))
        (with-current-buffer target-buffer
          (save-buffer)))))

  (message "Task completed and filed."))

(provide 'org-minitask)
;;; org-minitask.el ends here
