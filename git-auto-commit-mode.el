;;; git-auto-commit-mode.el --- Emacs Minor mode to automatically commit and push

;; Copyright (C) 2012,2013 Tom Willemsen <tom@ryuslash.org>

;; Author: Tom Willemsen <tom@ryuslash.org>
;; Created: Jan 9, 2012
;; Version: 4.2.2
;; Keywords: vc
;; URL: http://ryuslash.org/projects/git-auto-commit-mode/

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; git-auto-commit-mode is an Emacs minor mode that tries to commit
;; changes to a file after every save.

;; When `gac-automatically-push-p' is non-nil, it also tries to push
;; to the current upstream.

;;; Code:

(defgroup git-auto-commit-mode nil
  "Customization options for `git-auto-commit-mode'."
  :group 'external)

(defcustom gac-automatically-push-p nil
  "Control whether or not `git-auto-commit-mode' should also
  automatically push the changes committed after each save."
  :tag "Automatically push"
  :group 'git-auto-commit-mode
  :type 'boolean
  :risky t)
(make-variable-buffer-local 'gac-automatically-push-p)

(defun gac-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun gac-git-dir (filename)
  "Find repository directory for FILENAME, or return nil."
  (let ((tried-dir
         (replace-regexp-in-string
          "\n+$" "" (shell-command-to-string
                     (concat "cd " (file-name-directory filename) " ; " "git rev-parse --show-toplevel")))))
    (if (string-match "\\:" tried-dir)
        nil
      tried-dir)))

(defun gac-raw-branches (filename)
  "Raw git branches of FILENAME."
  (let* ((git-directory (gac-git-dir filename))
         (branches
          (if (gac-git-dir filename)
              (shell-command-to-string
               (concat "cd " git-directory " ; " "git branch")))))
    branches))

(defun gac-current-branch (filename)
  "Current git branch of FILENAME."
  (let ((gb-output (gac-raw-branches filename)))
    (when gb-output
      (with-temp-buffer
        (insert gb-output)
        (goto-char (point-min))
        (and (re-search-forward "^\\*\\s-+\\(.*\\)" nil t)
             (match-string 1))))))

(defun gac-checkout-branch-or-create (filename branch)
  "Switch to FILENAME's BRANCH creating it if neccesary."
  (let ((current-branch (gac-current-branch filename))
        (branch-list (gac-branch-list filename))
        (git-directory (gac-git-dir filename)))
    (when (not (string-match "^wip\\/.*" current-branch))
      (shell-command
       (concat "cd " git-directory
               " ; "
               "git checkout "
               (if (member branch branch-list)
                   ""
                 " -b ")
               branch)))))

(defun gac-to-wip-branch (filename)
  "Zzz FILENAME."
  (let ((current-branch (gac-current-branch filename))
        (git-directory (gac-git-dir filename)))
    (when (not (string-match "^wip\\/.*" current-branch))
      (gac-checkout-branch-or-create
       filename
       (concat "wip/" current-branch)))))

(defun gac-from-wip-brach (filename)
  "Zzz FILENAME.")

(defun gac-branch-list-clean (branches)
  "Remove junk from BRANCHES."
  (delete "*" (split-string branches )))

(defun gac-branch-list (filename)
  "List of git branches for FILENAME."
  (let ((raw-branches (gac-raw-branches filename)))
    (if raw-branches
        (gac-branch-list-clean raw-branches )
      nil)))

(defun gac-relative-file-name (filename)
  "Find the path to the FILENAME relative to the git directory."
  (let* ((git-dir
          (replace-regexp-in-string
           "\n+$" "" (shell-command-to-string
                      "git rev-parse --show-toplevel")))
         (relative-file-name
          (replace-regexp-in-string
           "^/" "" (replace-regexp-in-string
                    git-dir "" filename))))
    relative-file-name))

(defun gac-shell-command-in-dir (command dir)
  "zzz"
  (shell-command (concat "cd " dir " ; " command)))


(defun gac-password (proc string)
  "Ask the user for a password when necessary."
  (let (ask)
    (cond
     ((or
       (string-match "^Enter passphrase for key '\\\(.*\\\)': $" string)
       (string-match "^\\\(.*\\\)'s password:" string))
      (setq ask (format "Password for '%s': " (match-string 1 string))))
     ((string-match "^[pP]assword:" string)
      (setq ask "Password:")))

    (when ask
      (process-send-string proc (concat (read-passwd ask nil) "\n")))))

(defun gac-process-filter (proc string)
  "Checks if the process is asking for a password and asks the
user for one when it does."
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t))
      (gac-password proc string))))

(defun gac-process-sentinel (proc status)
  "Report the process' status change."
  (message "git %s" (substring status 0 -1)))

(defun gac-commit ()
  "Commit `buffer-file-name' to git"
  (let* ((filename (buffer-file-name))
         (relative-filename
          (gac-relative-file-name filename)))
    (shell-command
     (concat "git add " filename
             " && git commit -m '" relative-filename "'"))))

(defun gac-push ()
  "Push changes to the repository to the current upstream. This
doesn't check or ask for a remote, so the correct remote should
already have been set up."
  (let ((proc (start-process "git" "*git-auto-push*" "git" "push")))
    (set-process-sentinel proc 'gac-process-sentinel)
    (set-process-filter proc 'gac-process-filter)))

(defun gac-after-save-func ()
  "Commit the changes to the current file, and when
`gac-automatically-push-p' is not `nil', push."
  (gac-commit)
  (when gac-automatically-push-p
    (gac-push)))

;;;###autoload
(define-minor-mode git-auto-commit-mode
  "Automatically commit any changes made when saving with this
mode turned on and optionally push them too."
  :lighter " ga"
  (if git-auto-commit-mode
      (add-hook 'after-save-hook 'gac-after-save-func t t)
    (remove-hook 'after-save-hook 'gac-after-save-func t)))

(provide 'git-auto-commit-mode)

;;; git-auto-commit-mode.el ends here
