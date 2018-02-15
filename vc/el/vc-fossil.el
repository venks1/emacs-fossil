;;; vc-fossil.el --- VC backend for the fossil sofware configuraiton management system

;; Author: Venkat Iyer <venkat@comit.com>

;; vc-fossil.el free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; vc-fossil.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a copy of the license, please see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This file contains a VC backend for the fossil version control
;; system.

;;; Installation:

;; 1. Install this vc-fossil package.
;; 2. Add "Fossil" to the list of VC backends using
;;    M-x customize-variable vc-handled-backends

;; Alternative manual installation
;; 1. Put this file somewhere in the Emacs load-path.
;; 2. Tell Emacs to load it when needed:
;;    (autoload 'vc-fossil-registered "vc-fossil")
;; 3. Add Fossil to the list of supported backends:
;;    (add-to-list 'vc-handled-backends 'Fossil t)

;;; Implemented Functions
;; BACKEND PROPERTIES
;; * revision-granularity
;; STATE-QUERYING FUNCTIONS
;; * registered (file)
;; * state (file) - 'up-to-date 'edited 'needs-patch 'needs-merge
;; * dir-status-files (dir files uf)
;; * workfile-version (file)
;; * checkout-model (file)
;; - workfile-unchanged-p (file)
;; - root (file)
;; STATE-CHANGING FUNCTIONS
;; * register (file &optional rev comment)
;; * checkin (file comment &optional rev)
;; * find-version (file rev buffer)
;; * checkout (file &optional editable rev)
;; * revert (file &optional contents-done)
;; * pull (prompt)
;; - push (prompt)
;; - responsible-p (file)
;; HISTORY FUNCTIONS
;; * print-log (file &optional buffer)
;; * diff (file &optional rev1 rev2 buffer async)
;; MISCELLANEOUS
;; - delete-file (file)
;; - rename-file (old new)

;;; Code:

(eval-when-compile (require 'vc))

(autoload 'vc-switches "vc")

;;; Customization

(defgroup vc-fossil nil
  "VC Fossil backend."
  :group 'vc)

(defcustom vc-fossil-diff-switches t ; Fossil doesn't support common args like -u
  "String or list of strings specifying switches for Fossil diff under VC.
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
                 (const :tag "None" t)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string))
  :group 'vc-fossil)

(defcustom vc-fossil-extra-header-fields (list :checkout :tags)
  "A list of keywords denoting extra header fields to show in the vc-dir buffer."
  :type '(set (const :repository) (const :remote-url) (const :synchro)
              (const :checkout) (const :comment) (const :tags))
  :group 'vc-fossil)


;;; BACKEND PROPERTIES

(defvar vc-fossil-history nil)

(defvar vc-fossil-pull-history nil)
(defvar vc-fossil-push-history nil)

(defun vc-fossil-revision-granularity () 'repository)


;; Internal Commands

(defun vc-fossil--call (buffer &rest args)
  (apply #'process-file "fossil" nil buffer nil args))

(defun vc-fossil--out-ok (&rest args)
  (zerop (apply #'vc-fossil--call '(t nil) args)))

(defun vc-fossil--run (&rest args)
  "Run a fossil command and return its output as a string"
  (catch 'bail
    (with-output-to-string
      (with-current-buffer standard-output
        (unless (apply #'vc-fossil--out-ok args)
          (throw 'bail nil))))))

(defun vc-fossil--command (buffer okstatus file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-fossil.el.
  The difference to vc-do-command is that this function always invokes `fossil'."
  (apply #'vc-do-command (or buffer "*vc*") okstatus "fossil" file-or-list flags)
  (when (eql major-mode 'vc-dir-mode)  ; update header info
    (revert-buffer (current-buffer))))

(defun vc-fossil--get-id (dir)
  (let* ((default-directory dir)
         (info (vc-fossil--run "info"))
         (pos (string-match "checkout: *\\([0-9a-fA-F]+\\)" info))
         (uid (match-string 1 info))
         )
    (substring uid 0 10)))

(defun vc-fossil--get-repository (dir)
  (let* ((default-directory dir)
         (info (vc-fossil--run "info")))
    (string-match "repository: *\\(.*\\)$" info)
    (match-string 1 info)))

(defun vc-fossil--do-async-prompted-command (command &optional prompt hist-var)
  "Run a fossil command asynchronously.
Allow user to edit command in minibuffer if PROMPT is non-nil."
  (let* ((root (vc-fossil-root default-directory))
         (buffer (format "*vc-fossil : %s*" (expand-file-name root)))
         (fossil-program "fossil")
         (args '()))
    (when prompt
      (setq args (split-string
                  (read-shell-command "Run Fossil (like this): "
                                      (concat fossil-program " " command)
                                      (or hist-var 'vc-fossil-history))
                  " " t))
      (setq fossil-program (car args)
            command (cadr args)
            args (cddr args)))
    (apply 'vc-do-async-command buffer root fossil-program command args)
    (with-current-buffer buffer
      (vc-run-delayed (vc-compilation-mode 'Fossil)))
    (vc-set-async-update buffer)))

;;; STATE-QUERYING FUNCTIONS

;;;###autoload
(defun vc-fossil-registered (file)
  "Check whether FILE is registered with fossil."
  (with-temp-buffer
    (let* ((str (ignore-errors
                  (vc-fossil--out-ok "finfo" "-s" (file-truename file))
                  (buffer-string))))
      (and str
           (> (length str) 7)
           (not (string= (substring str 0 7) "unknown"))))))

(defun vc-fossil-state-code (code)
  (cond ((not code)                 'unregistered)
        ((string= code "UNKNOWN")   'unregistered)
        ((string= code "UNCHANGED") 'up-to-date)
        ((string= code "CONFLICT")  'edited)
        ((string= code "ADDED")     'added)
        ((string= code "ADD")       'needs-update)
        ((string= code "EDITED")    'edited)
        ((string= code "REMOVE")    'removed)
        ((string= code "UPDATE")    'needs-update)
        ((string= code "MERGE")     'needs-merge)
        ((string= code "EXTRA")     'unregistered)
        ((string= code "MISSING")   'missing)
        ((string= code "RENAMED")   'added)
        (t           nil)))

(defun vc-fossil-state  (file)
  "Fossil specific version of `vc-state'."
  (let* ((line (vc-fossil--run "update" "-n" "-v" "current" (file-truename file)))
         (state (vc-fossil-state-code (car (split-string line)))))
    ;; if 'fossil update' says file is UNCHANGED check to see if it has been RENAMED
    (when (or (not state) (eql state 'up-to-date))
      (let ((line (vc-fossil--run "changes" "--classify" "--unchanged" "--renamed"
                                  (file-truename file))))
        (setq state (and line (vc-fossil-state-code (car (split-string line)))))))
    state))

(defun vc-fossil-working-revision (file)
  "Fossil Specific version of `vc-working-revision'."
  (let ((line (vc-fossil--run "finfo" "-s" (file-truename file))))
    (and line
         (cadr (split-string line)))))

(defun vc-fossil-workfile-unchanged-p (file)
  (eq 'up-to-date (vc-fossil-state file)))

(defun vc-fossil-root (file)
  (or (vc-find-root file ".fslckout")
      (vc-find-root file "_FOSSIL_")))

;; TODO: mode-line-string
;; TODO: dir-printer

(defun vc-fossil-dir-status (dir update-function)
  "Get fossil status for all files in a directory"
  (vc-fossil--dir-status-files dir nil update-function))

(defvar vc-fossil--file-classifications nil
  "An alist of (filename . classification) pairs.")

(defun vc-fossil--classify-all-files (dir)
  (setq vc-fossil--file-classifications nil)
  (let* ((default-directory dir)
         (lines (split-string (vc-fossil--run "changes" "--classify" "--all") "[\n\r]+" t)))
    (dolist (line lines)
      (string-match "^\\(\\w+\\)\\s-+\\(.+\\)$" line)
      (let ((pair (cons (match-string 2 line) (match-string 1 line))))
        (push pair vc-fossil--file-classifications)))))

(defun vc-fossil--dir-status-files (dir files update-function)
  "Get fossil status for all specified files in a directory.
If `files` is nil return the status for all files."
  (vc-fossil--classify-all-files dir)
  (insert (apply 'vc-fossil--run "update" "-n" "-v" "current"
                 (or files (list dir))))
  (let ((result '())
        (root (vc-fossil-root dir)))
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
             (status-word (car (split-string line))))
        (if (string-match "-----" status-word)
            (goto-char (point-max))
          (let ((file (substring line (+ (length status-word) 1)))
                (state (vc-fossil-state-code status-word)))
            (setq file (expand-file-name file root))
            (setq file (file-relative-name file dir))
            ;; if 'fossil update' says file is UNCHANGED check to see if it has been RENAMED
            (when (or (not state) (eql state 'up-to-date))
              (setq state (vc-fossil-state-code (cdr (assoc file vc-fossil--file-classifications)))))
            (push (list file state) result)))
        (forward-line)))
    ;; now collect untracked files
    (delete-region (point-min) (point-max))
    (insert (apply 'vc-fossil--run "extras" "--dotfiles" (or files (list dir))))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((file (buffer-substring-no-properties (point) (line-end-position))))
        (setq file (expand-file-name file dir))
        (setq file (file-relative-name file dir))
        (push (list file (vc-fossil-state-code nil)) result)
        (forward-line)))
    (funcall update-function result nil)))

(if (>= emacs-major-version 25)
    (defun vc-fossil-dir-status-files (dir files update-function)
      (vc-fossil--dir-status-files dir files update-function))
  (defun vc-fossil-dir-status-files (dir files default-state update-function)
    (vc-fossil--dir-status-files dir files update-function)))

(defun vc-fossil-checkout-model (files) 'implicit)

(defun vc-fossil--propertize-header-line (name value)
  (concat (propertize name  'face 'font-lock-type-face)
          (propertize value 'face 'font-lock-variable-name-face)))

(defun vc-fossil-dir-extra-headers (dir)
  (let ((info (vc-fossil--run "info"))
        (settings (vc-fossil--run "settings"))
        (lines nil))
    (dolist (field vc-fossil-extra-header-fields)
      (unless (null lines)
        (push "\n" lines))
      (cond ((eql field :repository)
             (string-match "repository: *\\(.*\\)$" info)
             (let ((repo (match-string 1 info)))
               (push (vc-fossil--propertize-header-line "Repository : " repo) lines)))
            ((eql field :remote-url)
             (let ((remote-url (car (split-string (vc-fossil--run "remote-url")))))
               (push (vc-fossil--propertize-header-line "Remote URL : " remote-url) lines)))
            ((eql field :synchro)
             (let* ((as-match (string-match "^autosync +.+ +\\([[:graph:]]+\\)$" settings))
                    (autosync (and as-match (match-string 1 settings)))
                    (dp-match (string-match "^dont-push +.+ +\\([[:graph:]]+\\)$" settings))
                    (dontpush (and dp-match (match-string 1 settings))))
               (push (vc-fossil--propertize-header-line "Synchro    : "
                                                        (concat (and autosync "autosync=") autosync
                                                                (and dontpush " dont-push=") dontpush))
                     lines)))
            ((eql field :checkout)
             (let* ((posco (string-match "checkout: *\\([0-9a-fA-F]+\\) \\([-0-9: ]+ UTC\\)" info))
                    (coid (substring (match-string 1 info) 0 10))
                    (cots (format-time-string "%Y-%m-%d %H:%M:%S %Z"
                                              (safe-date-to-time (match-string 2 info))))
                    (child-match (string-match "child: *\\(.*\\)$" info))
                    (leaf (if child-match "non-leaf" "leaf")))
               (push (vc-fossil--propertize-header-line "Checkout   : "
                                                        (concat coid " " cots
                                                                (concat " (" leaf ")")))
                     lines)))
            ((eql field :comment)
             (string-match "comment: *\\(.*\\)$" info)
             (let ((msg (match-string 1 info)))
               (push (vc-fossil--propertize-header-line "Comment    : " msg) lines)))
            ((eql field :tags)
             (string-match "tags: *\\(.*\\)" info)
             (let ((tags (match-string 1 info)))
               (push (vc-fossil--propertize-header-line "Tags       : " tags) lines)))))
    (apply #'concat (nreverse lines))))

;;; STATE-CHANGING FUNCTIONS

(defun vc-fossil-create-repo ()
  "Create a new Fossil Repository."
  (vc-fossil--command nil 0 nil "new"))

;; We ignore the comment.  There's no comment on add.
(defun vc-fossil-register (files &optional rev comment)
  "Register FILE into the fossil version-control system."
  (vc-fossil--command nil 0 files "add"))

(defun vc-fossil-responsible-p (file)
  (vc-fossil-root file))

(defun vc-fossil-unregister (file)
  (vc-fossil--command nil 0 file "rm"))

(declare-function log-edit-extract-headers "log-edit" (headers string))

(defun vc-fossil--checkin (files comment &optional rev)
  (apply 'vc-fossil--command nil 0 files
         (nconc (list "commit" "-m")
                (log-edit-extract-headers
                 `(("Author" . "--user-override")
                   ("Date" . "--date-override"))
                 comment)
                (vc-switches 'Fossil 'checkin))))

(if (>= emacs-major-version 25)
    (defun vc-fossil-checkin (files comment &optional rev)
      (vc-fossil--checkin files comment rev))
  (defun vc-fossil-checkin (files rev comment)
    (vc-fossil--checkin files comment rev)))

(defun vc-fossil-find-revision (file rev buffer)
  (apply #'vc-fossil--command buffer 0 file
         "cat"
         (nconc
          (unless (zerop (length rev)) (list "-r" rev))
          (vc-switches 'Fossil 'checkout))))

(defun vc-fossil-checkout (file &optional editable rev)
  (apply #'vc-fossil--command nil 0 file
         "update"
         (nconc
          (cond
           ((eq rev t) (list "current"))
           ((equal rev "") (list "trunk"))
           ((stringp rev) (list rev)))
          (vc-switches 'Fossil 'checkout))))

(defun vc-fossil-revert (file &optional contents-done)
  "Revert FILE to the version stored in the fossil repository."
  (if contents-done t
    (vc-fossil--command nil 0 file "revert")))

(defun vc-fossil-pull (prompt)
  "Pull upstream changes into the current branch.

With a prefix argument, or if PROMPT is non-nil, prompt for a specific
Fossil pull command.  The default is \"fossil update\"."
  (interactive "P")
  (vc-fossil--do-async-prompted-command "update" prompt 'vc-fossil-pull-history))

(defun vc-fossil-push (prompt)
  "Push changes to upstream repository.

With a prefix argument or if PROMPT is non-nil, prompt for a specific
Fossil push command.  The default is \"fossil push\"."
  (interactive "P")
  (vc-fossil--do-async-prompted-command "push" prompt 'vc-fossil-push-history))

;; HISTORY FUNCTIONS

;; FIXME, we actually already have short, start and limit, need to
;; add it into the code

(autoload 'vc-setup-buffer "vc-dispatcher")

(defun vc-fossil-print-log (files buffer &optional shortlog start-revision limit)
  "Print full log for a file"
  (vc-setup-buffer buffer)
  (let ((inhibit-read-only t))
    (with-current-buffer buffer
      (dolist (file files)
        (apply #'vc-fossil--command buffer 0 nil "timeline"
               (nconc
                (when start-revision (list "before" start-revision))
                (when limit (list "-n" (number-to-string limit)))
                (list "-p" (file-relative-name (expand-file-name file)))))))))

(defvar log-view-message-re)
(defvar log-view-file-re)
(defvar log-view-font-lock-keywords)
(defvar log-view-per-file-logs)

(define-derived-mode vc-fossil-log-view-mode log-view-mode "Fossil-Log-View"
  (require 'add-log) ;; we need the add-log faces
  (setq word-wrap t)
  (set (make-local-variable 'wrap-prefix) "                      ")
  (set (make-local-variable 'log-view-file-re) "\\`a\\`")
  (set (make-local-variable 'log-view-per-file-logs) nil)
  (set (make-local-variable 'log-view-message-re)
       "^[0-9:]+ \\[\\([0-9a-fA-F]*\\)\\] \\(?:\\*[^*]*\\*\\)? ?\\(.*\\)")
  (set (make-local-variable 'log-view-font-lock-keywords)
       (append
        '(
          ("^\\([0-9:]*\\) \\(\\[[[:alnum:]]*\\]\\) \\(\\(?:\\*[[:word:]]*\\*\\)?\\) ?\\(.*?\\) (user: \\([[:word:]]*\\) tags: \\(.*\\))"
           (1 'change-log-date)
           (2 'change-log-name)
           (3 'highlight)
           (4 'log-view-message)
           (5 'change-log-name)
           (6 'highlight))
          ("^=== \\(.*\\) ==="
           (1 'change-log-date))))))

;; TODO: implement diff for directories
(defun vc-fossil-diff (files &optional rev1 rev2 buffer async)
  "Get Differences for a file"
  (let ((buf (or buffer "*vc-diff*"))
        (root (and files (expand-file-name (vc-fossil-root (car files))))))
    ;; if we diff the root directory, do not specify a file
    (if (or (null files)
            (and (null (cdr files))
                 (equal root (expand-file-name (car files)))))
        (setq files nil))
    (apply #'vc-fossil--command
           buf 0 files "diff" "-i"
           (nconc
            (cond
             (rev2 (list "--from" (or rev1 "current") "--to" rev2))
             (rev1 (list "--from" rev1)))
            (vc-switches 'Fossil 'diff)))))

(declare-function vc-annotate-convert-time "vc-annotate" (time))

(defun vc-fossil-annotate-command (file buffer &optional rev)
  "Execute \"fossil annotate\" on FILE, inserting the contents in BUFFER.
If REV is specified, annotate that revision."
  ;;(assert (not rev) nil "Annotating a revision not supported")
  (vc-fossil--command buffer 0 file "annotate"))

(defconst vc-fossil-annotate-re
  "\\([[:word:]]+\\)\\s-+\\([-0-9]+\\)\\s-+[0-9]+: ")

;; TODO: currently only the date is used, not the time
(defun vc-fossil-annotate-time ()
  (when (looking-at vc-fossil-annotate-re)
    (goto-char (match-end 0))
    (vc-annotate-convert-time
     (date-to-time (format "%s 00:00:00" (match-string-no-properties 2))))))

(defun vc-fossil-annotate-extract-revision-at-line ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at vc-fossil-annotate-re)
      (goto-char (match-end 0))
      (match-string-no-properties 1))))

;;; TAG SYSTEM

;; FIXME: we need a convenience function to check that there's nothing checked
;; out in the tree, since we tag or branch the whole repository

(defun vc-fossil-create-tag (file name branchp)
  (let* ((dir (if (file-directory-p file) file (file-name-directory file)))
         (default-directory dir))
    (apply #'vc-fossil--command nil 0 nil `(,@(if branchp
                                                 '("branch" "new")
                                               '("tag" "add"))
                                           ,name ,(vc-fossil--get-id dir)))))

;; FIXME: we should update buffers if update is non-nill

(defun vc-fossil-retrieve-tag (dir name update)
  (let ((default-directory dir))
    (vc-fossil--command nil 0 nil "checkout" name)))

;;; MISCELLANEOUS

(defun vc-fossil-previous-revision (file rev)
  "Fossil specific version of the `vc-previous-revision'."
  (with-temp-buffer
    (cond
     (file
      (vc-fossil--command t 0 (file-truename file) "finfo" "-l" "-b")
      (goto-char (point-min))
      (and (re-search-forward (concat "^" (regexp-quote rev)) nil t)
           (zerop (forward-line))
           (looking-at "^\\([0-9a-zA-Z]+\\)")
           (match-string 1)))
     (t
      (vc-fossil--command t 0 nil "info" rev)
      (goto-char (point-min))
      (and (re-search-forward "parent: *\\([0-9a-fA-F]+\\)" nil t)
           (match-string 1))))))

(defun vc-fossil-next-revision (file rev)
  "Fossil specific version of the `vc-previous-revision'."
  (with-temp-buffer
    (cond
     (file
      (vc-fossil--command t 0 (file-truename file) "finfo" "-l" "-b")
      (goto-char (point-min))
      (and (re-search-forward (concat "^" (regexp-quote rev)) nil t)
           (zerop (forward-line -1))
           (looking-at "^\\([0-9a-zA-Z]+\\)")
           (match-string 1)))
     (t
      (vc-fossil--command t 0 nil "info" rev)
      (goto-char (point-min))
      (and (re-search-forward "child: *\\([0-9a-fA-F]+\\)" nil t)
           (match-string 1))))))

(defun vc-fossil-delete-file (file)
  (vc-fossil--command nil 0 (file-truename file) "rm" "--hard"))

(defun vc-fossil-rename-file (old new)
  (vc-fossil--command nil 0 (list (file-truename old) (file-truename new)) "mv" "--hard"))

(provide 'vc-fossil)

;;; vc-fossil.el ends here
