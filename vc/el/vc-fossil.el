;;; vc-fossil.el --- VC backend for the fossil sofware configuraiton management system
;; Author: Venkat Iyer <venkat@comit.com>

;;; Commentary:

;; This file contains a VC backend for the fossil version control
;; system.

;;; Installation:

;; 1. Put this file somewhere in the emacs load-path.
;; 2. Tell Emacs to load it when needed:
;;    (autoload 'vc-fossil-registered "vc-fossil")
;; 3. Add Fossil to the list of supported backends:
;;    (add-to-list 'vc-handled-backends 'Fossil)

;;; Implemented Functions
;; BACKEND PROPERTIES
;; * revision-granularity
;; STATE-QUERYING FUNCTIONS
;; * registered (file)
;; * state (file) - 'up-to-date 'edited 'needs-patch 'needs-merge
;; * workfile-version (file)
;; * checkout-model (file)
;; - workfile-unchanged-p (file)
;; STATE-CHANGING FUNCTIONS
;; * register (file &optional rev comment)
;; * checkin (file rev comment)
;; * find-version (file rev buffer)
;; * checkout (file &optional editable rev)
;; * revert (file &optional contents-done)
;; - responsible-p (file)
;; HISTORY FUNCTIONS
;; * print-log (file &optional buffer)
;; * diff (file &optional rev1 rev2 buffer)
;; MISCELLANEOUS
;; - delete-file (file)
;; - rename-file (old new)

(eval-when-compile (require 'vc))

;;; BACKEND PROPERTIES

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

(defun vc-fossil-root (file)
  (vc-find-root file "_FOSSIL_"))

(defun vc-fossil-command (buffer okstatus file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-fossil.el.
  The difference to vc-do-command is that this function always invokes `fossil'."
  (apply #'vc-do-command (or buffer "*vc*") okstatus "fossil" file-or-list flags))

(defun vc-fossil-get-id (dir)
  (let* ((default-directory dir)
	 (info (vc-fossil--run "info"))
	 (pos (string-match "checkout: *\\([0-9a-fA-F]+\\)" info))
	 (uid (match-string 1 info))
	 )
    (substring uid 0 9)))

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
  (cond ((not code)		    'unregistered)
	((string= code "UNKNOWN")   'unregistered)
	((string= code "UNCHANGED") 'up-to-date)
	((string= code "CONFLICT")  'edited)
	((string= code "ADDED")     'added)
	((string= code "ADD")       'needs-update)
	((string= code "EDITED")    'edited)
	((string= code "REMOVE")    'removed)
	((string= code "UPDATE")    'needs-update)
	((string= code "MERGE")     'needs-merge)
	(t			    nil)))

;; (vc-fossil-state "/proj/fiesta/tools/fossil/emacs-fossil/vc/el/vc-fossil.el")

(defun vc-fossil-state  (file)
  "Fossil specific version of `vc-state'."
  ;; (message (format "vc-fossil-state on %s" file))
  (let ((line (vc-fossil--run "update" "-n" "-v" "current" file)))
    (and line
	 (vc-fossil-state-code (car (split-string line))))))

(defun vc-fossil-working-revision (file)
  "Fossil Specific version of `vc-working-revision'."
  (let ((line (vc-fossil--run "finfo" "-s" file)))
    (and line
	 (cadr (split-string line)))))

(defun vc-fossil-workfile-unchanged-p (file)
  (eq 'up-to-date (vc-fossil-state file)))

;; TODO: mode-line-string
;; TODO: dir-printer  / dir-extra-headers


(defun vc-fossil-dir-status (dir update-function)
  "Get Fossil status for all files in a directory"
  ;; (message dir)
  (insert (vc-fossil--run "update" "-n" "-v" "current" dir))
  (let ((result '())
	(done nil)
	(root (vc-fossil-root dir)))
    (goto-char (point-min))
    (while (and (not (eobp)) (not done))
      (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
	     (status-word (car (split-string line))))
	(setq done (string-match "-----" status-word))
	(unless done
	  (let ((file (substring line (+ (length status-word) 1))))
	    (let ((file (expand-file-name file root)))
	      (let ((file (file-relative-name file dir)))
		(setq result
		      (cons (list file (vc-fossil-state-code status-word))
			    result)))))))
      (forward-line))
    (funcall update-function result nil)))

(defun vc-fossil-after-dir-status (callback)
  "Function to call after the status process has finished"
  (message "after-dir-status called %s" (buffer-string))
  (let ((result '()))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties (point) (line-end-position)))
	    (status-word '()))
	(message line)
	(let* ((state (vc-fossil-state-code (car (split-string line))))
	       (file (expand-file-name (substring line (+ (length status-word) 1)))))
	  (setq result (cons (list file state) result))))
      (forward-line))
    (funcall callback result t)))

(defun vc-fossil-checkout-model (files) 'implicit)

(defun vc-fossil-dir-extra-headers (dir)
  (let* ((info (vc-fossil--run "info"))
	 (posco (string-match "checkout: *\\([0-9a-fA-F]+\\) \\([-0-9: ]+ UTC\\)" info))
	 (coid (substring (match-string 1 info) 0 9))
	 (cots (format-time-string "%Y-%m-%d %H:%M:%S %Z"
				   (safe-date-to-time (match-string 2 info))))
	 (postag (string-match "tags: *\\(.*\\)" info))
	 (tags (match-string 1 info))
	 )
    (concat
     (propertize "Checkout   : " 'face 'font-lock-type-face)
     (propertize (concat coid " " cots) 'face 'font-lock-variable-name-face)
     "\n"
     (propertize "Tags       : " 'face 'font-lock-type-face)
     (propertize tags 'face 'font-lock-variable-name-face))))

;;; STATE-CHANGING FUNCTIONS

(defun vc-fossil-create-repo ()
  "Create a new Fossil Repository."
  (vc-fossil-command nil 0 nil "new"))

;; We ignore the comment.  There's no comment on add.
(defun vc-fossil-register (files &optional rev comment)
  "Register FILE into the fossil version-control system."
  (vc-fossil-command nil 0 files "add"))

(defun vc-fossil-responsible-p (file)
  (vc-fossil-root file))

(defun vc-fossil-unregister (file)
  (vc-fossil-command nil 0 file "rm"))


(defun vc-fossil-checkin (files rev comment)
  (vc-fossil-command nil 0 files "commit" "-m" comment))

(defun vc-fossil-find-revision (file rev buffer)
  (apply #'vc-fossil-command buffer 0 file
	 "finfo" `(,@(if (string= rev "")
			 '()
		       `("-r" ,rev)) "-p")))

(defun vc-fossil-checkout (file &optional editable rev)
  (apply #'vc-fossil-command nil 0 nil
	 "update" `(,@(if (eq rev t) '() `(,rev)))))

(defun vc-fossil-revert (file &optional contents-done)
  "Revert FILE to the version stored in the fossil repository."
  (if contents-done t
    (vc-fossil-command nil 0 file "revert")))

;; HISTORY FUNCTIONS

;; FIXME, we actually already have short, start and limit, need to
;; add it into the code

(defun vc-fossil-print-log (files buffer &optional shortlog start-revision limit)
  "Print full log for a file"
  (when files
    (vc-fossil-command buffer 0 (car files) "finfo" "-l")
    (vc-fossil-print-log (cdr files) buffer)))

;; TBD: log-entry

(defun vc-fossil-diff (file &optional rev1 rev2 buffer)
  "Get Differences for a file"
  ;; (message (format "Get diffs between rev <%s> and <%s> for file <%s>" rev1 rev2 file))
  (let ((buf (or buffer "*vc-diff*")))
    (apply #'vc-fossil-command
	   buf 0 file "diff" "-i"
	   `(,@(if rev1 `("--from" ,rev1) '())
	     ,@(if rev2 `("--to" ,rev2) '())))))

;;; TAG SYSTEM

;; FIXME: we need a convenience function to check that there's nothing checked
;; out in the tree, since we tag or branch the whole repository

(defun vc-fossil-create-tag (file name branchp)
  (let* ((dir (if (file-directory-p file) file (file-name-directory file)))
	 (default-directory dir))
    (apply #'vc-fossil-command nil 0 nil `(,@(if branchp
						 '("branch" "new")
					       '("tag" "add"))
					   ,name ,(vc-fossil-get-id dir)))))

;; FIXME: we should update buffers if update is non-nill

(defun vc-fossil-retrieve-tag (dir name update)
  (let ((default-directory dir))
    (vc-fossil-command nil 0 nil "checkout" name)))

;;; MISCELLANEOUS

(defun vc-fossil-previous-revision (file rev)
  "Fossil specific version of the `vc-previous-revision'."
  (when file
    (with-temp-buffer
      (let ((found (not rev))
	    (newver nil))
	(insert (vc-fossil--run "finfo" "-l" "-b" file))
	;; (vc-fossil--call "fossil" "finfo" "-l" "-b" file)
	(goto-char (point-min))
	(while (not (eobp))
	  (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
		 (version (car (split-string line))))
	    ;; (message line)
	    (setq newver (or newver (and found version)))
	    (setq found  (string= version rev)))
	  (forward-line))
	newver))))

(defun vc-fossil-next-revision (file rev)
  "Fossil specific version of the `vc-previous-revision'."
  (when file
    (with-temp-buffer
      (let ((found (not rev))
	    (oldver nil))
	(insert (vc-fossil--run "finfo" "-l" "-b" file))
	;; (vc-fossil--call "fossil" "finfo" "-l" "-b" file)
	(goto-char (point-min))
	(while (not (eobp))
	  (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
		 (version (car (split-string line))))
	    (setq found  (string= version rev))
	    (setq oldver (or oldver found version)))
	  (forward-line))
	oldver))))


(defun vc-fossil-delete-file (file)
  (vc-fossil-command nil 0 file "rm"))

(defun vc-fossil-rename-file (old new)
  (vc-fossil-command nil 0 (list old new) "mv"))

(provide 'vc-fossil)
