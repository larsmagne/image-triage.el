;;; image-triage.el --- View and mark images quickly -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: images

;; image-triage is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; image-triage is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;;; Commentary:


;;; Code:

(require 'cl-lib)
(require 'find-func)
(require 'dired)

(defvar image-triage-file-name-regexps image-type-file-name-regexps
  "Alist of regexps for matching image file names.")

(defvar image-triage-layout 'grid
  "What layout to use when listing image files.
`grid' will give you a grid layout, and `list' will give you a
 more traditional dired-like layout.")

(defvar image-triage-thumbnail-height 80
  "The height (in pixels) of image thumbnails in list mode.")

(defvar image-triage-thumbnail-width 200
  "The width (in pixels) of image thumbnails in grid mode.")

(defvar image-triage-preview-height 800
  "The height (in pixels) of image previews.")

;; Buffer-local variables used to store various things.
(defvar image-triage--data)
(defvar image-triage--file-name)
(defvar image-triage--last-process)
(defvar image-triage--parent-buffer)
(defvar image-triage--marked-files)
(defvar image-triage--sort-mode)
(defvar image-triage--list-mode)
(defvar image-triage--precomputed-previews)
(defvar image-triage--layout)
(defvar image-triage--line-height)

(defvar image-triage--thumbnail-instance 0)
(defvar image-triage--preview-instance 0)

(defun image-triage-directory (directory &optional override-layout)
  "View all images under DIRECTORY.
Which layout (grid or list) is used is determined by the
`image-triage-layout' variable.

If OVERRIDE-LAYOUT (interactively, the prefix), use the non-default
layout."
  (interactive "DView directory: \nP")
  (let* ((regexp (mapconcat #'car image-triage-file-name-regexps "\\|"))
	 (files
	  (let ((case-fold-search t))
	    (seq-filter (lambda (file)
			  (string-match-p regexp file))
			;; `directory-files' doesn't respect
			;; `case-fold-search', so we do it the hard
			;; way.
			(directory-files directory t)))))
    (unless files
      (user-error "No images in %s to display" directory))
    (image-triage--setup directory files override-layout)
    (setq-local image-triage--list-mode 'directory)))

(defun image-triage-tree (directory &optional override-layout)
  "View all images under DIRECTORY, recursively.
Which layout (grid or list) is used is determined by the
`image-triage-layout' variable.

If OVERRIDE-LAYOUT (interactively, the prefix), use the non-default
layout."
  (interactive "DView all images under directory: \nP")
  (let ((files (let ((case-fold-search t))
		 (directory-files-recursively
		  (expand-file-name directory)
		  (mapconcat #'car image-triage-file-name-regexps "\\|")))))
    (unless files
      (user-error "No images under %s to display" directory))
    (image-triage--setup directory files override-layout)
    (setq-local image-triage--list-mode 'tree)))

(defun image-triage--setup (directory files override-layout)
  (setq directory (expand-file-name directory))
  (switch-to-buffer (image-triage--create-buffer directory))
  (let ((inhibit-read-only t)
	(layout image-triage-layout)
	(current (and (eq major-mode 'image-triage-list-mode)
		      (image-triage--file-name)))
	(point (point)))
    ;; Allow the user to flip between the two layouts.
    (when override-layout
      (setq layout (if (eq layout 'grid) 'list 'grid)))
    (erase-buffer)
    ;; Avoid erasing buffer-local variables if we're just doing a
    ;; refresh of the buffer.
    (unless (eq major-mode 'image-triage-list-mode)
      (image-triage-list-mode))
    (setq-local image-triage--file-name directory
		image-triage--last-process nil
		image-triage--layout layout)
    ;; We default to sorting by newest first.
    (unless image-triage--sort-mode
      (setq image-triage--sort-mode 'time))
    (setq default-directory directory)
    (let ((sorted-files
	   (if (eq image-triage--sort-mode 'time)
	       (sort files (lambda (f1 f2)
			     ;; Sort newest first.
			     (time-less-p (file-attribute-modification-time
					   (file-attributes f2))
					  (file-attribute-modification-time
					   (file-attributes f1)))))
	     ;; Alphabeticalish.
	     (sort files #'string-version-lessp))))
      (if (eq layout 'grid)
	  (image-triage--insert-grid sorted-files)
	(image-triage--insert-list sorted-files)))
    ;; When refreshing, try to keep the same position.
    (or (and current
	     (image-triage--goto-file current))
	(goto-char point))
    (beginning-of-line)
    ;; Automatically update when we scroll.
    (add-hook 'window-scroll-functions #'image-triage--window-update
	      nil t)
    (image-triage--fill-thumbnails)))

(defun image-triage--insert-list (files)
  ;; We do this buffer the hard way -- by precomputing some defaults
  ;; and then using them throughout.  This is much faster than doing
  ;; things properly like `vtable' does, but speed is important here
  ;; -- we want this mode to work for directories/trees with tens of
  ;; thousands of images.
  (let* ((twidth (* image-triage-thumbnail-height 3))
	 (swidth (string-pixel-width
		  (propertize "9999G" 'face 'variable-pitch)))
	 (mwidth (string-pixel-width
		  (propertize "*" 'face 'variable-pitch)))
	 (dwidth (max (string-pixel-width
		       (propertize "WWW 30 2019" 'face 'variable-pitch))
		      (string-pixel-width
		       (propertize "99:99 2019" 'face 'variable-pitch))))
	 (new (- (time-convert nil 'integer)
		 (* 60 60 24 300)))
	 (seps
	  (list
	   (propertize " " 'display
		       `(space :align-to (,twidth)))
	   (propertize " " 'display
		       `(space :align-to (,(+ twidth mwidth 20))))
	   (propertize " " 'display
		       `(space :align-to (,(+ twidth mwidth 30 swidth))))
	   (propertize " " 'display
		       `(space :align-to (,(+ twidth mwidth 40 swidth dwidth)))))))
    (setq header-line-format
	  (propertize
	   (concat
	    "Thumb" (nth 0 seps)
	    "M" (nth 1 seps)
	    "Size" (nth 2 seps)
	    "Date" (nth 3 seps)
	    "File")
	   'face 'variable-pitch))
    (dolist (file files)
      (let ((atts (file-attributes file)))
	(insert (propertize " " 'image-triage--file file)
		(nth 0 seps)
		(propertize 
		 (if (member file image-triage--marked-files)
		     "*" " ")
		 'face 'variable-pitch 'toggle t)
		(nth 1 seps)
		(propertize
		 (file-size-human-readable (file-attribute-size atts))
		 'face 'variable-pitch)
		(nth 2 seps)
		(propertize
		 (if (> (time-convert (file-attribute-modification-time atts)
				      'integer)
			new)
		     (format-time-string
		      "%b %d %H:%M"
		      (file-attribute-modification-time atts))
		   (format-time-string
		    "%b %d %Y"
		    (file-attribute-modification-time atts)))
		 'face 'variable-pitch)
		(nth 3 seps)
		(propertize (file-name-nondirectory file) 'face 'variable-pitch)
		(propertize "\n" 'line-height
			    (+ image-triage-thumbnail-height 8)))))))

(defun image-triage--insert-grid (files)
  ;; In case we're switching between layouts, nix out the header line.
  (setq header-line-format nil)
  (let* ((columns (truncate
		  (1- (/ (frame-pixel-width) image-triage-thumbnail-width))))
	;; Directories often contain a bunch of images of the same
	;; size.  So using the size of the first image in the
	;; directory to guess at a reasonable image height seems, er,
	;; reasonable.
	(size (image-size (create-image (image-triage--preview (car files))
					nil t :scale 1)
			  t))
	(line-height (+ (truncate (* (/ (cdr size) (float (car size)))
				     image-triage-thumbnail-width))
			10))
	(column 0))
    ;; Save this for later use, too.
    (setq-local image-triage--line-height line-height)
    (dolist (file files)
      (insert (propertize
	       "*" 'image-triage--file file
	       'help-echo (image-triage--file-details file)
	       'display `(min-width ((,(+ image-triage-thumbnail-width 20))))))
      (if (= column columns)
	  (progn
	    (insert (propertize "\n" 'line-height line-height))
	    (setq column 0))
	(cl-incf column)))))

(defun image-triage--file-details (file)
  (let ((atts (file-attributes file)))
    (format "%s (%s, %s)"
	    (file-name-nondirectory file)
	    (file-size-human-readable (file-attribute-size atts))
	    (if (> (time-convert (file-attribute-modification-time atts)
				 'integer)
		   (- (time-convert nil 'integer)
		      (* 60 60 24 300)))
		(format-time-string
		 "%b %d %H:%M"
		 (file-attribute-modification-time atts))
	      (format-time-string
	       "%b %d %Y"
	       (file-attribute-modification-time atts))))))

(defun image-triage--goto-file (file)
  (let ((point (point)))
    (goto-char (point-min))
    (or (when-let ((match
		    (text-property-search-forward 'image-triage--file
						  file #'equal)))
	  (if (eq image-triage--layout 'list)
	      (beginning-of-line)
	    (goto-char (prop-match-beginning match)))
	  t)
	(progn
	  ;; Restore point.
	  (goto-char point)
	  nil))))

(defun image-triage--file-name ()
  "Return the name of the file on the current line."
  (if (eq image-triage--layout 'list)
      (get-text-property (pos-bol) 'image-triage--file)
    (or (get-text-property (point) 'image-triage--file)
	(save-excursion
	  (and (text-property-search-forward 'image-triage--file)
	       (get-text-property (point) 'image-triage--file))))))

(defvar-keymap image-triage-list-mode-map
  "RET" #'image-triage-display-image
  "SPC" #'image-triage-list-toggle-mark
  "TAB" #'image-triage-list-display-next
  "<backtab>" #'image-triage-list-display-previous
  "n" #'image-triage-list-display-next
  "p" #'image-triage-list-display-previous
  "m" #'image-triage-list-mark
  "U" #'image-triage-list-unmark-all
  "u" #'image-triage-list-unmark
  "g" #'image-triage-list-reload
  "v" #'image-triage-list-view-in-dired
  "C" #'image-triage-list-copy
  "D" #'image-triage-list-delete
  "&" #'image-triage-list-start
  "!" #'image-triage-list-execute
  "=" #'image-triage-list-details
  "s" #'image-triage-list-toggle-sort)

(define-derived-mode image-triage-list-mode special-mode "Image Triage"
  :interactive nil
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq-local image-triage--data (make-hash-table :test #'equal)
	      image-triage--sort-mode 'time
	      image-triage--marked-files nil))

(defun image-triage-list-toggle-sort ()
  "Toggle sorting by time/alphabet."
  (interactive nil image-triage-list-mode)
  (setq image-triage--sort-mode
	(if (eq image-triage--sort-mode 'time)
	    'alpha 'time))
  (image-triage-list-reload))

(defun image-triage-list-reload ()
  "Reload the current list of images."
  (interactive nil image-triage-list-mode)
  (if (eq image-triage--list-mode 'directory)
      (image-triage-directory
       image-triage--file-name
       (not (eq image-triage--layout image-triage-layout)))
    (image-triage-tree image-triage--file-name
		       (not (eq image-triage--layout image-triage-layout)))))

(defun image-triage-list-copy (files to-directory)
  "Copy the marked files somewhere.
If none are marked, copy the file under point."
  (interactive (let ((files (image-triage-marked-files)))
		 (list files
		       (cond
			((length= files 0)
			 (user-error "No file under point"))
			((length= files 1)
			 (read-file-name "Copy to: "))
			(t
			 (read-directory-name "Copy to: ")))))
	       image-triage-list-mode)
  (unless files
    (user-error "No file under point"))
  (if (and (length= files 1)
	   (not (file-directory-p to-directory)))
      ;; Copying a single file, and not to a directory.
      (cond
       ((or (not (file-exists-p to-directory))
	    (yes-or-no-p (format "Overwrite %s? " to-directory)))
	(copy-file (car files) to-directory t t)
	(message "Copied 1 file"))
       (t
	(message "Copy aborted")))
    ;; Copying lots of files.
    (let ((done 0))
      (map-y-or-n-p
       (lambda (file)
	 (let ((to (expand-file-name
		    (file-name-nondirectory file) to-directory)))
	   (if (file-exists-p to)
	       (format "Overwrite %s? " to)
	     t)))
       (lambda (file)
	 (copy-file file (expand-file-name (file-name-nondirectory file)
					   to-directory)
		    t t)
	 (cl-incf done))
       files)
      (message "Copied %s files" done))))

(defun image-triage-list-delete (files)
  "Delete the marked files.
If none are marked, delete the file under point."
  (interactive (list (image-triage-marked-files)) image-triage-list-mode)
  (unless files
    (user-error "No file under point"))
  (let ((done 0))
    (map-y-or-n-p
     (lambda (file)
       (format "Delete %s? " file))
     (lambda (file)
       (delete-file file)
       (image-triage--goto-file file)
       (let ((inhibit-read-only t))
	 (delete-region (pos-bol) (pos-bol 2)))
       (cl-incf done))
     files)
    (message "Deleted %s file%s" done (if (= done 1) "" "s"))))

(defun image-triage-list-details (file)
  "Show the details of the file under point."
  (interactive (list (image-triage--file-name)) image-triage-list-mode)
  (unless file
    (user-error "No file under point"))
  (save-excursion
    (image-triage--goto-file file)
    (message "%s" (get-text-property (point) 'help-echo))))

(defun image-triage-list-start (files command)
  "Open a command with the marked files asynchronously."
  (interactive (list (image-triage-marked-files)
		     (read-shell-command "Shell command: "))
	       image-triage-list-mode)
  (unless files
    (user-error "No file under point"))
  (let ((command (split-string-shell-command command)))
    (apply #'start-process (car command) nil (append command files))
    (message "Started %s..." (car command))))

(defun image-triage-list-execute (files command)
  "Run a command over the marked files."
  (interactive (list (image-triage-marked-files)
		     (read-shell-command "Shell command: "))
	       image-triage-list-mode)
  (unless files
    (user-error "No file under point"))
  (let ((command (split-string-shell-command command)))
    (with-temp-buffer
      (apply #'call-process (car command) nil (current-buffer) nil
	     (append (cdr command) files))
      (message "%s" (buffer-string)))))

(defun image-triage-list-view-in-dired (file)
  "View FILE in a dired buffer."
  (interactive (list (image-triage--file-name)) image-triage-list-mode)
  (unless file
    (user-error "No file under point"))
  (find-file (file-name-directory file))
  (dired-goto-file file))

(defun image-triage-list-toggle-mark ()
  "Toggle the mark on the image under point."
  (interactive nil image-triage-list-mode)
  (let ((file (image-triage--file-name)))
    (unless file
      (user-error "No file under point"))
    (if (member file image-triage--marked-files)
	(image-triage-list-unmark)
      (image-triage-list-mark))))

(defun image-triage-list-mark ()
  "Set the mark on the image under point."
  (interactive nil image-triage-list-mode)
  (let ((file (image-triage--file-name))
	(inhibit-read-only t))
    (unless file
      (user-error "No file under point"))
    (save-excursion
      (if (eq image-triage--layout 'list)
	  (progn
	    (beginning-of-line)
	    (text-property-search-forward 'toggle)
	    ;; We do it like this to preserve text properties.
	    (subst-char-in-region (point) (1- (point)) ?\s ?*))
	(put-text-property (point) (1+ (point))
			   'face `(:box (:line-width 1 :color "blue"))))
      (setq image-triage--marked-files (delete file image-triage--marked-files))
      (push file image-triage--marked-files)
      (message "Marked %s" file))))

(defun image-triage-list-unmark-all ()
  "Unset the mark on all images."
  (interactive nil image-triage-list-mode)
  (unless image-triage--marked-files
    (user-error "No files have been marked"))
  (let ((count 0))
    (save-excursion
      (dolist (file image-triage--marked-files)
	(image-triage--goto-file file)
	(image-triage-list-unmark t)
	(cl-incf count)))
    (message "Unmarked %s file%s" count (if (= count 1) "" "s"))))

(defun image-triage-list-display-next ()
  "Go to the next image and display it."
  (interactive)
  (image-triage--list-display-move 'next))

(defun image-triage-list-display-previous ()
  "Go to the previous image and display it."
  (interactive)
  (image-triage--list-display-move 'previous))

(defun image-triage--list-display-move (direction)
  (let ((next (if (eq direction 'next) (image-triage--next-file)
		(image-triage--previous-file)))
	(parent (current-buffer))
	(win (selected-window))
	(help-echo (get-text-property (point) 'help-echo)))
    (unless next
      (user-error (if (eq direction 'next) "No next file"
		    "No previous file")))
    (when help-echo
      (message "%s" help-echo))
    (let ((file (image-triage--file-name)))
      (unless (file-exists-p file)
	(user-error "%s no longer exists"))
      (pop-to-buffer (or (image-triage--display-window)
			 (image-triage--create-buffer file))
		     nil t)
      (unless (eq major-mode 'image-triage-display-mode)
	(image-triage-display-mode))
      (setq-local image-triage--file-name file
		  image-triage--parent-buffer parent)
      (image-triage--display-image file)
      (rename-buffer (generate-new-buffer-name
		      (concat (file-name-nondirectory
			       (directory-file-name file))
			      " Triage")))
      (select-window win))))

(defun image-triage--display-window ()
  "Return a buffer that displays an image that can be reused."
  (cl-loop for buffer in (buffer-list (selected-frame))
	   when (eq (buffer-local-value 'major-mode buffer)
		    'image-triage-display-mode)
	   return buffer))
    
(defun image-triage-marked-files ()
  "Return the list of marked files, or if none, the file under point."
  (or image-triage--marked-files
      (when-let ((file (image-triage--file-name)))
	(list file))))

(defun image-triage-list-unmark (&optional silent)
  "Unset the mark on the image under point."
  (interactive nil image-triage-list-mode)
  (let ((file (image-triage--file-name))
	(inhibit-read-only t))
    (unless file
      (user-error "No file under point"))
    (save-excursion
      (if (eq image-triage--layout 'list)
	  (progn
	    (beginning-of-line)
	    (text-property-search-forward 'toggle)
	    (subst-char-in-region (point) (1- (point)) ?* ?\s))
	(put-text-property (point) (1+ (point))
			   'face `(:box ( :line-width 1
					  :color ,(face-background 'default)))))
      (setq image-triage--marked-files
	    (delete file image-triage--marked-files))
      (unless silent
	(message "Removed mark from %s" file)))))

(defun image-triage-display-image (file)
  "Display the image under point."
  (interactive (list (image-triage--file-name)) image-triage-list-mode)
  (unless file
    (user-error "No file under point"))
  (unless (file-exists-p file)
    (user-error "File %s does not exist"))
  (let ((parent (current-buffer)))
    (switch-to-buffer (image-triage--create-buffer file))
    (unless (eq major-mode 'image-triage-display-mode)
      (image-triage-display-mode))
    (setq-local image-triage--file-name file
		image-triage--parent-buffer parent)
    (image-triage--display-image file)))

(defun image-triage-display-next ()
  "View the next image in the buffer."
  (interactive nil image-triage-display-mode)
  (image-triage--move 'next))

(defun image-triage-display-previous ()
  "View the previous image in the buffer."
  (interactive nil image-triage-display-mode)
  (image-triage--move 'previous))

(defun image-triage--next-file ()
  (if (eq image-triage--layout 'list)
      (and (forward-line 1)
	   (image-triage--file-name))
    (when-let ((match (text-property-search-forward 'image-triage--file
						    nil nil t)))
      (goto-char (prop-match-beginning match))
      (prop-match-value match))))

(defun image-triage--previous-file ()
  (if (eq image-triage--layout 'list)
      (and (zerop (forward-line -1))
	   (image-triage--file-name))
    (let ((value (get-text-property (point) 'image-triage--file)))
      (while (and (not (bobp))
		  (or (null (get-text-property (point) 'image-triage--file))
		      (equal value (get-text-property
				    (point) 'image-triage--file))))
	(forward-char -1))
      (and (not (equal value (get-text-property (point) 'image-triage--file)))
	   (get-text-property (point) 'image-triage--file)))))

(defun image-triage--move (direction)
  (let* ((parent image-triage--parent-buffer)
	 (file image-triage--file-name)
	 (window (get-buffer-window parent))
	 next-file new-point)
    (unless (buffer-live-p parent)
      (user-error "The parent buffer has been killed"))
    (save-window-excursion
      (switch-to-buffer parent t t)
      (image-triage--goto-file file)
      (let ((point (point)))
	(if (equal direction 'next)
	    (progn
	      (image-triage--next-file)
	      (setq next-file (image-triage--file-name)))
	  (unless (bobp)
	    (image-triage--previous-file)
	    (setq next-file (image-triage--file-name))))
	(unless next-file
	  (goto-char point)
	  (user-error "No next file"))
	(setq new-point (point))))
    (when window
      (message "%s" (point))
      (set-window-point window new-point))
    (image-triage--display-image next-file)
    ;; Update the buffer for the new file name.  When hitting `n' in a
    ;; view buffer, we don't want to create a whole bunch of buffers,
    ;; one per image, so we reuse a single buffer.
    (setq image-triage--file-name next-file
	  default-directory (file-name-directory next-file))
    (rename-buffer (generate-new-buffer-name
		    (concat (file-name-nondirectory (directory-file-name
						     next-file))
			    " Triage")))
    (message "Selected %s image" (if (eq direction 'next)
				     "next" "previous"))))

(defun image-triage-display-full ()
  "Display the full version of the current image.
(By default, this mode displays downscaled versions to speed
things up.)"
  (interactive nil image-triage-display-mode)
  (image-triage--display-image image-triage--file-name t)
  (message "Displaying full version of %s" image-triage--file-name))

(defun image-triage-display-toggle-mark ()
  "Toggle marking the current image in the buffer."
  (interactive nil image-triage-display-mode)
  (let ((parent image-triage--parent-buffer)
	(file image-triage--file-name))
    (unless (buffer-live-p parent)
      (user-error "The parent buffer has been killed"))
    ;; Do it this way so that point actually moves in the list buffer.
    (save-window-excursion
      (switch-to-buffer parent t t)
      (image-triage--goto-file file)
      (image-triage-list-toggle-mark))))

(defun image-triage-display-mark ()
  "Mark the current image in the parent buffer."
  (interactive nil image-triage-display-mode)
  (let ((parent image-triage--parent-buffer)
	(file image-triage--file-name))
    (unless (buffer-live-p parent)
      (user-error "The parent buffer has been killed"))
    (save-window-excursion
      (switch-to-buffer parent t t)
      (image-triage--goto-file file)
      (image-triage-list-mark))))

(defun image-triage-display-unmark ()
  "Unmark the current image in the parent buffer."
  (interactive nil image-triage-display-mode)
  (let ((parent image-triage--parent-buffer)
	(file image-triage--file-name))
    (unless (buffer-live-p parent)
      (user-error "The parent buffer has been killed"))
    (save-window-excursion
      (switch-to-buffer parent t t)
      (image-triage--goto-file file)
      (image-triage-list-unmark))))

(defun image-triage--display-image (file &optional inhibit-preview)
  (let ((inhibit-read-only t)
	(idata (and (not inhibit-preview)
		    (or (image-triage--precomputed file)
			(image-triage--preview file)))))
    (when (zerop (length idata))
      (setq idata nil))
    (erase-buffer)
    (insert (propertize
	     " " 'display
	     (create-image
	      (if idata idata file) nil idata
	      :width (truncate (* (window-pixel-width) 0.95))
	      :max-height (truncate (* (window-pixel-height) 0.95))
	      :scale 1)))
    (goto-char (point-min))
    (image-triage--preload file image-triage--parent-buffer)))

(defun image-triage--precomputed (file)
  ;; Prune the length of the previews.
  (when-let ((tail (nthcdr 20 image-triage--precomputed-previews)))
    (setcdr tail nil))
  (cdr (assoc file image-triage--precomputed-previews)))

(defun image-triage--preload (from-file parent)
  ;; We want to preload images that come after the current viewed
  ;; image, so that hitting `n' is fast.
  (when (buffer-live-p parent)
    (let* ((precomputed image-triage--precomputed-previews)
	   ;; Get the names of the next ten images (that we haven't
	   ;; already done).
	   (next-files
	    (with-current-buffer parent
	      (save-excursion
		(when (image-triage--goto-file from-file)
		  (cl-loop repeat 5
			   for file = (image-triage--file-name)
			   while file
			   unless (assoc file precomputed)
			   collect file
			   do (image-triage--next-file)))))))
      (when next-files
	(image-triage--preload-1 (current-buffer) next-files
				 (cl-incf image-triage--preview-instance))))))

(defun image-triage--preload-1 (buffer next-files instance)
  (let (next)
    (setq next
	  (lambda ()
	    (image-triage--fetch-thumbnail
	     (car next-files) buffer
	     (lambda ()
	       (pop next-files)
	       (when (and next-files
			  ;; Stop fetching if someone else has started.
			  (= instance image-triage--preview-instance))
		 (funcall next)))
	     nil t)))
    (funcall next)))

(defun image-triage--jpgpreview ()
  (expand-file-name
   "jpgpreview" (file-name-directory (find-library-name "image-triage.el"))))

(defun image-triage--preview (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (cond
     ((file-exists-p (image-triage--jpgpreview))
      (call-process (image-triage--jpgpreview) nil t nil "preview" file))
     (t
      (call-process "exiftool" nil t nil
		    "-json" "-a" "-G1" "-s"
		    "-preview:all" file)
      (goto-char (point-min))
      (let* ((json (elt (json-parse-buffer :object-type 'plist) 0))
	     (case-fold-search t)
	     (preview-name
	      (cl-loop for (slot _value) on json by #'cddr
		       when (string-match-p "preview" (symbol-name slot))
		       return (substring (symbol-name slot) 1))))
	(erase-buffer)
	(if preview-name
	    (call-process "exiftool" nil t nil
			  "-b" (concat "-" preview-name) file)
	  ;; No preview -- downscale ourselves.
	  (call-process "convert" nil t nil
			"-resize" "800x" file "jpg:-")))))
    (buffer-string)))

(defun image-triage--create-buffer (file)
  "If a buffer for FILE exists, return it.
Otherwise, create a new buffer."
  (or
   ;; Find an old image-triage buffer that displays FILE.
   (cl-loop for buffer in (buffer-list)
	    when (equal (with-current-buffer buffer
			  (and (boundp 'image-triage--file-name)
			       image-triage--file-name))
			file)
	    return buffer)
   ;; Create a new one.
   (get-buffer-create
    (generate-new-buffer-name
     (concat (file-name-nondirectory (directory-file-name file))
	     " Triage")))))

(defvar-keymap image-triage-display-mode-map
  "n" #'image-triage-display-next
  "p" #'image-triage-display-previous
  "g" #'image-triage-display-full
  "TAB" #'image-triage-display-next
  "<backtab>" #'image-triage-display-previous
  "SPC" #'image-triage-display-toggle-mark
  "m" #'image-triage-display-mark
  "u" #'image-triage-display-unmark)

(define-derived-mode image-triage-display-mode special-mode "Triage Display"
  :interactive nil
  (buffer-disable-undo)
  (setq cursor-type nil
	truncate-lines t
	image-triage--precomputed-previews nil))

(defun image-triage--window-update (_ _)
  ;; `window-end' isn't updated right away, apparently, so postpone
  ;; this slightly.
  (let ((buffer (current-buffer)))
    (run-at-time 0.01 nil
		 (lambda ()
		   (with-current-buffer buffer
		     (image-triage--fill-thumbnails))))))

(defun image-triage--fill-thumbnails ()
  (when (and (boundp 'image-triage--last-process)
	     (or (null image-triage--last-process)
		 ;; More than 200ms since the last time, so it's
		 ;; probably done.
		 (time-less-p
		  '(0 0 200000 0)
		  (time-since image-triage--last-process))))
    ;; We want to fill thumbnails on files that's displayed in the
    ;; window, and then the next window-ful, and then the previous
    ;; window-ful.
    (let ((buffer (current-buffer))
	  function instance)
      ;; This is used to reduce parallellism -- in case two processes
      ;; are fired off at the same time anyway.
      (setq instance (cl-incf image-triage--thumbnail-instance))
      (setq function
	    (lambda ()
	      (image-triage--fill-thumbnails-1 buffer function instance)))
      ;; We call the function this awkward way because we're passing
      ;; the function in to itself.  The function will load one
      ;; thumbnail (asynchronously), and then at the end, it will call
      ;; itself to fetch the next one, and so on.  Until we have a
      ;; sufficient number of thumbnails.
      (funcall function))))

(defun image-triage--fill-thumbnails-1 (buffer next instance)
  ;; Only do something if the buffer is visible.
  (when-let ((window (get-buffer-window buffer t)))
    (with-current-buffer buffer
      (let ((start (window-start window))
	    (end (window-end window))
	    (lines 0)
	    (done nil))
	(cl-flet ((check ()
		    (when-let ((file (image-triage--file-name)))
		      (unless (get-text-property (point) 'thumbnail)
			(let ((elem (gethash file image-triage--data)))
			  (if (plist-get elem 'thumbnail)
			      (progn
				(image-triage--insert-thumbnail
				 (plist-get elem 'thumbnail))
				(forward-char -1))
			    ;; If something else has fired off a new thumbnail
			    ;; fetcher, then we stop.
			    (when (= instance image-triage--thumbnail-instance)
			      (image-triage--fetch-thumbnail file buffer next))
			    (setq done t)))))))
	(save-excursion
	  (goto-char start)
	  (beginning-of-line)
	  ;; Do the displayed lines.
	  (while (and (< (point) end)
		      (not done)
		      (progn
			(check)
			(image-triage--next-file)))
	    (cl-incf lines))
	  ;; Do the next screen and a half.
	  (setq lines (truncate (* lines 1.5)))
	  (unless done
	    (let ((checked-lines 0))
	      (while (and (< checked-lines lines)
			  (not done)
			  (not (eobp)))
		(check)
		(image-triage--next-file)
		(cl-incf checked-lines))))
	  ;; Do the previous screen and a half.
	  (unless done
	    (goto-char start)
	    (let ((checked-lines 0))
	      (while (and (< checked-lines lines)
			  (not done))
		(check)
		(if (bobp)
		    (setq done t)
		  (image-triage--previous-file))
		(cl-incf checked-lines))))))))))

(defun image-triage--insert-thumbnail (data &optional dummy)
  (let ((inhibit-read-only t))
    (cond
     ;; We have no data, but mark the line.
     (dummy
      (put-text-property (point) (1+ (point)) 'thumbnail :error))
     ((let ((disp (get-text-property (point) 'display)))
	(and disp
	     (consp disp)
	     (eq (car disp) 'display)))
      ;; Do nothing if there is already an image here.  This should
      ;; never happen, so it's just a sanity check against two
      ;; fetching threads going off at once.
      )
     (t
      (let ((file (get-text-property (point) 'image-triage--file))
	    (help-echo (get-text-property (point) 'help-echo)))
	(delete-region (point) (1+ (point)))
	;; Protect against invalid image data.
	(condition-case ()
	    (insert
	     (propertize
	      " "
	      'image-triage--file file
	      'thumbnail t
	      'help-echo help-echo
	      'face
	      (if (eq image-triage--layout 'list)
		  'default
		`(:box ( :line-width 1
			 :color ,(if (member file image-triage--marked-files)
				     "blue"
				   (face-background 'default)))))
	      'display
	      `(,(if (eq image-triage--layout 'list)
		     (create-image data nil t
				   :height image-triage-thumbnail-height
				   :scale 1
				   :ascent 100)
		   (create-image data nil t
				 :width image-triage-thumbnail-width
				 :max-height image-triage--line-height
				 :scale 1
				 :ascent 100
				 :margin '(10 . 0))))))
	  (error nil)))))))

(defun image-triage--fetch-thumbnail (file buffer next &optional inhibit-fast
					   preview)
  (when (buffer-live-p buffer)
    (cond
     ;; If the file has disappeared in the meantime, just skip to
     ;; the next.
     ((not (file-exists-p file))
      (funcall next))
     ;; Fast utility distributed with `image-triage'.
     ((and (not inhibit-fast)
	   (file-exists-p (image-triage--jpgpreview)))
      (image-triage--fetch-thumbnail-from-exif
       file buffer next (if preview "preview" "thumb")
       (image-triage--jpgpreview) preview))
     ;; Slow version -- we first see what the thumbnail is called,
     ;; and then we extract it.
     (t
      (with-current-buffer buffer
	(setq image-triage--last-process (current-time)))
      (make-process
       :name "exiftool"
       :buffer (generate-new-buffer " *exiftool*")
       :command (list "exiftool" "-json" "-a" "-G1" "-s"
		      "-preview:all" file)
       :sentinel
       (lambda (proc _)
	 (with-current-buffer (process-buffer proc)
	   (let ((data nil))
	     (goto-char (point-min))
	     (unwind-protect
		 (let ((json (elt (ignore-errors
				    (json-parse-buffer :object-type 'plist))
				  0))
		       (case-fold-search t))
		   ;; Hopefully the thumbnail is called something like
		   ;; "thumbnail" and the preview something with
		   ;; "preview".
		   (setq data
			 (cl-loop
			  for (slot _value) on json by #'cddr
			  append (cond
				  ((string-match-p
				    "thumbnail" (symbol-name slot))
				   (list 'thumbnail
					 (substring (symbol-name slot) 1)))
				  ((string-match-p
				    "preview" (symbol-name slot))
				   (list 'preview
					 (substring (symbol-name slot) 1)))))))
	       (kill-buffer (current-buffer)))
	     (when (buffer-live-p buffer)
	       ;; We have the name of the thumbnail exif element, so
	       ;; extract it.
	       (image-triage--fetch-thumbnail-from-exif
		file buffer next
		(plist-get (if preview 'preview 'thumbnail) data)
		nil preview))))))))))

(defun image-triage--fetch-thumbnail-from-exif (file buffer next
						     thumbnail-name
						     &optional fast
						     preview)
  (with-current-buffer (generate-new-buffer " *exif thumbnail*")
    (set-buffer-multibyte nil)
    (with-current-buffer buffer
      (setq image-triage--last-process (current-time)))
    (make-process
     :name "exiftool"
     :buffer (current-buffer)
     :command (cond
	       (fast
		(list fast thumbnail-name file))
	       (thumbnail-name
		(list "exiftool" "-b" (concat "-" thumbnail-name) file))
	       (t
		;; If we don't have a thumbnail, then extract from the
		;; file itself.
		(list "convert" "-resize"
		      (format "x%d" (if preview
					image-triage-preview-height
				      image-triage-thumbnail-height))
		      file "jpg:-")))
     :sentinel
     (lambda (proc _)
       (if (and fast
		(not (zerop (process-exit-status proc))))
	   ;; If we used the fast version, we may not have gotten
	   ;; a thumbnail (because there wasn't one).  If so,
	   ;; go back and do it that hard way.
	   (image-triage--fetch-thumbnail file buffer next t preview)
	 (let ((thumbnail
		(with-current-buffer (process-buffer proc)
		  (prog1
		      (buffer-string)
		    (kill-buffer (current-buffer))))))
	   (when (buffer-live-p buffer)
	     (with-current-buffer buffer
	       (if preview
		   ;; We have a preview, so stash it in the correct
		   ;; alist.
		   (progn
		     (push (cons file thumbnail)
			   image-triage--precomputed-previews)
		     (funcall next))
		 ;; We have a thumbnail, so insert it in the list
		 ;; buffer.
		 (save-excursion
		   (let ((elem (gethash file image-triage--data)))
		     (setf (gethash file image-triage--data)
			   (plist-put elem 'thumbnail thumbnail)))
		   (when (image-triage--goto-file file)
		     (image-triage--insert-thumbnail
		      thumbnail (not (zerop (process-exit-status proc)))))
		   (funcall next)))))))))))

;; "(Binary data 10499 bytes, use -b option to extract)"
(defun image-triage--exif-bytes (string)
  (and (string-match " \\([0-9]+\\) bytes" string)
       (string-to-number (match-string 1 string))))

(provide 'image-triage)

;;; image-triage.el ends here
