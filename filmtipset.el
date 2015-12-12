;;; filmtipset.el --- Filmtipset.se in Emacs
;;
;; Copyright (C) 2011 Deniz Dogan
;;
;; Version: 1.0
;; Keywords: filmtipset
;; Author: Deniz Dogan <deniz@dogan.se>
;; Maintainer: Deniz Dogan
;;
;; This file is not part of GNU Emacs.
;;

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;;; Commentary:
;;
;; If you find any bugs or have suggestions for new functionality,
;; send me an e-mail.
;;
;; Install filmtipset.el by putting this file in your Emacs load path
;; and then using requiring it.  Example:
;;
;; (add-to-list 'load-path "~/repos/filmtipset")
;; (require 'filmtipset)

;;; Bugs:
;;
;; * There is no way to add comments.  This is because of some problem
;;   with the API for Filmtipset.
;;
;;; Coming features
;; * List film recommendations.
;; * Messaging.
;; * Add films to lists.

;;; Code:

(require 'sgml-mode) ;; sgml-char-names

;;;; Variables
(defvar ftip-buffer-name "*Filmtipset*"
  "Name of the Filmtipset buffer.")
(defvar ftip-history nil)
(defvar ftip-history-cursor nil)
(defvar ftip-running nil
  "If non-nil, something is loading asynchronously.")
(defvar ftip-cgi-format "http://www.filmtipset.se/api/api.cgi?%s"
  "URL format to use for the CGI script.")
(defvar ftip-access-key "rs2tIYX50JNNOOd7LZiU8A"
  "Access key for the Filmtipset mode.")
(defvar ftip-movie-id nil
  "ID of the currently last movie.")
(defvar ftip-movie-name nil
  "Name of the currently last movie.")

;;;; Customization
(defcustom ftip-user-key nil
  "Your Filmtipset user key for this application."
  :type 'string
  :group 'filmtipset)

(defcustom ftip-display-continuation-indications nil
  "If non-nil, display continuation indicators in the fringes."
  :type 'boolean
  :group 'filmtipset)

(defcustom ftip-display-images t
  "If non-nil, display images when supported."
  :type 'boolean
  :group 'filmtipset)

;;;; Faces
(defface ftip-error-face
  '((t (:foreground "red" :weight bold :height 1.4)))
  "Used for Filmtipset errors.")

(defface ftip-header-face
  '((t (:weight bold :height 1.4)))
  "Used for Filmtipset headers.")

(defface ftip-section-header-face
  '((t (:height 0.85 :inherit ftip-header-face)))
  "Used for Filmtipset section headers.")

(defface ftip-label-face
  '((t (:weight bold)))
  "Used for Filmtipset section headers.")

(defface ftip-grade-face
  '((t (:weight bold :slant normal)))
  "Face for grades.")

(defface ftip-grade-1-face
  '((t (:foreground "#ddc58a" :inherit ftip-grade-face)))
  "Face for grade 1.")

(defface ftip-grade-2-face
  '((t (:foreground "#fdbdd8" :inherit ftip-grade-face)))
  "Face for grade 2.")

(defface ftip-grade-3-face
  '((t (:foreground "#f7f48a" :inherit ftip-grade-face)))
  "Face for grade 3.")

(defface ftip-grade-4-face
  '((t (:foreground "#badcf2" :inherit ftip-grade-face)))
  "Face for grade 4.")

(defface ftip-grade-5-face
  '((t (:foreground "#c2f1b0" :inherit ftip-grade-face)))
  "Face for grade 5.")

(defface ftip-grade-?-face
  '((t (:foreground "#888" :inherit ftip-grade-face)))
  "Face for unknown grades.")

(defface ftip-seen-face
  '((t (:weight bold :underline t)))
  "Face for the \"seen\" indicator.")

(defface ftip-unseen-face
  '((t (:weight normal)))
  "Face for the \"unseen\" indicator.")

;;;; Macros
(defmacro ftip-interactive-button (text action &rest props)
  "Insert a text button with text TEXT and action ACTION.
ACTION will be called interactively when the button is clicked."
  (declare (indent 2))
  `(insert-text-button
    ,text 'action (lambda (button)
		    (call-interactively ',action 'interactive))
    ,@props))

(defmacro ftip-as-writable (&rest body)
  "Execute BODY with the buffer set to writable."
  `(let ((inhibit-read-only t))
     ,@body))

(defmacro ftip-progn-indent (width &rest body)
  "Execute BODY and indent the result with WIDTH spaces."
  (declare (indent 1))
  `(let ((old-point (point))
	 (prefix ,(make-string width ? )))
     ,@body
     (add-text-properties
      old-point (point) (list 'wrap-prefix prefix 'line-prefix prefix))))

(defmacro ftip-xml (node &rest order)
  `(let ((current-elem ,node))
     (dolist (item ',order)
       (when current-elem
	 (setq current-elem (aget current-elem item))))
     current-elem))

;;;; Util functions
(defun ftip-set-user-key (key)
  (interactive "sKey: ")
  (customize-save-variable 'ftip-user-key key)
  (message "Saved user key: %s" key)
  key)

(defun ftip-start-screen ()
  (interactive)
  (ftip-as-writable
   (erase-buffer)
   (insert (ftip-header "Filmtipset") "\n\n")
   (insert (ftip-section-header "Welcome") "\n")

   (if ftip-user-key
       (progn
	 (insert
	  "You have set your user key to " (propertize ftip-user-key 'face 'bold)
	  ".  Make sure this is correct before you proceed.  \
The user key is needed for commenting on movies and grade them, \
etc.  The basic functionality will still work without the user key \
but you probably wouldn't be here if you didn't want to use it.\n\n")
	 (ftip-interactive-button "Change user key" ftip-set-user-key))
     (insert "You have not set your user key yet.  Go to ")
     (let ((url "http://nyheter24.se/filmtipset/api.cgi?keys=1"))
       (ftip-button-link url url))
     (insert ", find this mode in the list and click \
\"Skapa nyckel\" (that's Swedish for \"Create key\").  Then ")
     (ftip-interactive-button "click here" ftip-set-user-key)
     (insert " to save it."))

   (insert
    "\n\n"
    (ftip-section-header "Basic Usage") "\n"
    "The following keys are used:\n"
    (substitute-command-keys "  `\\[ftip-search-movie]' -- Search for movies\n")
    (substitute-command-keys "  `\\[ftip-rate-movie]' -- Rate the last viewed movie\n")
    (substitute-command-keys "  `\\[ftip-goto-movie]' -- Go to movie by ID\n")
    (substitute-command-keys "  `\\[ftip-history-backward]' -- Go back in history\n")
    (substitute-command-keys "  `\\[ftip-history-forward]' -- Go forward in history\n")
    (substitute-command-keys "  `\\[ftip-start-screen]' -- Go to this page\n")
    "\n")
   (goto-char (point-min))
   (search-forward "\n" nil t)))

(defun ftip-c (string)
  "Convert HTML entities in STRING and return the result.
Uses `sgml-char-names' for conversion."
  (let ((from 0)
	entity
	char)
    (while (string-match "\\(&\\([[:alnum:]]+\\);\\)" string from)
      (setq entity (match-string 2 string)
	    char (position entity sgml-char-names :test 'string=)
	    string (replace-match (char-to-string char) t t string 1)
	    from (1+ (match-beginning 1))))
    string))

(defun ftip-d (text)
  (decode-coding-string (or text "") 'windows-1254))

(defun ftip-s (text)
  (replace-regexp-in-string "[\n ]+" " " text))

(defun ftip-button-link (text url)
  (insert-text-button text 'action 'ftip-button-link-action 'link url))

(defun ftip-button-link-action (button)
  (let ((url (button-get button 'link)))
    (browse-url url)))

(defun ftip-button-movie (text id)
  (insert-text-button text 'action 'ftip-button-movie-action 'id id))

(defun ftip-button-movie-action (button)
  (let ((id (button-get button 'id)))
    (ftip-goto-movie id)))

(defun ftip-button-person (text id)
  (insert-text-button text 'action 'ftip-button-person-action 'id id))

(defun ftip-button-person-action (button)
  (let ((id (button-get button 'id)))
    (ftip-request "person" `(("id" . ,id)))))

(defun ftip-button-user (text id)
  (insert-text-button text 'action 'ftip-button-user-action 'id id))

(defun ftip-button-user-action (button)
  (let ((id (button-get button 'id)))
    (ftip-request "user" `(("id" . ,id)
			   ("ignorelists" . "1")))))

(defun ftip-grade (grade)
  (setq grade (or grade "?"))
  (propertize grade 'face (intern-soft (format "ftip-grade-%s-face" grade))))

(defun ftip-label (text)
  (propertize text 'face 'ftip-label-face))

(defun ftip-header (text)
  (propertize text 'face 'ftip-header-face))

(defun ftip-section-header (text)
  (propertize text 'face 'ftip-section-header-face))

(defun ftip-error-handler (err pairs)
  (erase-buffer)
  (let ((text (cdr err)))
    (insert (propertize "Error" 'face 'ftip-error-face) "\n\n")
    (insert "The error was: \"" text "\".\n\n")
    (insert "An unexpected error occurred.  If you think that you found a bug, please contact the author of ftip.el with details on how to reproduce the error.  Please include the following information:\n\n")
    (insert "Request information:\n")
    (dolist (p pairs)
      (insert "  " (car p) ": " (cdr p) "\n"))))

(defun ftip-error (&rest text)
  "Signal a Filmtipset error with TEXT."
  (signal 'ftip-error (apply 'concat text)))

;; based on `http-url-encode' from the from http-get package found at
;; http://savannah.nongnu.org/projects/http-emacs
(defun ftip-url-encode (str)
  "URL encode STR using iso-8859-1."
  (apply 'concat
	 (mapcar (lambda (c)
		   (if (or (and (>= c ?a) (<= c ?z))
			   (and (>= c ?A) (<= c ?Z))
			   (and (>= c ?0) (<= c ?9)))
		       (string c)
		     (format "%%%02x" c)))
		 (encode-coding-string str 'iso-8859-1))))

(defun ftip-qs (alist)
  "Build a query string from ALIST."
  (mapconcat
   (lambda (pair)
     (concat (ftip-url-encode (car pair)) "=" (ftip-url-encode (cdr pair))))
   alist "&"))

(defun ftip-movie-grade< (m1 m2)
  (let ((g1 (cadr (ftip-xml m1 grade value)))
	(g2 (cadr (ftip-xml m2 grade value))))
    (if g1
	(if g2
	    (not (string< g1 g2))
	  t)
      nil)))

(defun ftip-running-time (minutes)
  "Return MINUTES formatted in hours and minutes."
  (when (stringp minutes)
    (setq minutes (string-to-number minutes)))
  (format-seconds "%hhrs %z%mmins" (* 60 minutes)))

(defun ftip-imdb-movie-url (imdb)
  (format "http://www.imdb.com/title/tt%s/" imdb))

;;;; Handlers
(defun ftip-handler-grade (doc)
  (ftip-handler-movie doc))

(defun ftip-insert-grade-seen (grade-value grade-type)
  (let ((grade-face (intern-soft (format "ftip-grade-%s-face" grade-value))))
    (insert
     (propertize
      (or grade-value "?") 'face
      (list grade-face
	    (if (string= grade-type "seen")
		'ftip-seen-face
	      'ftip-unseen-face)))
     " ")))

(defun ftip-handler-user (doc)
  (let* ((id (cdr (ftip-xml doc info data id)))
	 (member (cdr (ftip-xml doc info data member)))
	 (name (ftip-d (ftip-d (cadr (ftip-xml member name)))))
	 (personaltext (ftip-c (ftip-d (cadr (ftip-xml member personaltext)))))
	 (firstname (ftip-d (cadr (ftip-xml member firstname))))
	 (lastname (ftip-d (cadr (ftip-xml member lastname))))
	 (fullname (if (not (zerop (length firstname)))
		       (if (not (zerop (length lastname)))
			   (concat firstname " " lastname)
			 firstname)
		     (if (not (zerop (length lastname)))
			 lastname
		       nil)))
	 (age (or (cadr (ftip-xml member age)) "Unknown"))
	 (activity (cadr (ftip-xml member activity)))
	 (city (ftip-d (cadr (ftip-xml member activity)))))
    (insert (ftip-header (concat name (if fullname
					  (concat " (" fullname ")")
					""))) "\n\n"
	    (ftip-label "Age: ") age "\n"
	    (ftip-label "Activity: ") activity "\n"
	    (ftip-label "City: ") city "\n\n")
    (insert (ftip-section-header "Presentation") "\n"
	    personaltext)
    (goto-char (point-min))
    (search-forward "\n" nil t)))

(defun ftip-handler-person (doc)
  (let* ((actor (ftip-xml doc info data actor))
	 (name (ftip-d (cadr (ftip-xml actor name))))
	 (type (cadr (ftip-xml actor type)))
	 (comments (cdr (ftip-xml actor comments)))
	 (movies (cdr (ftip-xml actor movies))))
    (insert (ftip-header name) " (as " type ")\n\n")
    (insert (ftip-section-header "Movies") "\n")
    (setq movies (sort movies 'ftip-movie-grade<))
    (dolist (movie movies)
      (let* ((id (ftip-d (cadr (ftip-xml movie id))))
	     (name (ftip-d (cadr (ftip-xml movie name))))
	     (orgname (ftip-d (cadr (ftip-xml movie orgname))))
	     (year (cadr (ftip-xml movie year)))
	     (grade (cdr (ftip-xml movie grade)))
	     (grade-value (cadr (ftip-xml grade value)))
	     (grade-type (cadr (ftip-xml grade type))))
	(ftip-insert-grade-seen grade-value grade-type)
	(ftip-button-movie name id)
	(insert " (" year ")\n")))
    (goto-char (point-min))
    (search-forward "\n" nil t)))

(defun ftip-handler-search (doc)
  (let* ((data (cdr (ftip-xml doc info data)))
	 (search-string (ftip-d (cadr (ftip-xml data search))))
	 (movies (cdr (ftip-xml data hits))))
    (insert (ftip-header (concat "Search results: " search-string)) "\n\n")
    (dolist (movie movies)
      (let* ((id (cadr (ftip-xml movie id)))
	     (name (ftip-d (cadr (ftip-xml movie name))))
	     (orgname (ftip-d (cadr (ftip-xml movie orgname))))
	     (grade (cdr (ftip-xml movie grade)))
	     (grade-value (or (cadr (ftip-xml grade value)) "?"))
	     (grade-type (cadr (ftip-xml grade type))))
	(ftip-insert-grade-seen grade-value grade-type)
	(ftip-button-movie name id)
	(unless (string= orgname name)
	  (insert " (" orgname ")"))
	(add-text-properties (point-at-bol) (point) '(wrap-prefix "  "))
	(insert "\n")))
    (goto-char (point-min))
    (search-forward "\n" nil t)))

(defun ftip-handler-movie (doc)
  (unless (cadr (ftip-xml doc info data))
    (ftip-error "Movie not found"))
  (let* ((movie (cdr (ftip-xml doc info data movie)))
	 (id (ftip-d (cadr (ftip-xml movie id))))
	 (name (ftip-d (cadr (ftip-xml movie name))))
	 (orgname (ftip-d (cadr (ftip-xml movie orgname))))
	 (description (ftip-s (ftip-d (cadr (ftip-xml movie description)))))
	 (grade (cdr (ftip-xml movie grade)))
	 (grade-value (or (cadr (ftip-xml grade value)) "?"))
	 (grade-type (cadr (ftip-xml grade type)))
	 (grade-face (intern-soft (format "ftip-grade-%s-face" grade-value)))
	 (imdb (cadr (ftip-xml movie imdb)))
	 (url (cadr (ftip-xml movie url)))
	 (length (cadr (ftip-xml movie length)))
	 (country (cadr (ftip-xml movie country)))
	 (writerids (cdr (ftip-xml movie writerids)))
	 (directorids (cdr (ftip-xml movie directorids)))
	 (actorids (cdr (ftip-xml movie actorids)))
	 (genres (cdr (ftip-xml movie genres)))
	 (image (cadr (ftip-xml movie image)))
	 (comments (cdr (ftip-xml movie comments))))
    (setq ftip-movie-id id
	  ftip-movie-name name)
    (insert (propertize grade-value
			'face
			(list grade-face
			      (if (string= grade-type "seen")
				  'ftip-seen-face
				'ftip-unseen-face)
			      'ftip-header-face
			      ))
	    " "
	    (ftip-header name)
	    (if (not (string= name orgname))
		(format " (%s)" orgname)
	      "")
	    "\n\n")

    (when (and image ftip-display-images (display-images-p))
      (let ((temp-dir (getenv "TEMP")))
	(when temp-dir
	  (let ((filename (convert-standard-filename (concat temp-dir "/" id ".jpg"))))
	    (url-copy-file image filename t)
	    (let ((image-data (create-image filename nil nil :margin 2)))
	      (when image-data
		(let ((old-point (point))
		      (new-point (progn
				   (insert-image image-data " ")
				   (insert "\n")
				   (point))))
		  (add-text-properties (1- old-point) new-point '(intangible t))
		  (insert "\n"))))))))

    (insert (ftip-section-header "Information") "\n")
    (insert (ftip-label "Country: ") country "\n")
    (insert (ftip-label "Running time: ") (ftip-running-time length) "\n")

    (when genres
      (insert (ftip-label "Genres: "))
      (dolist (genre genres)
	(insert (ftip-c (nth 2 genre)) ", "))
      (backward-delete-char 2)
      (insert "\n"))

    (when writerids
      (insert (ftip-label "Writers: "))
      (dolist (writer writerids)
	(let ((name (ftip-d (cadr (ftip-xml writer name))))
	      (id (cadr (ftip-xml writer id))))
	  (ftip-button-person name id)
	  (insert ", ")))
      (backward-delete-char 2)
      (insert "\n"))

    (when directorids
      (insert (ftip-label "Directors: "))
      (dolist (director directorids)
	(let ((name (ftip-d (cadr (ftip-xml director name))))
	      (id (cadr (ftip-xml director id))))
	  (ftip-button-person name id)
	  (insert ", ")))
      (backward-delete-char 2)
      (insert "\n"))

    (when actorids
      (insert (ftip-label "Actors: "))
      (dolist (actor actorids)
	(let ((name (ftip-d (cadr (ftip-xml actor name))))
	      (id (cadr (ftip-xml actor id))))
	  (ftip-button-person name id)
	  (insert ", ")))
      (backward-delete-char 2)
      (insert "\n")

      (insert (ftip-label "Links: "))
      (ftip-button-link "IMDb" (ftip-imdb-movie-url imdb))
      (insert ", ")
      (ftip-button-link "Filmtipset" (ftip-imdb-movie-url url))
      (insert "\n\n"))

    (insert (ftip-section-header "Description") "\n")
    (insert description "\n\n")

    (when comments
      (insert (ftip-section-header "Comments") "\n")
      (dolist (comment comments)
	(let ((text
	       (ftip-c
		(ftip-s
		 (ftip-d (cadr (ftip-xml comment text))))))
	      (date (cadr (ftip-xml comment date)))
	      (member (cadr (ftip-xml comment member)))
	      (name (ftip-d (cadr (ftip-xml comment name))))
	      (grade (cadr (ftip-xml comment grade))))
	  (insert (ftip-grade grade) " ")
	  (ftip-button-user name member)
	  (insert " " date ":\n")
	  (insert (propertize (concat text)
			      'wrap-prefix "  "
			      'line-prefix "  ") "\n"))))
    (insert "\n")
    (goto-char (point-min))
    (re-search-forward "\n" nil)))

;;;; History
(defun ftip-add-history (action pairs)
  ;; Remove anything in front of the pointer.
  (setq ftip-history
	(butlast ftip-history
		 (- (length ftip-history)
		    ftip-history-pointer
		    1)))
  ;; Add to the history.
  (setq ftip-history (append ftip-history (list (cons action pairs))))
  ;; Point to the last history entry.
  (setq ftip-history-pointer (1- (length ftip-history))))

(defun ftip-history-forward ()
  (interactive)
  (if (< ftip-history-pointer (1- (length ftip-history)))
      (progn
	(incf ftip-history-pointer)
	(let* ((entry (nth ftip-history-pointer ftip-history))
	       (action (car entry))
	       (pairs (cdr entry)))
	  (ftip-request action pairs t)))
    (message "No history forward")))

(defun ftip-history-backward ()
  (interactive)
  (if (<= ftip-history-pointer 0)
      (message "No history backward")
    (decf ftip-history-pointer)
    (let* ((entry (nth ftip-history-pointer ftip-history))
	   (action (car entry))
	   (pairs (cdr entry)))
      (ftip-request action pairs t))))

(defun ftip-request-callback (status pairs inhibit-history)
  (setq ftip-running nil)
  (search-forward "\n\n" nil t)
  (delete-region (point) (point-min))
  (let* ((document (xml-parse-region (point-min) (point-max)))
	 (action (aget pairs "action"))
	 (handler (intern-soft (format "ftip-handler-%s" action)))
	 (buffer (get-buffer ftip-buffer-name)))
    (when buffer
      (with-current-buffer buffer
	(setq mode-line-process "")
	(ftip-as-writable
	 (erase-buffer)
	 (condition-case e
	     (if (fboundp handler)
		 (progn
		   (erase-buffer)
		   (funcall handler document)
		   (unless inhibit-history
		     (ftip-add-history action pairs)))
	       (signal 'ftip-error
		       (format "No handler for action \'%s\'" action)))
	   (ftip-error
	    (ftip-error-handler e pairs))))))))

(defun ftip-request (action args &optional inhibit-history)
  (if ftip-running
      (message "Wait for the current request to finish.")
    (let* ((pairs (append `(("action" . ,action)
			    ("returntype" . "xml")
			    ("accesskey" . ,ftip-access-key))
			  (if ftip-user-key
			      `(("userkey" . ,ftip-user-key))
			    nil)
			  args))
	   (qs (ftip-qs pairs))
	   (url (format ftip-cgi-format qs)))
      (setq ftip-running (url-retrieve url 'ftip-request-callback (list pairs inhibit-history) t))
      (setq mode-line-process "/Loading..."))))

;;;; Commands
(defun ftip-goto-movie (movie-id)
  (interactive "nMovie ID: ")
  (when (numberp movie-id)
    (setq movie-id (number-to-string movie-id)))
  (ftip-request "movie" `(("id" . ,movie-id))))

(defun ftip-search-movie (movie-name)
  (interactive "sMovie name: ")
  (ftip-request "search" `(("id" . ,movie-name))))

(defun ftip-rate-movie (grade)
  (interactive
   (list
    (if (not (and ftip-movie-id ftip-movie-name))
	(error "Not viewing any movie")
      (when (not ftip-user-key)
	(if (y-or-n-p "You haven't set your user key.  \
Would you like to do that now? (y/n)")
	    (let ((user-key
		   (read-string "Enter your user key: ")))
	      (customize-save-variable 'ftip-user-key user-key)
	      (message "Setting it to: %s" user-key)
	      (call-interactively 'ftip-rate-movie))
	  (error "You haven't set your user key.")))
      (read-char (format "Rate \"%s\" (0 means erase): " ftip-movie-name)))))
  (unless ftip-movie-id
    (ftip-error "Not viewing any movie"))
  (ftip-request
   "grade"
   (append `(("id" . ,ftip-movie-id)
	     ("name" . ,ftip-movie-name))
	   (unless (eql grade ?0)
	     `(("grade" . ,(char-to-string grade)))))))

;;;; Mode
(defun ftip ()
  (interactive)
  (let ((buffer (get-buffer ftip-buffer-name)))
    (if buffer
	(switch-to-buffer buffer)
      (setq buffer (generate-new-buffer ftip-buffer-name))
      (switch-to-buffer buffer)
      (ftip-mode))))
(defalias 'filmtipset 'ftip)

(define-derived-mode ftip-mode special-mode "Filmtipset"
  "Filmtipset major mode."

  ;; Fringe indicators for continuations are ugly and somewhat
  ;; unnecessary in this mode.
  (unless ftip-display-continuation-indications
    (dolist (item fringe-indicator-alist)
      (when (eq (car item) 'continuation)
	(setq fringe-indicator-alist (delete item fringe-indicator-alist)))))

  (ftip-start-screen)
  (setq word-wrap t
	mode-line-process ""
	ftip-history nil
	ftip-history-pointer -1))

(define-key ftip-mode-map (kbd "v") 'ftip-goto-movie)
(define-key ftip-mode-map (kbd "s") 'ftip-search-movie)
(define-key ftip-mode-map (kbd "h") 'ftip-start-screen)
(define-key ftip-mode-map (kbd "r") 'ftip-rate-movie)
(define-key ftip-mode-map (kbd "b") 'ftip-history-backward)
(define-key ftip-mode-map (kbd "f") 'ftip-history-forward)
(define-key ftip-mode-map (kbd "TAB") 'forward-button)
(define-key ftip-mode-map (kbd "<S-tab>") 'backward-button)

(put 'ftip-error 'error-conditions '(error ftip-error))
(put 'ftip-error 'error-message "Filmtipset error")

(provide 'filmtipset)
