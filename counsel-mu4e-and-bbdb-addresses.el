;;; counsel-mu4e-and-bbdb-addresses.el --- Counsel addressbook search using mu4e contacts and bbdb. -*- lexical-binding: t -*-

;; Author: Boris Glavic <lordpretzel@gmail.com>
;; Maintainer: Boris Glavic <lordpretzel@gmail.com>
;; Version: 0.1
;; Package-Requires: ((advice-tools "0.1") (dash "2.12.0") (counsel-bbdb "20181128.1320") (bbdb "3.2") (ht "20201119.518") idf)
;; Homepage: https://github.com/lordpretzel/counsel-mu4e-and-bbdb-addresses
;; Keywords:


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Contact completion using counsel over a combination of mu4e and bbdb contacts.

;;; Code:

;; ********************************************************************************
;; IMPORTS
(require 'subr-x)
(require 'cl-lib)
(require 'advice-tools)
(require 'counsel-bbdb)
(require 'bbdb)
(require 'dash)
(require 'ht)
(require 'mu4e)
(require 'bbdb-com)
(require 'idf)

;; ********************************************************************************
;; CUSTOM
(defcustom counsel-mu4e-and-bbdb-addresses-counsel-bbdb-only-use-primary-email-for-groups
  t
  "Use only primary email address for persons when expanding group."
  :group 'counsel-mu4e-and-bbdb-addresses
  :type 'boolean)

(defcustom counsel-mu4e-and-bbdb-addresses--logging
  nil
  "If true than show a lot of log output for debugging."
  :group 'counsel-mu4e-and-bbdb-addresses
  :type 'boolean)

;; ********************************************************************************
;; VARIABLES
(defvar counsel-mu4e-and-bbdb-addresses-mu4e-contacts-history nil
  "History of completions for counsel-mu4e-and-bbdb-addresses-mu4e-contacts.")

(defvar counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended nil
  "HT that stores mu4e~contexts and bbdb contacts for mu4e completion.")

(defvar counsel-mu4e-and-bbdb-addresses-mu4e-contacts nil
  "Replaces `mu4e~contacts'.")

(defvar counsel-mu4e-and-bbdb-addresses-counsel-bbdb-contacts nil
  "Extends `counsel-bbdb-contacts'.")

(defvar counsel-mu4e-and-bbdb-addresses--counsel-bbdb-mv nil
  "Materialized IDF view for counsel-bbdb formated addresses.")

(defvar counsel-mu4e-and-bbdb-addresses--counsel-all-mv nil
  "Materialized IDF view of contacts for address completion.")

(defvar counsel-mu4e-and-bbdb-addresses--mu4e-search-completion-mv nil
  "Materialized IDF view of contacts for mu4e search completion.")

(defvar counsel-mu4e-and-bbdb-addresses-mu4e-previous-contacts nil
  "Store copy of mu4e contacts hashmap to determine differences.")

(defvar counsel-mu4e-and-bbdb-addresses-bbdb-new-contact-cache nil
  "Cache newly created contact from BBDB.")

(defvar mu4e-search-company-completion-contacts nil
  "Define to avoid cyclic imports.")

;; ********************************************************************************
;; FUNCTIONS

;; ********************************************************************************
;; helper function for debug logging
(defmacro counsel-mu4e-and-bbdb-addresses--log (format-string &rest args)
  "Call message with FORMAT-STRING and ARGS.

Only writes messages when `mu4e-views--debug' is true."
  `(when counsel-mu4e-and-bbdb-addresses--logging
     (message ,format-string ,@args)))


;; return mu4e contacts in a version independent way
(defun counsel-mu4e-and-bbdb-addresses-get-mu4e-contacts ()
  "Return mu4e's the internal hashmap of contacts."
  (cond
   ((version-list-<= '(1 8) (version-to-list mu4e-mu-version))
    mu4e--contacts-set)
   ((version-list-<= '(1 7) (version-to-list mu4e-mu-version))
    mu4e--contacts-hash)
   ((version-list-<= '(1 5) (version-to-list mu4e-mu-version))
    mu4e~contacts-hash)
   (t mu4e~contacts)))

;; ivy-rich helper functions
(defun counsel-mu4e-and-bbdb-addresses-counsel-bbdb-get-name (entry)
  "Get name from counsel-bbdb string ENTRY which is `name:email => alias'."
  (save-match-data
    (if (string-match "\\([^:]+\\)" entry)
        (string-trim (match-string 1 entry))
      "")))

(defun counsel-mu4e-and-bbdb-addresses-counsel-bbdb-get-email (entry)
  "Get email from counsel-bbdb string ENTRY which is `name:email => alias'."
  (save-match-data
    (if (string-match "\\([^:]+\\)[:]\\([^=]*\\)" entry)
        (string-trim (match-string 2 entry))
      "")))

(defun counsel-mu4e-and-bbdb-addresses-counsel-bbdb-get-alias (entry)
  "Get mail aliases from counsel-bbdb string ENTRY which is `name:email => alias'."
  (save-match-data
    (if (string-match "\\(.*\\)[=][>]\\(.*\\)" entry)
        (string-trim (match-string 2 entry))
      "")))

(defun counsel-mu4e-and-bbdb-addresses-prepend-comma ()
  "Insert a comma before the current position.

Unless it is an empty to/bcc/cc prefix or there already is a comma."
  (let (match
        insert-comma-point)
    (save-excursion
      (re-search-backward "\\([^[:space:]]\\)[[:space:]]*")
      (setq match (match-string 1))
      (when (not (or (string-equal match ":") (string-equal match ",")))
        (setq insert-comma-point (1+ (match-beginning 1)))))
    (when insert-comma-point
      (goto-char insert-comma-point)
      (insert ", "))))

;; counsel-bbdb insert mail
(defun counsel-mu4e-and-bbdb-addresses-counsel-bbdb-insert-email (r &optional prepend-comma append-comma)
  "Function to insert email address R selected from counsel-bbdb.

If APPEND-COMMA / PREPEND-COMMA then append / prepend a comma.
If this is the first email address in a to/cc/bcc field or there
already is a comma then do not prepend."
  (interactive)
  (let* ((points (bounds-of-thing-at-point 'symbol)))
    (when points
      (delete-region (car points) (cdr points))))
  ;; prepend "," if necessary
  (when prepend-comma
    (counsel-mu4e-and-bbdb-addresses-prepend-comma))
  (counsel-bbdb-insert-one-mail-address r append-comma))

(defun counsel-mu4e-and-bbdb-addresses-counsel-bbdb-open-in-bbdb-action (r)
  "Open a person `R` selected in counsel-bbdb in BBDB."
  (interactive)
  (unless counsel-bbdb-contacts
    (counsel-bbdb-reload))
  (let* ((family-name (nth 1 r))
         (mail (nth 4 r)))
    (when (and family-name mail)
      (bbdb-display-records (bbdb-search (bbdb-records)
                                         :bool 'and
                                         :mail mail
                                         :name-lf family-name)))
    (when (and family-name (not mail))
      (bbdb-display-records (bbdb-search (bbdb-records)
                                         :name-lf family-name)))
    (when (and (not family-name) mail)
      (bbdb-display-records (bbdb-search (bbdb-records)
                                         :mail mail)))))

;; add receipient to TO , CC, or BCC with counsel-bbdb completion
(defun counsel-mu4e-and-bbdb-addresses-counsel-bbdb-add (&optional where)
  "Add receipient to WHERE with counsel-bbdb.

WHERE should be a symbol, one of `to', `cc', or `bcc'."
  (interactive)
  (let ((where (or where
                   (intern (ivy-read "Add to: "
                                     '("to" "cc" "bcc")
                                     :require-match t
                                     :preselect "to")))))
    (pcase where
      ('to (message-goto-to))
      ('cc (message-goto-cc))
      ('bcc (message-goto-bcc))
      (_ (error "Only to, cc, bcc are allowed options for inserting receipient"))))
  (counsel-bbdb-complete-mail t))

;; replacement functions for counsel-bbdb
(defun counsel-mu4e-and-bbdb-addresses-mu4e-counsel-bbdb-complete-mail (&optional append-comma)
  "Select person from BBDB to insert into email as to/cc/bcc.

Complete email address before point. Extra argument APPEND-COMMA
will append comma after email."
  (interactive "P")
  (ignore append-comma)
  (counsel-mu4e-and-bbdb-addresses-create-contacts-list-from-mu4e-and-bbdb)
  (ivy-read "Contacts: "
            (idf-mv-get-result counsel-mu4e-and-bbdb-addresses--counsel-bbdb-mv);; counsel-mu4e-and-bbdb-addresses-counsel-bbdb-contacts
            :initial-input (or (thing-at-point 'symbol) "")
            :caller 'counsel-bbdb-complete-mail
            :action (lambda (r) (counsel-mu4e-and-bbdb-addresses-counsel-bbdb-insert-email r t nil))))

(defun counsel-mu4e-and-bbdb-addresses-counsel-bbdb ()
  "Lookup people in BBDB."
  (interactive)
  (counsel-mu4e-and-bbdb-addresses-create-contacts-list-from-mu4e-and-bbdb)
  (ivy-read "Contacts: "
            ;; counsel-mu4e-and-bbdb-addresses-counsel-bbdb-contacts
            (idf-mv-get-result counsel-mu4e-and-bbdb-addresses--counsel-bbdb-mv)
            ;; lordpretzel-counsel-bbdb-contacts
            :initial-input (or (thing-at-point 'symbol) "")
            :caller 'counsel-bbdb-complete-mail
            :action '(1
                      ("b" lordpretzel/counsel-bbdb-open-in-bbdb-action "open in BBDB")
                      ("r" counsel-bbdb-reload "reload from BBDB file"))))

(defun counsel-mu4e-and-bbdb-addresses-counsel-bbdb-expand-mail-alias ()
  "Insert multiple mail address in alias/group."
  (interactive)
  (unless counsel-bbdb-contacts
    (counsel-bbdb-reload))
  ;; We just need filter the `counsel-bbdb-contacts' by selected alias
  (let* ((alias (ivy-read "Alias: "
                          counsel-bbdb-mail-alias-list
                          :caller 'counsel-bbdb-expand-mail-alias))
         (names nil))
    (when alias
      (counsel-mu4e-and-bbdb-addresses-prepend-comma)
      (dolist (r counsel-bbdb-contacts)
        (let* ((r-alias (nth 4 (cdr r)))
               (name (nth 1 r)))
          (when (and r-alias
                     (string-match-p (format "%s\\(,\\| *$\\)" alias) r-alias))
            (unless (member name names)
              (counsel-bbdb-insert-one-mail-address r t)
              (push name names)
              (counsel-mu4e-and-bbdb-addresses--log "%s names" names)))))
      (goto-char (line-beginning-position))
      (re-search-forward ",[[:space:]]*$" (line-end-position))
      (replace-match ""))))

(defun counsel-mu4e-and-bbdb-addresses-counsel-bbdb-reload ()
  "Load contacts from `bbdb-file'."
  (interactive)
  (let* (raw-records)
    (with-temp-buffer
      (insert-file-contents bbdb-file)
      (goto-char (point-min)) (insert "(\n")
      (goto-char (point-max)) (insert "\n)")
      (goto-char (point-min))
      (setq raw-records (read (current-buffer))))
    ;; convert to ivy friendly list with readable keyword:
    ;;   - full-name:mail
    ;;   - given-name family-name:mail
    ;;   - :mail
    (setq counsel-bbdb-contacts nil)
    (setq counsel-bbdb-mail-alias-list nil)
    (dolist (r raw-records)
      (let* ((full-name (counsel-bbdb-full-name r))
             (family-name (counsel-bbdb-family-name r))
             (given-name (counsel-bbdb-given-name r))
             (mails (counsel-bbdb-emails r))
             (mail-alias (counsel-bbdb-mail-alias r))
             (prefix full-name))

        (when mail-alias
          (let* ((strs (split-string mail-alias ", ")))
            (dolist (s strs)
              (add-to-list 'counsel-bbdb-mail-alias-list s))))

        (when (= (length prefix) 0)
          (setq prefix (concat family-name
                               " "
                               given-name)))
        (if (= (length prefix) 1) (setq prefix ""))

        (dolist (m mails)
          (add-to-list 'counsel-bbdb-contacts
                       (cons (concat prefix
                                     ":"
                                     m
                                     (if mail-alias (format " => %s" mail-alias)))
                             (list family-name
                                   given-name
                                   full-name
                                   m
                                   mail-alias))))
        (setq counsel-bbdb-contacts (nreverse counsel-bbdb-contacts))))))

;; extracting information from entries
(defun counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address (add)
  "Extract email address from mu4e contact ADD.

ADD may use one of two formats: 'email' or 'name <email>'."
  (save-match-data
	(if (string-match "<\\([^>]+\\)>" add)
		(match-string 1 add)
	  add)))

(defun counsel-mu4e-and-bbdb-addresses--get-email (add)
  "Get email from contact ADD in ivy-completion."
  (plist-get (cdr add) :email))

(defun counsel-mu4e-and-bbdb-addresses-mu4e-extract-name-from-contact (add)
  "Extract name from a mu4e-contact ADD.

If ADD is of the form 'name <email>', return nil if this is only
an email address."
  (save-match-data
	(if (string-match "\\([^<]+\\)<\\([^>]+\\)>" add)
		(string-trim (match-string 1 add))
	  nil)))

;; return name or "" if there is no name
(defun counsel-mu4e-and-bbdb-addresses-mu4e-extract-name-or-emtpy-string (add)
  "Extract name from a mu4e-contact ADD.

If ADD it of the form 'name <email>', return empty string if this
is only an email address."
  (let ((name (counsel-mu4e-and-bbdb-addresses-mu4e-extract-name-from-contact add)))
	(if name
		name
	  "")))

;; find all email addressses for mu4e person (TODO do smarter matching)
(defun counsel-mu4e-and-bbdb-addresses-mu4e-get-all-email-addresses-for-person (add)
  "Find all emails from a given mu4e-contact ADD.

If the contact ADD is of the form 'name <email>', then also
return emails send from contacts with the same name but different
email address."
  (let ((person (counsel-mu4e-and-bbdb-addresses-mu4e-extract-name-from-contact (car add)))
		(emails (list (counsel-mu4e-and-bbdb-addresses--get-email add))))
	(counsel-mu4e-and-bbdb-addresses-ensure-mu4e-contacts)
	(when person
	  (setq emails (append emails (cl-remove-duplicates (mapcar (lambda (a) (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address a)) (all-completions person (counsel-mu4e-and-bbdb-addresses-get-mu4e-contacts)))))))
	emails))

;; create query for returnning all emails send from person x (retrieving all email addressed for person from mu4e~contacts)
(defun counsel-mu4e-and-bbdb-addresses-mu4e-get-all-emails-from-person (add)
  "Return a mu4e query to retrieve all emails send from a person ADD."
  (concat "(maildir:\"/lordpretzel-gmail/[lordpretzel].All Mail\" OR maildir:\"/bglavic-iit/[bglavic].All Mail\" OR maildir:/bglavic-iit/INBOX OR maildir:/lordpretzel-gmail/INBOX) AND ("
		  (mapconcat (lambda (x) (concat "from:" x)) (counsel-mu4e-and-bbdb-addresses-mu4e-get-all-email-addresses-for-person add) " OR ") ")"))

(defun counsel-mu4e-and-bbdb-addresses-mu4e-extract-is-org-from-address (add)
  "Return * if ADD ends in # (an org-contact contact) and space otherwise."
  (if (string-match "[#]$" add)
	  "*"
	"-"))

(when (version-list-<= '(1 7) (version-to-list mu4e-mu-version))
  (defalias 'mu4e~request-contacts-maybe 'mu4e--request-contacts-maybe))

(defun counsel-mu4e-and-bbdb-addresses-ensure-mu4e-contacts ()
  "Make sure that mu4e contacts have been loaded."
  (unless (counsel-mu4e-and-bbdb-addresses-get-mu4e-contacts)
    (when (not (version-list-< (version-to-list mu4e-mu-version) '(1 7)))
      (mu4e--init-handlers))
	(mu4e~request-contacts-maybe)
	(let ((prevcount 0))
	  (while (not (counsel-mu4e-and-bbdb-addresses-get-mu4e-contacts))
        (sleep-for 0 100)	    )
      (sleep-for 0 100)
	  ;; this is async, so we have to poll
      (while (not (eq (hash-table-count (counsel-mu4e-and-bbdb-addresses-get-mu4e-contacts)) prevcount))
        (sleep-for 0 100)
        (setq prevcount (hash-table-count (counsel-mu4e-and-bbdb-addresses-get-mu4e-contacts)))))))

;; ********************************************************************************
;; FUNCTIONS FOR CREATING AND MAINTAINING DATA STRUTURES
(defun counsel-mu4e-and-bbdb-addresses--contact-less-p  (a b)
  "Compare two contacts A and B for sorting.

BBDB contacts come first. Secondary we sort on name in increasing order."
  (let ((ca (plist-get (cdr a) :contact-from))
        (cb (plist-get (cdr b) :contact-from))
        (na (plist-get (cdr a) :sort-name))
        (nb (plist-get (cdr b) :sort-name))
        (ea (plist-get (cdr a) :email))
        (eb (plist-get (cdr a) :email)))
    (or (string< ca cb)
        (and (string= ca cb)
             (string< na nb))
        (and (string= ca cb)
             (string= na nb)
             (string< ea eb)))))

(defun counsel-mu4e-and-bbdb-addresses--contact-bbdb-less-p (a b)
  "Compare two BBDB contact A and B for sorting."
  (or (string< (nth 5 a)
               (nth 5 b))
      (and (string= (nth 5 a)
                    (nth 5 b))
           (string< (nth 2 a)
                    (nth 2 b)))
      (and (string= (nth 5 a)
                    (nth 5 b))
           (string= (nth 2 a)
                    (nth 2 b))
           (string< (nth 4 a)
                    (nth 4 b)))))

(defun counsel-mu4e-and-bbdb-addresses--extract-bbdb-contact (c)
  "Extract information from an BBDB contact C."
  (let* ((group (nth 5 c))
         (email (nth 4 c))
         (lastname (nth 1 c))
		 (name (format "%s %s" (nth 2 c) (nth 1 c)))
         (sortname (if name (downcase name) email))
		 (fullcontact (format "%s <%s>" name email))
		 (contact `(:full-contact ,fullcontact :name ,name :email ,email :sort-name ,sortname :last-name ,lastname :contact-from "bbdb" :group ,group)))
	contact))

(defun counsel-mu4e-and-bbdb-addresses--extract-mu4e-contact  (c)
  "Extract information from mu4e contact C."
  (let* ((email (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address c))
		 (name (counsel-mu4e-and-bbdb-addresses-mu4e-extract-name-from-contact c))
         (sortname (if name (downcase name) email))
		 (contact `(:full-contact ,c :name ,name :email ,email :sort-name ,sortname :contact-from "mu4e"  :group "MU4E")))
                          contact))

(defun counsel-mu4e-and-bbdb-addresses--transform-to-bbdb (c)
  "Transform our contact C format to BBDB contacts."
  (let* ((name (plist-get c :name))
         (email (plist-get c :email))
         (group (plist-get c :group))
         (fullcontact (format "%s:%s => %s"
                              name
                              email
                              group)))
    `(,fullcontact ,name nil nil ,email ,group)))

(defun counsel-mu4e-and-bbdb-addresses--transform-to-mu4e (c)
  "Transform out contact format C to mu4e."
  (let* ((fullcontact (plist-get c :full-contact))
         (key (if (string= (plist-get c :contact-from) "mu4e")
                  fullcontact
                (concat fullcontact " #"))))
    (cons key c)))

(defun counsel-mu4e-and-bbdb-addresses--transform-to-mu4e-search-completion (c)
  "Transform contact C for mu4e-search completion."
  (let ((fullcontact (plist-get c :full-contact))
        (email (plist-get c :email)))
    (propertize fullcontact :email email)))

(defun counsel-mu4e-and-bbdb-addresses-create-contacts-list-from-mu4e-and-bbdb (&optional force)
  "Create a hash-table storing contacts from mu4e and bbdb.

This is used for address book and email address look-ups. If
FORCE is non-nil, then regenerate the views from scratch."
  ;; update bbdb and mu4e contacts if need be
  (when (eq counsel-bbdb-contacts nil)
	(counsel-bbdb-reload))
  (unless (counsel-mu4e-and-bbdb-addresses-get-mu4e-contacts)
	(counsel-mu4e-and-bbdb-addresses-ensure-mu4e-contacts))
  ;; create materialized views
  (unless (and counsel-mu4e-and-bbdb-addresses--counsel-all-mv
               counsel-mu4e-and-bbdb-addresses--counsel-bbdb-mv
               (not force))
    (counsel-mu4e-and-bbdb-addresses--log "FETCH CONTACTS AND MATERIALIZE AS VIEWS: %s" (current-time-string))
    (let ((allcontacts (idf-union
            ;; create bbdb contacts
            (->
             (idf-create-source :name :bbdb-contacts :content counsel-bbdb-contacts)
             (idf-filter :fn (lambda (c) (not (string-prefix-p "@Conflict" (nth 0 c)))))
             (idf-map #'counsel-mu4e-and-bbdb-addresses--extract-bbdb-contact))
            ;; create mu4e contacts
            (->
             (idf-create-source
              :name :mu4e-contacts
              :content (ht-keys (counsel-mu4e-and-bbdb-addresses-get-mu4e-contacts)))
             (idf-map #'counsel-mu4e-and-bbdb-addresses--extract-mu4e-contact)))))
      (counsel-mu4e-and-bbdb-addresses--log "MATERIALIZED allcontacts AS VIEW: %s" (current-time-string))
      ;; create counsel-bbdb version of our contacts too
      (setq counsel-mu4e-and-bbdb-addresses--counsel-bbdb-mv
            (->
             allcontacts
             (idf-map #'counsel-mu4e-and-bbdb-addresses--transform-to-bbdb)
             (idf-materialize-as :viewtype 'sortedlist
                                 :comparefn #'counsel-mu4e-and-bbdb-addresses--contact-bbdb-less-p)))
      (counsel-mu4e-and-bbdb-addresses--log "MATERIALIZED counsel-bbdb contact format AS VIEW: %s" (current-time-string))
      ;; create view of sorted contacts from bbdb and mu4e
      (setq counsel-mu4e-and-bbdb-addresses--counsel-all-mv
            (->
             allcontacts
             (idf-map #'counsel-mu4e-and-bbdb-addresses--transform-to-mu4e)
             (idf-materialize-as
              :viewtype 'sortedlist
              :comparefn #'counsel-mu4e-and-bbdb-addresses--contact-less-p)))
      (counsel-mu4e-and-bbdb-addresses--log "MATERIALIZED mu4e contact format AS VIEW: %s" (current-time-string))
      ;; create view storing propertized strings for mu4e seach completion
      (setq counsel-mu4e-and-bbdb-addresses--mu4e-search-completion-mv
            (->
             allcontacts
             (idf-map #'counsel-mu4e-and-bbdb-addresses--transform-to-mu4e-search-completion)
             (idf-materialize-as
              :viewtype 'sortedlist
              :comparefn #'string<
              :maintain-as-symbol 'mu4e-search-company-completion-contacts)))
      (counsel-mu4e-and-bbdb-addresses--log "MATERIALIZED mu4e-search completion contact format AS VIEW: %s" (current-time-string))
      ;; install hooks for incremental maintenance
      (counsel-mu4e-and-bbdb-addresses--install-maintenance-hooks))))

;;;###autoload
(defun counsel-mu4e-and-bbdb-full-contacts-sorted ()
  "Return sorted contacts.

Contacts are sorted based on the order recoded in
`counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended'."
  (counsel-mu4e-and-bbdb-addresses-create-contacts-list-from-mu4e-and-bbdb)
  (idf-mv-get-result counsel-mu4e-and-bbdb-addresses--mu4e-search-completion-mv)
  ;; (sort (ht-map (lambda (e c)
  ;;   			  (propertize (plist-get c :full-contact) :email e :pos (plist-get c :pos)))
  ;;   			counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended)
  ;;   	(lambda (l r) (<= (get-text-property 0 :pos l) (get-text-property 0 :pos r))))
  )

(defun counsel-mu4e-and-bbdb-addresses-get-sorted-contacts ()
  "Return sorted contacts.

Contacts are sorted by
`counsel-mu4e-and-bbdb-addresses-create-contacts-list-from-mu4e-and-bbdb'."
  (counsel-mu4e-and-bbdb-addresses-create-contacts-list-from-mu4e-and-bbdb)
  (idf-mv-get-result counsel-mu4e-and-bbdb-addresses--counsel-all-mv))

;; ********************************************************************************
;; hooks for incremental maintenance
(defun counsel-mu4e-and-bbdb-addresses--install-maintenance-hooks ()
  "Install hooks for incrementally maintaining our concact information.

These hooks trigger when BBDB or mu4e contacts change."
  (add-hook 'bbdb-change-hook
            #'counsel-mu4e-and-bbdb-addresses-bbdb-changed-hook)
  (add-hook 'bbdb-create-hook
            #'counsel-mu4e-and-bbdb-addresses-bbdb-create-hook)
  (add-hook 'mu4e-index-updated-hook
            #'counsel-mu4e-and-bbdb-addresses-mu4e-index-updated-hook))

(defun counsel-mu4e-and-bbdb-addresses-diff-mu4e-contacts ()
  "Determine which mu4e contacts have changed and create a delta for them."
  (let ((ins nil)
        (del nil)
        (newht (counsel-mu4e-and-bbdb-addresses-get-mu4e-contacts))
        (oldht counsel-mu4e-and-bbdb-addresses-mu4e-previous-contacts))
    (if (not oldht)
        (progn
          (setq counsel-mu4e-and-bbdb-addresses-mu4e-previous-contacts
                (ht-copy
                 (counsel-mu4e-and-bbdb-addresses-get-mu4e-contacts)))
          nil)
      ;; ins
      (ht-map (lambda (k v)
                (unless (equal (ht-get oldht k) v)
                  (push (cons k v) ins)))
              newht)
      ;; del
      (ht-map (lambda (k v)
                (unless (equal (ht-get newht k) v)
                  (push k del)))
              oldht)
      ;; updated HT
      (dolist (d del)
        (ht-remove oldht d))
      (dolist (i ins)
        (puthash (car i) (cdr i) oldht))
      (idf-delta-create
       :inserted (--map (car it) ins)
       :deleted del))))

(defun counsel-mu4e-and-bbdb-addresses-mu4e-index-updated-hook ()
  "After mu4e index update, maintain contacts for completion."
  (counsel-mu4e-and-bbdb-addresses--log "UPDATING CONTACTS FROM MU4E: %s"
                                        (current-time-string))
  (let ((delta (counsel-mu4e-and-bbdb-addresses-diff-mu4e-contacts)))
    (counsel-mu4e-and-bbdb-addresses--log "DETERMINED DELTA: have %s deleted / inserted records: %s"
                                          (idf-delta-size delta)
                                          (current-time-string))
    (idf-maintain-multiple `(,counsel-mu4e-and-bbdb-addresses--counsel-bbdb-mv
                             ,counsel-mu4e-and-bbdb-addresses--counsel-all-mv
                             ,counsel-mu4e-and-bbdb-addresses--mu4e-search-completion-mv)
                           `(:mu4e-contacts ,delta))
    (counsel-mu4e-and-bbdb-addresses--log "FINISHED APPLYING DELTA: %s" (current-time-string))
    (setq counsel-bbdb-contacts
          (idf-mv-get-result counsel-mu4e-and-bbdb-addresses--counsel-bbdb-mv))))

(defun counsel-mu4e-and-bbdb-addresses-bbdb-record-to-contacts (r)
  "Take bbdb record R and turn it into our contact format."
  (let ((firstname (aref r 1))
        (lastname (aref r 0))
        (emails (aref r 7))
        (mailaliases (alist-get 'mail-alias (aref r 8))))
    (--map (let ((full-contact (format "%s%s:%s => %s"
                                       (or firstname "")
                                       (or lastname "")
                                       it
                                       (or mailaliases "BBDB"))))
             `(,full-contact ,lastname ,firstname nil ,it ,mailaliases))
           emails)))

(defun counsel-mu4e-and-bbdb-addresses-bbdb-create-hook (changedr)
  "Cache a newly created BBDB record CHANGEDR."
  (setq counsel-mu4e-and-bbdb-addresses-bbdb-new-contact-cache
        changedr)
  (counsel-mu4e-and-bbdb-addresses--log "CREATED NEW BBDB CONTACT: %s" changedr))

(defun counsel-mu4e-and-bbdb-addresses-bbdb-changed-hook (changedr)
  "Run this hook when BBDB record CHANGEDR changed.

These hook incrementally maintain our views."
  (let ((ins nil)
        (del nil))
    (if counsel-mu4e-and-bbdb-addresses-bbdb-new-contact-cache
        (setq ins (counsel-mu4e-and-bbdb-addresses-bbdb-record-to-contacts changedr))
      (setq ins (counsel-mu4e-and-bbdb-addresses-bbdb-record-to-contacts changedr)))
    ;; TODO deal with deletion which does not call a hook and update where we need to find the old version and delete it
    (setq counsel-mu4e-and-bbdb-addresses-bbdb-new-contact-cache nil)
    (counsel-mu4e-and-bbdb-addresses--log "CHANGED BBDB CONTACT: %s\nSTART MAINTENANCE AT: %s"
                                          changedr
                                          (current-time-string))
    (idf-maintain-multiple `(,counsel-mu4e-and-bbdb-addresses--counsel-bbdb-mv
                             ,counsel-mu4e-and-bbdb-addresses--counsel-all-mv
                             ,counsel-mu4e-and-bbdb-addresses--mu4e-search-completion-mv)
                  `(:bbdb-contacts ,(idf-delta-create :inserted ins :deleted del)))
    (counsel-mu4e-and-bbdb-addresses--log "CHANGED BBDB CONTACT: %s\nDELTA: %s\nFINISHED AT: %s"
                                          changedr
                                          (idf-delta-create :inserted ins :deleted del)
                                          (current-time-string))))

;; ********************************************************************************
;; functions for user address search and completion

(defun counsel-mu4e-and-bbdb-addresses-mu4e~compose-complete-handler (str pred action)
  "Complete address STR with predication PRED for ACTION."
  (unless counsel-mu4e-and-bbdb-addresses-mu4e-contacts
	(counsel-mu4e-and-bbdb-addresses-create-contacts-list-from-mu4e-and-bbdb))
  (cond
   ((eq action nil)
	(try-completion str counsel-mu4e-and-bbdb-addresses-mu4e-contacts pred))
   ((eq action t)
	(all-completions str counsel-mu4e-and-bbdb-addresses-mu4e-contacts pred))
   ((eq action 'metadata)
	;; our contacts are already sorted - just need to tell the
	;; completion machinery not to try to undo that...
	'(metadata
	  (display-sort-function . identity)
	  (cycle-sort-function   . identity)))))

;; searches through contacts with ivy
;;;###autoload
(defun counsel-mu4e-and-bbdb-addresses-mu4e-contacts ()
  "Select a contact from mu4e or BBDB via counsel.

Default action is to show emails from the selected contact."
  (interactive)
  (ivy-read "Contact:"
			(counsel-mu4e-and-bbdb-addresses-get-sorted-contacts)
			:history 'counsel-mu4e-and-bbdb-addresses-mu4e-contacts-history
			:action '(1
					  ("a" (lambda (add)
							 (mu4e-headers-search (counsel-mu4e-and-bbdb-addresses-mu4e-get-all-emails-from-person add)))
					   "show all emails from all email addresses for this person")
					  ("i" (lambda (add)
							 (let ((nohash (replace-regexp-in-string "[ ]*#" "" (car add))))
							   (insert nohash)))
                       "insert")
					  ("w" (lambda (add)
							 (let ((nohash (replace-regexp-in-string "[ ]*#" "" (car add))))
							   (kill-new nohash)))
                       "copy to killring")
					  ("I" (lambda (add)
							 (insert (counsel-mu4e-and-bbdb-addresses--get-email add)))
                       "insert email address only")
					  ("W" (lambda (add)
							 (kill-new (counsel-mu4e-and-bbdb-addresses--get-email add)))
                       "copy to email address only to killring")
					  ("l" (lambda (add)
							 (let* ((email (counsel-mu4e-and-bbdb-addresses--get-email add))
								    (query (concat "from:" email " OR " "to:" email)))
							   (mu4e-headers-search query)))
					   "show email from/to this person")
					  ("f" (lambda (add)
							 (let* ((email (counsel-mu4e-and-bbdb-addresses--get-email add))
								    (query (concat "from:" email)))
							   (mu4e-headers-search query)))
					   "show emails from this person")
					  ("c" (lambda (add)
							 (let* ((email (counsel-mu4e-and-bbdb-addresses--get-email add)))
							   (mu4e~compose-mail email))
							 (mu4e-headers-search query))
					   "send an email to person")
					  ("b" (lambda (add)
							 (let* ((email (counsel-mu4e-and-bbdb-addresses--get-email add)))
							   ;; 4th entry is the email address (counsel-bbdb format)
							   (counsel-mu4e-and-bbdb-addresses-counsel-bbdb-open-in-bbdb-action `(nil nil nil nil ,email))))
					   "open record in BBDB"))
			:caller 'counsel-mu4e-and-bbdb-addresses-mu4e-contacts))

;;;###autoload
(defun counsel-mu4e-and-bbdb-addresses-mu4e-send-mail-to-contact ()
  "Select a contact from mu4e or BBDB via counsel.

Default action is to show emails from the selected contact."
  (interactive)
  (ivy-read "Send email to contact: "
			(counsel-mu4e-and-bbdb-addresses-get-sorted-contacts)
			:history 'counsel-mu4e-and-bbdb-addresses-mu4e-contacts-history
			:action (lambda (add)
							 (let* ((email (counsel-mu4e-and-bbdb-addresses--get-email add)))
							   (mu4e~compose-mail email))
							 (mu4e-headers-search query))
			:caller 'counsel-mu4e-and-bbdb-addresses-mu4e-send-mail-to-contact))

;;;###autoload
(defun counsel-mu4e-and-bbdb-addresses-setup ()
  "Advice mu4e contacts."
  (require 'counsel-bbdb)
  (advice-tools/advice-add-if-def
   'mu4e~compose-complete-handler
   :override
   'counsel-mu4e-and-bbdb-addresses-mu4e~compose-complete-handler)
  (advice-tools/advice-add-if-def
   'counsel-bbdb-complete-mail
   :override
   'counsel-mu4e-and-bbdb-addresses-mu4e-counsel-bbdb-complete-mail)
  (advice-tools/advice-add-if-def
   'counsel-bbdb-expand-mail-alias
   :override
   'counsel-mu4e-and-bbdb-addresses-counsel-bbdb-expand-mail-alias)
  (advice-tools/advice-add-if-def
   'counsel-bbdb
   :override
   'counsel-mu4e-and-bbdb-addresses-counsel-bbdb)
  (advice-tools/advice-add-if-def
   'counsel-bbdb-expand-mail-alias
   :override
   'counsel-mu4e-and-bbdb-addresses-counsel-bbdb-expand-mail-alias)
  (advice-tools/advice-add-if-def
   'counsel-bbdb-reload
   :override
   'counsel-mu4e-and-bbdb-addresses-counsel-bbdb-reload)
  ;; allow reloading and opening people in bbdb
  (ivy-add-actions 'counsel-bbdb-complete-mail
                   '(("b" counsel-mu4e-and-bbdb-addresses-counsel-bbdb-open-in-bbdb-action "open in BBDB")
                     ("r" counsel-bbdb-reload "reload from BBDB file")))

  ;; allow reloading and opening people in bbdb
  (ivy-add-actions 'counsel-bbdb-complete-mail
                   '(("b" counsel-mu4e-and-bbdb-addresses-counsel-bbdb-open-in-bbdb-action "open in BBDB")
                     ("r" counsel-bbdb-reload "reload from BBDB file"))))

;;;###autoload
(defun counsel-mu4e-and-bbdb-addresses-remove ()
  "Advice mu4e contacts."
  (advice-tools/advice-remove-if-def
   'mu4e~compose-complete-handler
   'counsel-mu4e-and-bbdb-addresses-mu4e~compose-complete-handler)
  (advice-tools/advice-remove-if-def
   'counsel-bbdb-complete-mail
   'counsel-mu4e-and-bbdb-addresses-mu4e-counsel-bbdb-complete-mail)
  (advice-tools/advice-remove-if-def
   'counsel-bbdb-expand-mail-alias
   'counsel-mu4e-and-bbdb-addresses-counsel-bbdb-expand-mail-alias)
  (advice-tools/advice-remove-if-def
   'counsel-bbdb
   'counsel-mu4e-and-bbdb-addresses-counsel-bbdb)
  (advice-tools/advice-remove-if-def
   'counsel-bbdb-expand-mail-alias
   'counsel-mu4e-and-bbdb-addresses-counsel-bbdb-expand-mail-alias)
  (advice-tools/advice-remove-if-def
   'counsel-bbdb-reload
   'counsel-mu4e-and-bbdb-addresses-counsel-bbdb-reload))

(provide 'counsel-mu4e-and-bbdb-addresses)
;;; counsel-mu4e-and-bbdb-addresses.el ends here
