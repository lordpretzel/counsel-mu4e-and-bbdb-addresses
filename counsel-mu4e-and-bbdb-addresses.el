;;; counsel-mu4e-and-bbdb-addresses.el --- Counsel addressbook search using mu4e contacts and bbdb. -*- lexical-binding: t -*-

;; Author: Boris Glavic <lordpretzel@gmail.com>
;; Maintainer: Boris Glavic <lordpretzel@gmail.com>
;; Version: 0.1
;; Package-Requires: ((advice-tools "0.1") (dash "2.12.0") (counsel-bbdb "20181128.1320") (bbdb "3.2") (ht "20201119.518"))
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

;;

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

;; ********************************************************************************
;; CUSTOM
(defcustom counsel-mu4e-and-bbdb-addresses-counsel-bbdb-only-use-primary-email-for-groups
  t
  "Use only primary email address for persons when expanding all email addresses for a group?."
  :group 'counsel-mu4e-and-bbdb-addresses
  :type 'boolean
  )

;; ********************************************************************************
;; VARIABLES
(defvar counsel-mu4e-and-bbdb-addresses-mu4e-contacts-history nil
  "Stores history of contact completions with counsel-mu4e-and-bbdb-addresses-mu4e-contacts.")

(defvar counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended nil
  "HT that stores mu4e~contexts and bbdb contacts for mu4e completion.")

(defvar counsel-mu4e-and-bbdb-addresses-mu4e-contacts nil
  "Replaces `mu4e~contacts'.")

(defvar counsel-mu4e-and-bbdb-addresses-counsel-bbdb-contacts nil
  "Extends `counsel-bbdb-contacts'."
  )

;; ********************************************************************************
;; FUNCTIONS

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

;; counsel-bbdb insert mail
(defun counsel-mu4e-and-bbdb-addresses-counsel-bbdb-insert-email (r &optional append-comma)
  "Function to insert email address R selected from counsel-bbdb.  If APPEND-COMMA then append a comma."
  (interactive)
  (let* ((points (bounds-of-thing-at-point 'symbol)))
    (when points (delete-region (car points) (cdr points))))
  (counsel-bbdb-insert-one-mail-address r append-comma)
  )

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
                                         :name-lf family-name)
                            )
      )
    (when (and family-name (not mail))
      (bbdb-display-records (bbdb-search (bbdb-records)
                                         :name-lf family-name)
                            )
      )
    (when (and (not family-name) mail)
      (bbdb-display-records (bbdb-search (bbdb-records)
                                         :mail mail)
                            )
      )
    )
  )

;; add receipient to TO , CC, or BCC with counsel-bbdb completion
(defun counsel-mu4e-and-bbdb-addresses-counsel-bbdb-add (&optional where)
  "Add receipient to `WHERE' (symbol: one of `to', `cc', or `bcc') with counsel-bbdb."
  (interactive)
  (let ((where (or where
                   (intern (ivy-read "Add to: "
                                     '("to" "cc" "bcc")
                                     :require-match t
                                     :preselect "to"
                                     ))
                   )
               ))
    (pcase where
      ('to (message-goto-to))
      ('cc (message-goto-cc))
      ('bcc (message-goto-bcc))
      (_ (error "Only to, cc, bcc are allowed options for inserting receipient"))
      )
    )
  (counsel-bbdb-complete-mail t)
  )

;; replacement functions for counsel-bbdb
(defun counsel-mu4e-and-bbdb-addresses-mu4e-counsel-bbdb-complete-mail (&optional append-comma)
  "Select person from BBDB to insert into email as to/cc/bcc.  Complete email before point.   Extra argument APPEND-COMMA will append comma after email."
  (interactive "P")
  (unless counsel-bbdb-contacts
    (counsel-bbdb-reload))
  (ivy-read "Contacts: "
            counsel-mu4e-and-bbdb-addresses-counsel-bbdb-contacts
            :initial-input (or (thing-at-point 'symbol) "")
            :caller 'counsel-bbdb-complete-mail
            :action (lambda (r) (counsel-mu4e-and-bbdb-addresses-counsel-bbdb-insert-email r append-comma))
            )
  )

(defun counsel-mu4e-and-bbdb-addresses-counsel-bbdb ()
  "Lookup people in BBDB."
  (interactive)
  (unless counsel-bbdb-contacts
    (counsel-bbdb-reload))
  (ivy-read "Contacts: "
            counsel-mu4e-and-bbdb-addresses-counsel-bbdb-contacts
            ;; lordpretzel-counsel-bbdb-contacts
            :initial-input (or (thing-at-point 'symbol) "")
            :caller 'counsel-bbdb-complete-mail
            :action '(1
                      ("b" lordpretzel/counsel-bbdb-open-in-bbdb-action "open in BBDB")
                      ("r" counsel-bbdb-reload "reload from BBDB file")
                      )
            )
  )

(defun counsel-mu4e-and-bbdb-addresses-counsel-bbdb-expand-mail-alias ()
  "Insert multiple mail address in alias/group."
  (interactive)
  (unless counsel-bbdb-contacts
    (counsel-bbdb-reload))
  ;; We just need filter the `counsel-bbdb-contacts' by selected alias
  (let* ((alias (ivy-read "Alias: "
                          counsel-bbdb-mail-alias-list
                          :caller 'counsel-bbdb-expand-mail-alias
                          ))
         (names nil)
         )
    (when alias
      (dolist (r counsel-bbdb-contacts)
        (let* ((r-alias (nth 4 (cdr r)))
               (name (concat (nth 1 (cdr r)) (nth 2 (cdr r))))
               )
          (when (and r-alias
                     (string-match-p (format "%s\\(,\\| *$\\)" alias) r-alias))
            (unless (member name names)
              (counsel-bbdb-insert-one-mail-address r t)
              (push name names)
              (message "%s names" names)
              )))))))

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
  "Extract email address from mu4e contact ADD which may use one of two formats: 'email' or 'name <email>'."
  (save-match-data
	(if (string-match "<\\([^>]+\\)>" add)
		(match-string 1 add)
	  add)))

(defun counsel-mu4e-and-bbdb-addresses-mu4e-extract-name-from-contact (add)
  "Extract name from a mu4e-contact ADD if it of the form 'name <email>', return nil if this is only an email address."
  (save-match-data
	(if (string-match "\\([^<]+\\)<\\([^>]+\\)>" add)
		(string-trim (match-string 1 add))
	  nil)))

;; return name or "" if there is no name
(defun counsel-mu4e-and-bbdb-addresses-mu4e-extract-name-or-emtpy-string (add)
  "Extract name from a mu4e-contact ADD if it of the form 'name <email>', return empty string if this is only an email address."
  (let ((name (counsel-mu4e-and-bbdb-addresses-mu4e-extract-name-from-contact add)))
	(if name
		name
	  "")))

;; find all email addressses for mu4e person (TODO do smarter matching)
(defun counsel-mu4e-and-bbdb-addresses-mu4e-get-all-email-addresses-for-person (add)
  "Find all emails from a given mu4e-contact ADD.  If the contact is of the form 'name <email>', then also return emails send from contacts with the same name but different email address."
  (let ((person (counsel-mu4e-and-bbdb-addresses-mu4e-extract-name-from-contact add))
		(emails (list (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address add))))
	(counsel-mu4e-and-bbdb-addresses-ensure-mu4e-contacts)
	(when person
	  (setq emails (append emails (cl-remove-duplicates (mapcar (lambda (a) (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address a)) (all-completions person mu4e~contacts))))))
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

(defun counsel-mu4e-and-bbdb-addresses-ensure-mu4e-contacts ()
  (unless mu4e~contacts
	(mu4e~request-contacts-maybe)
	(let ((prevcount 0))
	  (while (not mu4e~contacts)
            (sleep-for 0 100)	    
	    )
            (sleep-for 0 100)	   
	  ;; this is async, so we have to poll
      (while (not (eq (hash-table-count mu4e~contacts) prevcount))
        (sleep-for 0 100)
        (setq prevcount (hash-table-count mu4e~contacts))))))

(defun counsel-mu4e-and-bbdb-addresses-create-contacts-list-from-mu4e-and-bbdb ()
  "Create a hash-table storing contacts from mu4e and bbdb for address book and email address look-ups."
  ;; create ht if it does not exist
  (when (eq counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended nil)
	(setq counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended (ht-create 'equal)))
  (when (eq counsel-mu4e-and-bbdb-addresses-mu4e-contacts nil)
	(setq counsel-mu4e-and-bbdb-addresses-mu4e-contacts (ht-create 'equal)))
  ;; update bbdb contacts if need be
  (when (eq counsel-bbdb-contacts nil)
	(counsel-bbdb-reload))
  (unless mu4e~contacts
	(counsel-mu4e-and-bbdb-addresses-ensure-mu4e-contacts))
  (let ((pos 1)
		;;(bbdb-maxpos (length counsel-bbdb-contacts))
        )
	;; insert bbdb contacts
	(mapc (lambda (c)
			(let* ((email (nth 4 c))
				   (name (format "%s %s" (nth 2 c) (nth 1 c)))
				   (fullcontact (format "%s <%s>" name email))
				   (contact `(:full-contact ,fullcontact :name ,name :contact-from "bbdb" :pos ,pos)))
			  (cl-incf pos)
			  (puthash email contact counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended)))
		  (sort
		   (--filter (not (string-prefix-p "@Conflict" (nth 0 it))) (copy-sequence counsel-bbdb-contacts))
		   (lambda (l r)
			 (or (and (nth 1 r) (string-prefix-p (nth 1 r) "@"))
				 (if (string= (nth 1 l) (nth 1 r))
					 (if (string= (nth 2 l) (nth 2 r))
						 (string-lessp (nth 4 l) (nth 4 r))
					   (or (not (nth 2 r))
						   (string-lessp (nth 2 l) (nth 2 r))))
				   (or (not (nth 1 r))
					   (string-lessp (nth 1 l) (nth 1 r))))))))
	;; insert mu4e contacts
	(mapc (lambda (c)
			(let* ((email (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address c))
				   (name (counsel-mu4e-and-bbdb-addresses-mu4e-extract-name-from-contact c))
				   (contact `(:full-contact ,c :name ,name :contact-from "mu4e" :pos ,pos)))
			  (unless (ht-contains-p counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended email)
				(cl-incf pos)
				(puthash email contact counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended))))
		  (ht-keys mu4e~contacts))
	;; put in hashtable for mu4e
	(mapc (lambda (c)
			(puthash (plist-get c :full-contact) (plist-get c :pos) counsel-mu4e-and-bbdb-addresses-mu4e-contacts))
		  (ht-values counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended))
	;; create list for counsel-bbdb
	(setq counsel-mu4e-and-bbdb-addresses-counsel-bbdb-contacts
		  (cl-union
		   counsel-bbdb-contacts
		   (mapcar (lambda (c)
					 (let* ((nameemail (replace-regexp-in-string "[>]" "" (replace-regexp-in-string " [<]" ":" (plist-get (cadr c) :full-contact))))
							(name  (plist-get (cadr c) :name))
							(email (car c))
							(fullcontact (concat nameemail " => MU4E"))
							)
					   `(,fullcontact ,name nil nil ,email nil)
					   ))
				   (--filter (string= (plist-get (cadr it) :contact-from) "mu4e")
							 (ht-map (lambda (k v) (list k v)) counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended)))))))

;;;###autoload
(defun counsel-mu4e-and-bbdb-full-contacts-sorted ()
  "Return contacts sorted based on the order recoded in `counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended'."
  (counsel-mu4e-and-bbdb-addresses-create-contacts-list-from-mu4e-and-bbdb)
  (sort (ht-map (lambda (e c)
				  (propertize (plist-get c :full-contact) :email e :pos (plist-get c :pos)))
				counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended)
		(lambda (l r) (<= (get-text-property 0 :pos l) (get-text-property 0 :pos r)))))

(defun counsel-mu4e-and-bbdb-addresses-get-sorted-contacts ()
  "Return contacts sorted based on the order recoded in `counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended'."
  (counsel-mu4e-and-bbdb-addresses-create-contacts-list-from-mu4e-and-bbdb)
  (mapcar 'car (sort (ht-map (lambda (e c)
                               (ignore e)
							   (cons (if (string= (plist-get c :contact-from) "bbdb")
									     (concat (plist-get c :full-contact) " #")
								       (plist-get c :full-contact))
								     (plist-get c :pos)))
							 counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended)
					 (lambda (l r) (<= (cdr l) (cdr r))))))

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
  "Select a contact from mu4e or BBDB via counsel.  Default action is to show emails from the selected contact."
  (interactive)
  (ivy-read "Contact:"
			(counsel-mu4e-and-bbdb-addresses-get-sorted-contacts)
			:history 'counsel-mu4e-and-bbdb-addresses-mu4e-contacts-history
			:action '(1
					  ("a" (lambda (add)
							 (mu4e-headers-search (counsel-mu4e-and-bbdb-addresses-mu4e-get-all-emails-from-person add)))
					   "show all emails from all email addresses for this person")
					  ("i" (lambda (add)
							 (let ((nohash (replace-regexp-in-string "[ ]*#" "" add)))
							   (insert nohash)))
                       "insert")
					  ("w" (lambda (add)
							 (let ((nohash (replace-regexp-in-string "[ ]*#" "" add)))
							   (kill-new nohash)))
                       "copy to killring")
					  ("I" (lambda (add)
							 (insert (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address add)))
                       "insert email address only")
					  ("W" (lambda (add)
							 (kill-new (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address add)))
                       "copy to email address only to killring")
					  ("l" (lambda (add)
							 (let* ((email (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address add))
								    (query (concat "from:" email " OR " "to:" email)))
							   (mu4e-headers-search query)))
					   "show email from/to this person")
					  ("f" (lambda (add)
							 (let* ((email (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address add))
								    (query (concat "from:" email)))
							   (mu4e-headers-search query)))
					   "show emails from this person")
					  ("c" (lambda (add)
							 (let* ((email (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address add)))
							   (mu4e~compose-mail email))
							 (mu4e-headers-search query))
					   "send an email to person")
					  ("b" (lambda (add)
							 (let* ((email (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address add)))
							   ;; 4th entry is the email address (counsel-bbdb format)
							   (counsel-mu4e-and-bbdb-addresses-counsel-bbdb-open-in-bbdb-action `(nil nil nil nil ,email))))
					   "open record in BBDB"))
			:caller 'counsel-mu4e-and-bbdb-addresses-mu4e-contacts))

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
                     ("r" counsel-bbdb-reload "reload from BBDB file")))
  )

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
