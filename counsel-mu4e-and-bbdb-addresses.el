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
(require 'advice-tools)
(require 'cl-lib)
(require 'counsel-bbdb)
(require 'bbdb)
(require 'dash)
(require 'ht)

;; ********************************************************************************
;; CUSTOM

;; ********************************************************************************
;; VARIABLES

;; variable store history in
(defvar counsel-mu4e-and-bbdb-addresses-mu4e-contacts-history nil
  "Stores history of contact completions with counsel-mu4e-and-bbdb-addresses-mu4e-contacts.")

;; HT to store contacts
(defvar counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended nil
  "HT that stores mu4e~contexts and bbdb contacts for mu4e completion.")

;; HT to replace mu4e~contacts
(defvar counsel-mu4e-and-bbdb-addresses-mu4e-contacts nil
  "Replaces `mu4e~contacts'.")

;; contacts-bbdb replacement that also has mu4e contacts
(defvar counsel-mu4e-and-bbdb-addresses-counsel-bbdb-contacts nil
  "Extends `counsel-bbdb-contacts'."
  )

;; ********************************************************************************
;; FUNCTIONS
;; add bbdb contacts to mu4e

;; extract email address from mu4e contact
(defun counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address (add)
  "Extract email address from mu4e contact which may use one of two formats: 'email' or 'name <email>'."
  (save-match-data
	(if (string-match "<\\([^>]+\\)>" add)
		(match-string 1 add)
	  add)))

;; returns name if mu4e-contact is of form 'name <email>'
(defun counsel-mu4e-and-bbdb-addresses-mu4e-extract-name-from-contact (add)
  "Extract name from a mu4e-contact if it of the form 'name <email>', return `nil' if this is only an email address."
  (save-match-data
	(if (string-match "\\([^<]+\\)<\\([^>]+\\)>" add)
		(string-trim (match-string 1 add))
	  nil)))

;; return name or "" if there is no name
(defun counsel-mu4e-and-bbdb-addresses-mu4e-extract-name-or-emtpy-string (add)
  "Extract name from a mu4e-contact if it of the form 'name <email>', return empty string if this is only an email address."
  (let ((name (counsel-mu4e-and-bbdb-addresses-mu4e-extract-name-from-contact add)))
	(if name
		name
	  "")))

;; find all email addressses for mu4e person (TODO do smarter matching)
(defun counsel-mu4e-and-bbdb-addresses-mu4e-get-all-email-addresses-for-person (add)
  "Find all emails from a given mu4e-contact. If the contact is of the form 'name <email>', then also return emails send from contacts with the same name but different email address."
  (let ((person (counsel-mu4e-and-bbdb-addresses-mu4e-extract-name-from-contact add))
		(emails (list (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address add))))
	(mu4e~request-contacts-maybe)
	(when person
	  (setq emails (append emails (cl-remove-duplicates (mapcar (lambda (a) (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address a)) (all-completions person mu4e~contacts))))))
	emails))

;; create query for returnning all emails send from person x (retrieving all email addressed for person from mu4e~contacts)
(defun counsel-mu4e-and-bbdb-addresses-mu4e-get-all-emails-from-person (add)
  "return a mu4e query to retrieve all emails send from a person."
  (concat "(maildir:\"/lordpretzel-gmail/[lordpretzel].All Mail\" OR maildir:\"/bglavic-iit/[bglavic].All Mail\" OR maildir:/bglavic-iit/INBOX OR maildir:/lordpretzel-gmail/INBOX) AND ("
		  (mapconcat (lambda (x) (concat "from:" x)) (counsel-mu4e-and-bbdb-addresses-mu4e-get-all-email-addresses-for-person add) " OR ") ")"))

(defun counsel-mu4e-and-bbdb-addresses-mu4e-extract-is-org-from-address (add)
  "return * if `add' ends in # (an org-contact contact) and space otherwise"
  (if (string-match "[#]$" add)
	  "*"
	"-"))

(defun counsel-mu4e-and-bbdb-addresses-create-contacts-list-from-mu4e-and-bbdb ()
  "Creates a hash-table storing contacts from mu4e and bbdb for address book and email address look-ups."
  ;; create ht if it does not exist
  (when (eq counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended nil)
	(setq counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended (ht-create 'equal)))
  (when (eq counsel-mu4e-and-bbdb-addresses-mu4e-contacts nil)
	(setq counsel-mu4e-and-bbdb-addresses-mu4e-contacts (ht-create 'equal)))
  ;; update bbdb contacts if need be
  (when (eq counsel-bbdb-contacts nil)
	(counsel-bbdb-reload))
  (unless mu4e~contacts
	(mu4e~request-contacts-maybe))
  (let ((pos 1)
		;;(bbdb-maxpos (length counsel-bbdb-contacts))
        )
	;; insert bbdb contacts
	(mapc (lambda (c)
			(let* ((email (nth 4 c))
				   (name (format "%s %s" (nth 2 c) (nth 1 c)))
				   (fullcontact (format "%s <%s>" name email))
				   (contact `(:full-contact ,fullcontact :name ,name :contact-from "bbdb" :pos ,pos)))
			  (incf pos)
			  (puthash email contact counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended)))
		  (sort
		   (--filter (not (string-prefix-p "@Conflict" (nth 0 it))) (copy-sequence counsel-bbdb-contacts))
		   (lambda (l r)
			 (or (and (nth 1 r) (string-prefix-p (nth 1 r) "@"))
				 (if (string= (nth 1 l) (nth 1 r))
					 (if (string= (nth 2 l) (nth 2 r))
						 (string-lessp (nth 4 l) (nth 4 r))
					   (or (not (nth 2 r))
						   (string-lessp (nth 2 l) (nth 2 r)))
					   )
				   (or (not (nth 1 r))
					   (string-lessp (nth 1 l) (nth 1 r))
					   )
				   )
				 )
			 )

		   )
		  )
	;; insert mu4e contacts
	(mapc (lambda (c)
			(let* ((email (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address c))
				   (name (counsel-mu4e-and-bbdb-addresses-mu4e-extract-name-from-contact c))
				   (contact `(:full-contact ,c :name ,name :contact-from "mu4e" :pos ,pos)))
			  (unless (ht-contains-p counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended email)
				(incf pos)
				(puthash email contact counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended))))
		  (ht-keys mu4e~contacts))
	;; put in hashtable for mu4e
	(mapc (lambda (c)
			(puthash (plist-get c :full-contact) (plist-get c :pos) counsel-mu4e-and-bbdb-addresses-mu4e-contacts)
			)
		  (ht-values counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended)
		  )
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
							 (ht-map (lambda (k v) (list k v)) counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended)
							 )
				   )
		   )
		  )
	)
  )

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
(defun counsel-mu4e-and-bbdb-addresses-mu4e-contacts ()
  "Select a contact from mu4e or BBDB via counsel.  Default action is to show emails from the selected contact."
  (interactive)
  (ivy-read "Contact:"
			(counsel-mu4e-and-bbdb-addresses-get-sorted-contacts)
			:history 'counsel-mu4e-and-bbdb-addresses-mu4e-contacts-history
			:action '(1
					  ("a" (lambda (add)
							 (mu4e-headers-search (counsel-mu4e-and-bbdb-addresses-mu4e-get-all-emails-from-person add))
							 )
					   "show all emails from all email addresses for this person")
					  ("i" (lambda (add)
							 (let ((nohash (replace-regexp-in-string "[ ]*#" "" add)))
							   (insert nohash)
							   )
							 ) "insert")
					  ("w" (lambda (add)
							 (let ((nohash (replace-regexp-in-string "[ ]*#" "" add)))
							   (kill-new nohash)
							   )
							 ) "copy to killring")
					  ("I" (lambda (add)
							 (insert (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address add))
							 ) "insert email address only")
					  ("W" (lambda (add)
							 (kill-new (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address add))
							 ) "copy to email address only to killring")
					  ("l" (lambda (add)
							 (let* ((email (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address add))
								    (query (concat "from:" email " OR " "to:" email)))
							   (mu4e-headers-search query)
							   )
							 )
					   "show email from/to this person")
					  ("f" (lambda (add)
							 (let* ((email (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address add))
								    (query (concat "from:" email)))
							   (mu4e-headers-search query)
							   )
							 )
					   "show emails from this person")
					  ("c" (lambda (add)
							 (let* ((email (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address add)))
							   (mu4e~compose-mail email))
							 (mu4e-headers-search query)
							 )
					   "send an email to person")
					  ("b" (lambda (add)
							 (let* ((email (counsel-mu4e-and-bbdb-addresses-mu4e-extract-email-from-address add)))
							   ;; 4th entry is the email address (counsel-bbdb format)
							   (counsel-mu4e-and-bbdb-addresses-counsel-bbdb-open-in-bbdb-action `(nil nil nil nil ,email))
							   )
							 )
					   "open record in BBDB"
					   )
					  )
			:caller 'counsel-mu4e-and-bbdb-addresses-mu4e-contacts
			)
  )



;;;###autoload
(defun counsel-mu4e-and-bbdb-addresses-setup ()
  "Advice mu4e contacts."
  (advice-tools/advice-add-if-def
   'mu4e~compose-complete-handler
   :override
   'counsel-mu4e-and-bbdb-addresses-mu4e~compose-complete-handler)
  )

;;;###autoload
(defun counsel-mu4e-and-bbdb-addresses-remove ()
  "Advice mu4e contacts."
  (advice-tools/advice-remove-if-def
   'mu4e~compose-complete-handler
   'counsel-mu4e-and-bbdb-addresses-mu4e~compose-complete-handler)
  )

(provide 'counsel-mu4e-and-bbdb-addresses)
;;; counsel-mu4e-and-bbdb-addresses.el ends here
