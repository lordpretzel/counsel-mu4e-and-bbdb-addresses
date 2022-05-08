;;; counsel-mu4e-and-bbdb-addresses-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "counsel-mu4e-and-bbdb-addresses" "counsel-mu4e-and-bbdb-addresses.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from counsel-mu4e-and-bbdb-addresses.el

(autoload 'counsel-mu4e-and-bbdb-full-contacts-sorted "counsel-mu4e-and-bbdb-addresses" "\
Return sorted contacts.

Contacts are sorted based on the order recoded in
`counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended'." nil nil)

(autoload 'counsel-mu4e-and-bbdb-addresses-mu4e-contacts "counsel-mu4e-and-bbdb-addresses" "\
Select a contact from mu4e or BBDB via counsel.

Default action is to show emails from the selected contact." t nil)

(autoload 'counsel-mu4e-and-bbdb-addresses-setup "counsel-mu4e-and-bbdb-addresses" "\
Advice mu4e contacts." nil nil)

(autoload 'counsel-mu4e-and-bbdb-addresses-remove "counsel-mu4e-and-bbdb-addresses" "\
Advice mu4e contacts." nil nil)

(register-definition-prefixes "counsel-mu4e-and-bbdb-addresses" '("counsel-mu4e-and-bbdb-addresses-" "mu4e-search-company-completion-contacts"))

;;;***

;;;### (autoloads nil "flycheck_counsel-mu4e-and-bbdb-addresses"
;;;;;;  "flycheck_counsel-mu4e-and-bbdb-addresses.el" (0 0 0 0))
;;; Generated autoloads from flycheck_counsel-mu4e-and-bbdb-addresses.el

(autoload 'counsel-mu4e-and-bbdb-full-contacts-sorted "flycheck_counsel-mu4e-and-bbdb-addresses" "\
Return sorted contacts.

Contacts are sorted based on the order recoded in
`counsel-mu4e-and-bbdb-addresses-mu4e-contacts-extended'." nil nil)

(autoload 'counsel-mu4e-and-bbdb-addresses-mu4e-contacts "flycheck_counsel-mu4e-and-bbdb-addresses" "\
Select a contact from mu4e or BBDB via counsel.

Default action is to show emails from the selected contact." t nil)

(autoload 'counsel-mu4e-and-bbdb-addresses-setup "flycheck_counsel-mu4e-and-bbdb-addresses" "\
Advice mu4e contacts." nil nil)

(autoload 'counsel-mu4e-and-bbdb-addresses-remove "flycheck_counsel-mu4e-and-bbdb-addresses" "\
Advice mu4e contacts." nil nil)

(register-definition-prefixes "flycheck_counsel-mu4e-and-bbdb-addresses" '("counsel-mu4e-and-bbdb-addresses-" "mu4e-search-company-completion-contacts"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; counsel-mu4e-and-bbdb-addresses-autoloads.el ends here
