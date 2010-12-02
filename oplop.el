(require 'base64)
(require 'hex-util)
(require 'md5)


(defun oplop:subsequence-of-digits (str)
  (if (string-match "\\([0-9]+\\)" str)
      (match-string 0 str)
    nil))


(defun oplop:replace-char-in-string (chr rep str)
  (let ((replaced str))
    (while (string-match (concat "\\(" (regexp-quote chr) "\\)") replaced)
      (setq replaced (replace-match rep nil nil replaced)))
    replaced))


(defun oplop:base64-encode-string-urlsafe (str)
  (let* ((encoded (base64-encode-string str 't))
         (safer (oplop:replace-char-in-string "+" "-" encoded))
         (safest (oplop:replace-char-in-string "/" "_" safer)))
    safest))


(defun oplop:account-password (nickname master-password)
  ;; The steps it takes to generate an account password is:
  (let* ((master-password (encode-coding-string master-password 'utf-8))
         (nickname (encode-coding-string nickname 'utf-8))
         ;; Concatenate the master password with the nickname (in that
         ;; order!).
         (plain-text (concat master-password nickname))
         ;; Generate the MD5 hash of the concatenated string.
         (digest (decode-hex-string (md5 plain-text)))
         ;; Convert the MD5 hash to URL-safe Base64.
         (encoded (oplop:base64-encode-string-urlsafe digest))
         ;; See if there are any digits in the first 8 characters.
         (first-8 (substring encoded 0 8))
         (digits (oplop:subsequence-of-digits first-8))
         ;; If no digits are found ... Search for the first
         ;; uninterrupted substring of digits. If a substring of
         ;; digits is found, prepend them to the Base64 string. If no
         ;; substring is found, prepend a 1. Use the first 8
         ;; characters as the account password.
         (account-password (if digits first-8 (concat "1" (substring first-8 0 7)))))
    account-password))


(defun oplop ()
  "Oplop is a password hashing algorithm. See http://code.google.com/p/oplop/"
  (interactive "")
  (let ((nickname (read-string "nickname: "))
        (master-password (read-passwd "master-password: "))
        )
    (kill-new (oplop:account-password nickname master-password))
  ))


(provide 'oplop)
