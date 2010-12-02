(require 'base64)
(require 'hex-util)
(require 'md5)


(defun oplop:subsequence-of-digits (str)
  (if (string-match "\\([0-9]+\\)" str)
      (match-string 0 str)
    nil))


(defun oplop:account-password (nickname master-password)
  ;; The steps it takes to generate an account password is:
  ;; Concatenate the master password with the nickname (in that
  ;; order!). Generate the MD5 hash of the concatenated string.
  ;; Convert the MD5 hash to URL-safe Base64. See if there are any
  ;; digits in the first 8 characters. If no digits are found ...
  ;; Search for the first uninterrupted substring of digits. If a
  ;; substring of digits is found, prepend them to the Base64 string.
  ;; If no substring is found, prepend a 1. Use the first 8 characters
  ;; as the account password.
  (let* ((master-password (encode-coding-string master-password 'utf-8))
         (nickname (encode-coding-string nickname 'utf-8))
         (plain-text (concat master-password nickname))
         (digest (decode-hex-string (md5 plain-text)))
         (encoded (base64-encode-string digest))
         (first-8 (substring encoded 0 8))
         (digits (oplop:subsequence-of-digits first-8)))
    (if (not digits) (concat "1" first-8)
      first-8)))


(defun oplop ()
  "Oplop is a password hashing algorithm. See http://code.google.com/p/oplop/"
  (interactive "")
  (let ((nickname (read-string "nickname: "))
        (master-password (read-passwd "master-password: "))
        )
    (kill-new (oplop:account-password nickname master-password))
  ))


(provide 'oplop)