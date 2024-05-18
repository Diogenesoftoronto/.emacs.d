;;; package --- Summary
;;; Commentary:
;;; This is a package that defines a series of example commands in elisp.

;;; Code:
(defun example-command
    (arg-one)
  "This is my first Emacs command.  It does nothing but say hello.  ARG-ONE is the name."
  (cond (= arg-one "Dio")
	(message "Yo %s" arg-one)
	
(message "Hello %s" arg-one)))

(provide 'example-local-package)

