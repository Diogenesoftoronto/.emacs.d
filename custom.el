;;; package --- Summary
;;; Commentary:
;;; Code:
;; This loads costum functions and settings, like removing elements of the ui.

;; let's make lambda look pretty! I think it makes sense to try
;; extracting more of this stuff to separate local packages that I can
;; also publish to github and have locally. There is a good amount of
;; goodies here at this point!
(defun dio-lisp-setup ()
  (setq prettify-symbols-alist '(("lambda" . 955))) ; greek lambda
  (prettify-symbols-mode 1)
  ;;; Setting the column width is not really necessary
  ;;; anymore. because of the inclusion of visual lines but I will
  ;;; keep with the google standards for common-lisp
  (setq current-fill-column 100)
  (electric-pair-local-mode 1) ;; instead of pair edit, we may choose
  ;; to enable a lisp version of meow along with paredit
  (auto-fill-mode))
(defun dio-prog-setup () (lambda ()
			   (setq prettify-symbols-alist
				 '(("lambda" . ?λ)
				   ("->"     . ?→)
				   ("map"    . ?↦)
				   ("/="     . ?≠)
				   ("!="     . ?≠)
				   ("=="     . ?≡)
				   ("<="     . ?≤)
				   (">="     . ?≥)
				   ("&&"     . ?∧)
				   ("||"     . ?∨)
				   ("sqrt"   . ?√)
				   ("..."    . ?…))))
       (prettify-symbols-mode 1))
(add-hook 'prog-mode-hook #'dio-prog-setup)
;; (add-hook 'lisp-mode-hook 'dio-lisp-setup)
;; (add-hook 'emacs-lisp-mode-hook 'dio-lisp-setup)

;; Just some simple function that are similar to helix!
;; You can use load-user
(defvar config-file (expand-file-name "~/.emacs.d/init.el"))
(defvar custom-file (expand-file-name "~/.emacs.d/custom.el"))
(defvar early-init-file (expand-file-name "~/.emacs.d/early-init.el"))
(defun open-config ()
  "This function opens the init file to begin editing."
  ;; (interactive "cEnter character: ")
  ;; The option here is that you would press c, i or e for early-init-file, init-file and custom-file.
  (interactive)
  (find-file config-file))
(defun open-custom ()
  "This function opens the init file to begin editing."
  (interactive)
  (find-file custom-file))

;; I wanted to preserve some of the niceties of helix
;; So this little utility function is helpful
;; Next I want to see if I can get a little box to appear
;; On the right hand side with the commands that are available next
;; and I really want a format command, or format on save.
(defun reload-config ()
  "This function reloads Emacs with the current config."
  (interactive)
  (load-file config-file))
(defun reload-all-conf ()
  "This function reloads all user configuration for Emacs."
  (interactive)
  ;; You can just make this a dolist instead
  ;; (let (()))
  (cl-loop for file in (list config-file custom-file early-init-file) do
	   (load-file file)))

					; (defun rotate (&key start end ele &optional direction ele-p)
					;   "This rotates a list of ELE and outputs the changed positons in DIRECTION, without giving direction, it defaults to clockwise"
					;   (interactive)
					;   (if direction
					;       (funcall (lambda (start end ele direction)
					; 		 ())
					; 	       start end ele direction)
					;     (funcall (lambda (start end ele)
					; 	       ;; by default it runs the function clockwise
					; 	       ;; we want to loop through ele, transposing the car with the cdr until we reach a cdr that fails ele-p.
					; 	       ;; once we do this we must then send the inner list and call the function again.
					; 	       ;; we do this until we reach an empty set and return the final list.
					; 	       (let ((fele (first ele))
					; 		     (lele (last ele))
					; 		     (retele ele)
					; 		     (prele nil))

					; 		 (if (ele-p lele) (setf prele lele)
					; 		   (setf lele prele)
					; 		   (setf fele lele)
					; 		   )
					; 		 start end ele)))))

					; (defun rotate-chars (chars &optional direction)
					;   "This rotates a list of ELE and outputs the changed positons in DIRECTION, without giving direction, it defaults to clockwise"
					;   (interactive)
					;   (if direction
					;       ((funcall (lambda (direction)
					; 		  ())
					; 		start end ele direction)
					;        (let  ((rotating (lambda (chars)
					; 			  (
					; 			   (let* ((first-char (car chars)
					; 					      (lars chars)
					; 					      (last-char (while (cdr lars)
					; 							   (setf lars (cdr chars))))))
					; 			     (setf chars (transpose-chars first-char last-char))
					; 			     (while chars (rotating chars)))))))
					; 	 (rotating chars)))
					;     ))

;; Let us make urls a bit nicer to deal with, lets wrap the browse command
;; with https if it is not in the url, otherwise we will just use browse
;; we can call it the dio-browse-url
(defun dio-url-push (prefix url)
  "Prepend PREFIX to URL if URL does not already start with PREFIX."
  (if (string-prefix-p prefix url)
      url
    (concat prefix url)))

(defun dio-browse-url (url &optional insecure)
  "Browse URL using HTTPS by default. If INSECURE is non-nil, use HTTP instead."
  (interactive "sEnter URL: \nP")
  (browse-url (dio-url-push (if insecure "http://" "https://") url)))

(defun dio-enable-scroll-mode (&rest _args)
  "Enable scroll bar mode."
  (scroll-bar-mode 1))

(defun dio-disable-scroll-mode (&rest _args)
  "Disable scroll bar mode."
  (scroll-bar-mode -1))

(defvar medium-delay 0.4
  "Medium delay in seconds.")

(defvar short-delay 0.15
  "Short delay in seconds.")

(defun enable-and-then-reset (&rest _args)
  "Enable scroll bar mode after a short delay, disable it after a medium delay."
  (run-at-time short-delay nil #'dio-enable-scroll-mode)
  (run-at-time medium-delay nil #'dio-disable-scroll-mode))

(advice-add 'scroll-up-command :before #'enable-and-then-reset)
(advice-add 'scroll-down-command :before #'enable-and-then-reset)
(advice-add 'scroll-left-command :before #'enable-and-then-reset)
(advice-add 'scroll-right-command :before #'enable-and-then-reset)

;; On focus display VISUAL MIXED line numbers via either disply visual
;; line numbers or some other facility, make sure to remove the mode
;; when switching buffers
(define-minor-mode mixed-line-numbers-mode
  "Toggle mixed visual line numbers mode."
  :init-value nil
  :global nil
  (if mixed-line-numbers-mode
      (progn
        (display-line-numbers-mode 1)
        (setq display-line-numbers 'visual)  ;; Set to visual line numbers
	(setq display-line-numbers-current-absolute t))
    (progn (setq display-numbers-current-absolute t) (display-line-numbers-mode -1))))  ;; Disable line numbers

(defun enable-mixed-line-numbers-mode ()
  "Enable mixed-line-numbers-mode."
  (mapcar (lambda (x) (when (derived-mode-p x)
			(mixed-line-numbers-mode 1)))
	  '(org-mode prog-mode)))  ;; Optionally limit to
;; programming modes and org-mode, should add markdown mode in the
;; future


(defun disable-mixed-line-numbers-mode ()
  "Disable mixed-line-numbers-mode."
  (mixed-line-numbers-mode -1))

(defun mixed-line-numbers-buffer-switch ()
  "Enable mixed-line-numbers-mode on the new buffer and disable it on the old one."

  (walk-windows (lambda (w)
                  (unless (eq w (selected-window))
                    (with-current-buffer (window-buffer w)
		      (disable-mixed-line-numbers-mode)))))
  (enable-mixed-line-numbers-mode))

;; (defun highlight-selected-window ()
;;   "Highlight selected window with a different background color."
;;   (walk-windows (lambda (w)
;;                   (unless (eq w (selected-window))
;;                     (with-current-buffer (window-buffer w)
;;                       (buffer-face-set '(:background
;;   (self-theme-darkened (current-theme))))))))
;;   ;; to get this to work you need to find the current theme and
;;   calculate a darkened value, this is slightly non-trivial since
;;   calculate the theme colour requires some math.
;;   (buffer-face-set 'default))
;; (add-hook 'buffer-list-update-hook 'highlight-selected-window)

(add-hook 'buffer-list-update-hook 'mixed-line-numbers-buffer-switch)
;; Set initial buffer choice to 'dired' or 'helix', does not seem to work.
;; (setq initial-buffer-choice 'dired) ;; Change 'dired' to your preferred mode

;; Set the fixed pitch face, this is something I will figure out later
;; We got this setting from systemcrafter: emacs from scratch
;; It seems that this will likely not work with emacs 27.1 which is currently comes
					; with Ubuntu/Pop-os. Will upgrade to Emacs 29.* now.
					; (set-face-attribute 'default nil :font "Fira Code Retina" :height 100)


					; (defun dio-matched-char (char)
					;   "Match a character."
					;   (pcase char
					;     (`?< ?>)
					;     (`?( ?))
					;     (`?[ ?])
					;     (`?{ ?})
					;     (_ char)))

					; (defun dio-surround-region-with-character (char)
					;   "Surrounds a region with char"
					;   (let ((start (region-beginning))
					;         (end (region-end)))
					;     (save-excursion
					;       (goto-char end)
					;       (insert (dio-matched-char char))
					;       (goto-char start)
					;       (insert char))))

					; (defun dio-surround-region-with-character-inter (char)
					;   "Surround region with CHAR and its matching counterpart."
					;   (interactive "cEnter character: ")
					;   (dio-surround-region-with-character char))
;; ;; Example usage:
;; ;; Select a region in the buffer and call the function, e.g.,
;; ;; (surround-region-with-character ?<)
;; ;; (define-prefix-command exterm exterm-mode-map "manipulate vterm from other-window")

					; (defvar-keymap exterm
					;   "C-v" #'vterm-other-window
					;   "v" #'vterm
					;   "C-p" (lambda () (progn (vterm-other-window)
					; 			  (meow-yank)
					; 			  (vterm-insert)))
					;   "p" (lambda () (progn (vterm-otherv-window)
					; 			(meow-clipboard-yank)
					; 			(vterm-insert))))

					; (bind-key "C-h C-f" #'describe-function)
					; (bind-key "C-h C-m" #'describe-mode)
					; (bind-key "C-o" #'switch-to-buffer)
;; This allows me to use meow-mode, a way of using emacs similar to Helix, it is
;; Selection Action Paradigm modal interaction system.


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e7820b899036ae7e966dcaaec29fd6b87aef253748b7de09e74fdc54407a7a02"
     "1781e8bccbd8869472c09b744899ff4174d23e4f7517b8a6c721100288311fa5"
     "e410458d3e769c33e0865971deb6e8422457fad02bf51f7862fa180ccc42c032"
     default))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; custom.el ends here
