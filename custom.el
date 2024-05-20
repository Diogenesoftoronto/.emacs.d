;; This loads costum functions and settings, like removing elements of the ui.

;; Just some simple function that are similar to helix!
(defvar config-file (expand-file-name "~/.emacs.d/init.el"))
(defun open-config ()
  "This function opens the init file to begin editing."
  (interactive)
  (find-file config-file))

;; I wanted to preserve some of the niceties of helix
;; So this little utility function is helpful
;; Next I want to see if I can get a little box to appear
;; On the right hand side with the commands that are available next
;; and I really want a format command, or format on save.
(defun reload-config ()
  "This function reloads Emacs with the current config."
  (interactive)
  (load-file config-file))

;; Let us make urls a bit nicer to deal with, lets wrap the browse command
;; with https if it is not in the url, otherwise we will just use browse
;; we can call it the dio-browse-url
					; (defun dio-browse-url
					;     (url optional)
					;   "URL is the url to browse, OPTIONAL is whether to use insecure http connections, https is the DEFAULT"
					;   (interactive)
					;   (if optional
					;   (browse-url (dio-url-push "http://" url))
					;   (browse-url (dio-url-push "https://" url))
					;   ))

					; (defun dio-url-push (string url)
					; "This is a helper function that will conditionally prepend the STRING to the URL depending on whether URL contains STRING as a prefix"
					; (if (has-prefix? string) (prepend string url)
					; (url)))


					; (defun dio-enable-scroll-mode (&rest _args)
					;   "Set the `dio-scrolling` variable to t"
					;   (setq  scroll-bar-mode 1))
					; (defun dio-reenable-scroll-mode ()
					; "Reset scrolling"
					;     (setq scroll-bar-mode -1))


					; (defvar medium-delay "0.4 sec")
					; (defvar short-delay "0.15 sec")

					; (defun enable-and-then-reset (&rest _args)
					;   "This function enables the scroll bar mode after a SHORT delay, and reenables it after a MEDIUM delay"
					; ;; Set scrolling to true after a SHORT delay
					; (run-at-time short-delay nil #'dio-enable-scroll-mode)

					; ;; Set scrolling to false after a MEDIUM delay
					; (run-at-time medium-delay nil #'dio-reenable-scroll-mode)
					; )
					; ;; you need to `advice` the function with events, basically this function
					; ;; will get called after the advice function is called. Maybe, more research on
					; ;; how this actually works at a different time.
					; (advice-add 'scroll-up-command :before #'dio-enable-and-then-reset)
					; (advice-add 'scroll-down-command :before #'dio-enable-and-then-reset)
					; (advice-add 'scroll-left-command :before #'dio-enable-and-then-reset)
					; (advice-add 'scroll-right-command :before #'dio-enable-and-then-reset)

;; Reset default UI to be more minimal
;; Disable menu bar, scroll bar, and tool bar at startup
(menu-bar-mode -1)

;; remove the tool bar
(tool-bar-mode -1)
;; I would love to get relative line numbers in emacs

;; Inhibit startup message
(setq inhibit-startup-message t)

;; Enable visual bell instead of sound
(setq visible-bell t)

;; remove scroll bars
(set-scroll-bar-mode nil)

;; Set initial buffer choice to 'dired' or 'helix', does not seem to work.
					; (setq initial-buffer-choice 'dired) ;; Change 'dired' to your preferred mode

;; Set the fixed pitch face, this is something I will figure out later
;; We got this setting from systemcrafter: emacs from scratch
;; It seems that this will likely not work with emacs 27.1 which is currently comes
					; with Ubuntu/Pop-os. Will upgrade to Emacs 29.* now.
					; (set-face-attribute 'default nil :font "Fira Code Retina" :height 100)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-enabled-themes '(material-theme))
;;  '(custom-safe-themes
;;    '("3325e2c49c8cc81a8cc94b0d57f1975e6562858db5de840b03338529c64f58d1"
;;      default))
;;  '(ispell-dictionary nil)
;;  '(package-selected-packages
;;    '(ayu-theme catppuccin-theme cmake-mode company dap-mode dashboard
;; 	       flycheck helm-lsp lisp-extra-font-lock lsp-ivy lsp-ui
;; 	       meow sly smart-comment smartparens swiper treemacs-evil
;; 	       treemacs-icons-dired treemacs-magit treemacs-projectile
;; 	       use-package vterm which-key which-key-posframe))
;;  '(warning-suppress-log-types '((use-package) (lsp-mode) (lsp-mode)))
;;  '(warning-suppress-types '((lsp-mode) (lsp-mode) (lsp-mode) (lsp-mode))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
