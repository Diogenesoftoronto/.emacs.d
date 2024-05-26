;; Load custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Load other configuration files
;; These do not yet exist, but this is the structure to look forward to once I want to split my config up more!
;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (load "init-packages")
;; (load "init-ui")
;; (load "init-editing")
;; (load "init-programming")
;; (load "init-org")
;; (load "init-keybindings")

;; Fix known issue with MELPA
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Initialize package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Dashboard for emacs
(use-package dashboard
  :ensure t
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

;; let's make lambda look pretty!
(defun my/lisp-setup ()
  (setq prettify-symbols-alist '(("lambda" . 955))) ; greek lambda
  (prettify-symbols-mode 1)
  (setq current-fill-column 100)
  (electric-pair-local-mode 1)
(auto-fill-mode))
(add-hook 'lisp-mode-hook 'my/lisp-setup)
(add-hook 'elisp-mode-hook 'my/lisp-setup)

;; Automatic code formatting
(use-package apheleia
  :ensure t
  :commands (alpheleia-global-mode +1)
  :config
  ;; (apheleia-global-mode)
  ;; Setup auto formatting for purescript
  (push '(purs-tidy "purs-tidy" "format") apheleia-formatters)
  (setf (alist-get 'purescript-mode apheleia-mode-alist) '(purs-tidy))
  ;; Setup auto formatting for haskell
  (push '(fourmolu "fourmolu") apheleia-formatters)
  (setf (alist-get 'haskell-mode apheleia-mode-alist) '(fourmolu)))

;; Display hex colors in emacs
(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode))

;; Themes
(use-package material-theme
  :ensure t
  :init
  (load-theme 'material t))

(use-package pulse
  ;; Highlight cursor position after movement
  :defer t
  :init (defun pulse-line (&rest _)
          (pulse-momentary-highlight-one-line (point)))
  (dolist (command '(other-window
                     windmove-do-window-select
                     mouse-set-point
                     mouse-select-window))
    (advice-add command :after #'pulse-line)))

;; Makes sure that the spinner dependency is available
(use-package spinner :ensure t)

;; LSP Mode
(use-package lsp-mode
  :commands lsp
  ;; We want to hook on language that have an lsp, langauge like lisp should not be included.
  ;; They seem mess up the syntax highlighting for some reason. Perhaps exception list on hooks would cool to have.
  ;; You might be able to have a this work via remove-hook or advice :after
  ;; :hook (prog-mode . lsp)
  :hook (go-mode python-mode rust-mode)
  :custom
  (lsp-keymap-prefix "s-l")
  :config
  (use-package lsp-ui :commands lsp-ui-mode)
  (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
  (use-package lsp-treemacs
    :config
    (lsp-treemacs-sync-mode 1)))

;; Company Mode
(use-package company
  :ensure t
  :init (global-company-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Flycheck
(use-package flycheck :ensure t
  :ensure t
  :commands (flycheck-mode)
  :config ;; Add the new checker to flycheck
  ;; Define the Common Lisp checker using SLY
  (defun my-flycheck-sly-start (checker callback)
    "Start a Flycheck syntax check with SLY."
    (let ((buffer (current-buffer)))
      (sly-compile-and-load-file
       (buffer-file-name)
       (lambda (result)
         (with-current-buffer buffer
           (funcall callback 'finished
                    (mapcar (lambda (msg)
                              (flycheck-error-new-at
                               (sly-note.line msg)
                               (sly-note.column msg)
                               (pcase (sly-note.severity msg)
                                 (`:error 'error)
                                 (`:warning 'warning)
                                 (`:note 'info))
                               (sly-note.message msg)
                               :checker checker
                               :buffer buffer
                               :filename (buffer-file-name)))
                            result)))))))

  (flycheck-define-generic-checker 'common-lisp-sly
    "A Common Lisp syntax checker using SLY."
    :start #'my-flycheck-sly-start
    :modes '(lisp-mode sly-mrepl-mode))
  
  (add-to-list 'flycheck-checkers 'common-lisp-sly)
  ;; Optional: Set common-lisp-sly as the default checker for lisp-mode
  (add-hook 'lisp-mode-hook
	    (lambda ()
	      (flycheck-select-checker 'common-lisp-sly)))
  )

;; EXPERIMENTAL
;; Adapted script from Github, made to use sly instead of slime 
;; (defun flymake-cl-backend (report-fn &rest \_args)
;;   ;; (slime-compile-region (point-min) (point-max))
;;   (let ((start (point-min))
;; 	(end (point-max)))
;;     (sly-connection)
;;     ;; (sly-flash-region start end)
;;     (run-hook-with-args 'sly-before-compile-functions start end)
;;     (let ((sly-compilation-finished-hook 'sly-maybe-show-compilation-log))
;;       (sly-compile-string (buffer-substring-no-properties start end) start))))

;; ;;;###autoload
;; (defun flymake-cl-setup (&optional enabled-checkers)
;;   "Setup Flymake for Cl."
;;   (interactive)
;;   (add-hook 'flymake-diagnostic-functions
;; 	    'flymake-cl-backend
;; 	    nil t))


;; ;; DAP Mode
;; (use-package dap-mode
;;   :commands dap-debug
;;   :config
;;   (dap-mode 1)
;;   (dap-ui-mode 1)
;;   (dap-tooltip-mode 1)
;;   (require 'dap-lldb)
;;   (dap-register-debug-template "Rust::LLDB"
;;                                (list :type "lldb"
;;                                      :request "launch"
;;                                      :name "LLDB::Run"
;;                                      :program "${workspaceFolder}/target/debug/your_program"
;;                                      :cwd "${workspaceFolder}")))

;; Treemacs
(use-package treemacs
  :commands treemacs
  :config
  (use-package treemacs-evil)
  (use-package treemacs-projectile)
  (use-package treemacs-magit)
  (use-package treemacs-icons-dired)
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1))

;; Helm
(use-package helm
  :ensure t
  :init (helm-mode 1))

(use-package helm-lsp
  :ensure t)

;; VTerm
(use-package vterm
  :ensure t
  :commands vterm)

;; far, its like ummm par which is like fmt but for emacs

;; ;; Fontaine
;; (use-package fontaine
;;   :ensure t
;;   :config
;;   (setopt fontaine-presets
;;           '((regular :default-height 140)
;;             (small :default-height 110)
;;             (large :default-weight semilight :default-height 180 :bold-weight extrabold)
;;             (extra-large :default-weight semilight :default-height 210 :line-spacing 5 :bold-weight ultrabold)
;;             (t :default-family "PragmataPro Mono Liga"))))

(setq inferior-lisp-program "/usr/bin/sbcl")

;; Ensure global syntax highlighting
(global-font-lock-mode)

;; Use sly for Common Lisp interaction
;; The hooks do not seem to be doing much of anything will have to figure that out later
(use-package sly
  :ensure t
  :commands (sly sly-connect)
  :hook ((lisp-mode . sly-mode))
  :config
  (setq sly-lisp-implementations
        '((sbcl ("sbcl" "--dynamic-space-size" "8192"))))
  
  )

					;(electric-pair-mode)
					;(setq electric-pair-pairs '(
					;			    (?\{ . ?\})
					;			    (?\[ . ?\])
					;			    (?\" . ?\")
					;			    (?\< . ?\>)
					;			    ))

;; Enable Rainbow Delimiters for color-coding nested parentheses
(use-package rainbow-delimiters
  :ensure t
  :hook (lisp-mode emacs-lisp-mode)
  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#999")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#666"))

;; (use-package helm-company
;;   :ensure t
;;   :config
;;   (define-key company-active-map
;; 	      (kbd "TAB")
;; 	      #'company-complete-common-or-cycle)
;;   (define-key company-active-map
;; 	      (kbd "<backtab>")
;; 	      (lambda ()
;;                 (interactive)
;;                 (company-complete-common-or-cycle -1))))

(use-package helm-company
  :ensure t
  ;; :config
  ;;(define-key sly-mrepl-mode-map (kbd "<tab>") 'helm-company))
  )

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '(":" . meow-M-x)
   '("<escape>" . ignore))
  (meow-leader-define-key
   '("j" . "H-j")
   '("k" . "H-k")
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("s" . meow-clipboard-yank)
   '("y" . meow-clipboard-save)
   )
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

;; This allows me to use meow-mode, a way of using emacs similar to Helix, it is 
;; Selection Action Paradigm modal interaction system.
(use-package meow
  :ensure t
  :config
  (meow-setup)
  (meow-global-mode 1))

(use-package cmake-mode)

