

					; (elpaca-wait)
;; A recipe for sly quicklisp a sly 'contrib' via Elpaca, not sure if this will work, going to try it.
;(:package "sly-quicklisp" :source nil :protocol https :inherit t :depth 1 :fetcher github :repo "joaotavora/sly-quicklisp" :files (:defaults "*.lisp" "*.asd"))

;; Load custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Load other configuration files
					;(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; These do not yet exist, but this is the structure to look forward once,
;; I want to split my config up more!
					;(load "init-packages")
					;(load "init-ui")
					;(load "init-editing")
					;(load "init-programming")
					;(load "init-org")
					;(load "init-keybindings")


;; Initialize package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))
;; Dashboard for emacs
(use-package dashboard
  :ensure
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

;; Enable visual bell instead of sound
(setq visible-bell t)

;; Fix known issue with MELPA
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; In place of smartparens and pairedit
(electric-pair-mode)
(setq electric-pair-pairs '(
			   (?\{ . ?\})
			   (?\[ . ?\])
			   (?\" . ?\")
			   (?\< . ?\>)

					))


;; themes
(use-package ayu-theme
  :init (load-theme 'ayu-dark t))

					;(use-package catppuccin-theme
					;  :defer t)

; Makes sure that the spinner dependency is available
(use-package spinner :ensure t)

;; LSP Mode
(use-package lsp-mode
  :commands lsp
  :hook (prog-mode . lsp)
  :custom
  (lsp-keymap-prefix "s-l")
  :config
  (use-package lsp-ui :commands lsp-ui-mode)
  (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
  (use-package lsp-treemacs
    :config
    (lsp-treemacs-sync-mode 1)))

;; Company Mode!
(use-package company
  :hook (after-init . global-company-mode))

;; Flycheck
(use-package flycheck
  :hook (after-init . global-flycheck-mode))

;; DAP Mode
(use-package dap-mode
  :commands dap-debug
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-lldb)
  (dap-register-debug-template "Rust::LLDB"
			       (list :type "lldb"
				     :request "launch"
				     :name "LLDB::Run"
				     :program "${workspaceFolder}/target/debug/your_program"
				     :cwd "${workspaceFolder}")))

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
  :init (helm-mode 1))

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)
; VTerm
(use-package vterm
  :commands vterm)

;;Fontaine
(use-package fontaine
    :ensure t
    :config
    (setopt fontaine-presets
            '((regular
               :default-height 140)
              (small
               :default-height 110)
              (large
               :default-weight semilight
               :default-height 180
               :bold-weight extrabold)
              (extra-large
               :default-weight semilight
               :default-height 210
               :line-spacing 5
               :bold-weight ultrabold)
              (t                        ; our shared fallback properties
               :default-family "PragmataPro Mono Liga")))


    
;; Sly (Common Lisp)
(use-package sly
  :ensure t
  :config
  ;; Set the Lisp implementation and dynamic space size for SBCL
  (setq sly-lisp-implementations
        '((sbcl ("sbcl" "--dynamic-space-size" "8192"))
          ;; Add other Lisp implementations here if needed
          )))

;; Alternative setup for sly using roswell, needs modification
;; (use-package sly
;;   :ensure t
;;   :commands (sly sly-connect)
;;   :init
;;   (setq sly-symbol-completion-mode nil
;;         sly-default-lisp 'roswell
;;         ros-config (locate-user-emacs-file
;;                     "ros-conf.lisp")
;;         sly-lisp-implementations
;;         `((sbcl ("sbcl") :coding-system utf-8-unix)
;;           (abcl ("abcl") :coding-system utf-8-unix)
;;           (ecl ("ecl") :coding-system utf-8-unix)
;;           (roswell ("ros" "-Q" "-l" ,ros-config "run"))
;;           (qlot ("qlot" "exec" "ros" "-l" ,ros-config "run" "-S" ".")
;;                 :coding-system utf-8-unix))))


(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
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
   '("?" . meow-cheatsheet))
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
  :ensure
  :config
  (meow-setup)
  (meow-global-mode 1))

(use-package cmake-mode)

;; Custom-set variables and faces
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(ayu-dark))
 '(custom-safe-themes
   '("3325e2c49c8cc81a8cc94b0d57f1975e6562858db5de840b03338529c64f58d1"
     default))
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(ayu-theme catppuccin-theme cmake-mode company dap-mode dashboard
	       flycheck helm-lsp lsp-ivy lsp-ui meow sly smart-comment
	       smartparens swiper treemacs-evil treemacs-icons-dired
	       treemacs-magit treemacs-projectile use-package vterm
	       which-key which-key-posframe)))

;; (custom-set-faces
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
;; )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

