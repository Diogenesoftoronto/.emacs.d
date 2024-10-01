;;; package --- Summary
;;; Commentary:
;;; Load custom file
;;; Code:
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Load other configuration files
;; These do not yet exist, but this is the structure to look forward to once I want to split my config up more!
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
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
                         ("elpa" . "https://elpa.gnu.org/packages/"))
      )

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
				(fish "https://github.com/ram02z/tree-sitter-fish")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
				(odin "https://github.com/ap29600/tree-sitter-odin")
				;; This probably won't work some extra work needs to be done. Possiblely using the newer version as well which has build problems
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Dashboard for emacs
(use-package dashboard
  :ensure t
  :config (dashboard-setup-startup-hook)
  :init
  (add-hook 'elpaca-after-init-hook
						#'dashboard-insert-startupify-lists
						(add-hook
						 'elpaca-after-init-hook #'dashboard-initialize))
  )

;; Automatic code formatting
(use-package apheleia
  :ensure t
  :init
  (require 'apheleia-formatters)
  :hook (prog-mode))

;; (use-package prism :ensure t
;;   :hook (prog-mode))
;; I used rainbow delimiters before but I am going to try using prism,
;; it seems genuinely interesting.
;; Display hex colors in emacs
;; (use-package rainbow-mode
;;   :ensure t
;;   :commands (rainbow-mode))
;; Enable Rainbow Delimiters for color-coding nested parentheses
;; (use-package rainbow-delimiters
;;   :ensure t
;;   :hook (lisp-mode emacs-lisp-mode)
;;   :config
;;   (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")
;;   (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")
;;   (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")
;;   (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")
;;   (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")
;;   (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")
;;   (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")
;;   (set-face-foreground 'rainbow-delimiters-depth-8-face "#999")
;;   (set-face-foreground 'rainbow-delimiters-depth-9-face "#666"))

;; Themes and making things pretty and based as hell!!!
;; Themes are fun and I can play around with them quite a lot, the
;; material theme is actually pretty nice. I also like the gruvbox
;; themes, the modus themes are okay, they are better as a way to setup
;; themes of your own.
;; NOTE: Because modus themes comes in emacs verson 30, there is no longer a
;; need to have the themes here. I have moved them to the emacs
;; package, along with other configuration that comes with emacs out
;; of the box.
;; I want them icons in me now.
;; (use-package all-the-icons :ensure t
;;   :if (display-graphic-p)
;;   :init (all-the-icons-install-fonts)
;;   )
;; This is a theme I think looks rather nice but I have gone for the modus themes more recently.
;; (use-package material-theme
;;   :ensure t
;;   :init
;;   (load-theme 'material t))

;; Instead of all the icons, we will be using the nerd icons instead, all the icons have been deprecated in treemacs.
;; Nerd Icons
(use-package nerd-icons :ensure t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

;; I want to move values up and around other stuff.
(use-package drag-stuff :ensure t
  :hook ((prog-mode . drag-stuff-mode)
				 (org-mode . drag-stuff-mode)
				 (text-mode . drag-stuff-mode))
  ;; :bind ("C-k" ("Drag this line up: " . drag-stuff-up)
  ;; 	 "C-j" ("Drag this line down: " . drag-stuff-down))
  ;; :bind-keymap
  ;; ("C-c `" . drag-stuff-mode-map)
  ;; :bind
  ;; (:map drag-stuff-mode-map ("C-j" drag-stuff-up) ("C-k" drag-stuff-down))
  ;; (:repeat-map drag-stuff-repeat-map ("j" drag-stuff-up))
  )
;; (use-package pulsar
;;   
;;   :defer t
;;   :init
;;   (defun pulse-line (&rest _)
;;           (pulse-momentary-highlight-one-line (point)))
;;   (dolist (command '(other-window
;;                      windmove-do-window-select
;;                      mouse-set-point
;;                      mouse-select-window))
;;     (advice-add command :after #'pulse-line)))

                                        ; Makes sure that the spinner dependency is available
(use-package spinner :ensure t)

;; Let's get surround capabilities similar to Helix
(use-package embrace :ensure t)

;; Flycheck
(use-package flycheck
  :ensure t
  :commands (flycheck-mode)
  :hook (emacs-lisp-mode go-mode fish-mode lisp-mode)
  ;; :config
  ;; Add the new checker to flycheck
  ;; Define the Common Lisp checker using SLY

  ;; (defun my-flycheck-sly-start (checker callback)
  ;;   "Start a Flycheck syntax check with SLY."
  ;;   (let ((buffer (current-buffer)))
  ;;     (sly-compile-and-load-file
  ;;      (buffer-file-name)
  ;;      (lambda (result)
  ;;        (with-current-buffer buffer
  ;;          (funcall callback 'finished
  ;;                   (mapcar (lambda (msg)
  ;;                             (flycheck-error-new-at
  ;;                              (sly-note.line msg)
  ;;                              (sly-note.column msg)
  ;;                              (pcase (sly-note.severity msg)
  ;;                                (`:error 'error)
  ;;                                (`:warning 'warning)
  ;;                                (`:note 'info))
  ;;                              (sly-note.message msg)
  ;;                              :checker checker
  ;;                              :buffer buffer
  ;;                              :filename (buffer-file-name)))
  ;;                           result)))))))
  ;; (flycheck-define-generic-checker 'common-lisp-sly
  ;;   "A Common Lisp syntax checker using SLY."
  ;;   :start #'my-flycheck-sly-start
  ;;   :modes '(lisp-mode sly-mrepl-mode))
  
  ;; (add-to-list 'flycheck-checkers 'common-lisp-sly)
  ;; Optional: Set common-lisp-sly as the default checker for lisp-mode
  

  ;; (if (not (bound-and-true-p plisp-mode)) (add-hook 'lisp-mode-hook

  ;; 						      (flycheck-select-checker 'common-lisp-sly))))
  )

;; EXPERIMENTAL
;; Adapted script from Github, made to use sly instead of slime.
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

;;;version control
(use-package transient
  :ensure (transient :host github :repo "magit/transient" :branch "main"))

(use-package magit
  :ensure (magit :host github :repo "magit/magit" :branch "main")
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
        magit-bury-buffer-function #'magit-restore-window-configuration))

;; DAP Mode
(use-package dap-mode
  :commands dap-debug
  :custom
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  :config
  (use-package dap-lldb)
  ;; (dap-register-debug-template "Rust::LLDB"
  ;;                              (list :type "lldb"
  ;;                                    :request "launch"
  ;;                                    :name "LLDB::Run"
  ;;                                    :program "${workspaceFolder}/target/debug/your_program"
  ;;                                    :cwd "${workspaceFolder}"))
  )

;; The doom modeline just looks better and has support in a wide variety of packages. Giving robust information in the modeline in a customizable way
(use-package doom-modeline
  :ensure t
  :functions doom-modeline-mode
  :init (doom-modeline-mode 1))

;;; Media selector and player and emacs!
(use-package ready-player
  :ensure (:host github :repo "xenodium/ready-player")
  :init (ready-player-mode 1)
  )
;; Dirvish, it replaces the function of treemacs and dired by providing a nice way to search and do actions on many files; It provides a tree like view of projects as well as working better with emacs as a whole.
(use-package dirvish
  :ensure t
  :functions dirvish-override-dired-mode
  ;; :defines dirvish-mode-line-format
  :config
  (dirvish-override-dired-mode)
  :custom
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (omit yank index)))
  (dirvish-mode-line-height 10)
  (dirvish-attributes
   '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (dirvish-subtree-state-style 'nerd)
  (delete-by-moving-to-trash t)
  (dirvish-path-separators (list
                            (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                            (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                            (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  )
;; Treemacs
(use-package treemacs :ensure t
  :commands treemacs
  :functions (treemacs-follow-mode treemacs-filewatch-mode)
  :config
  (use-package treemacs-evil) ;; is there a meow equivalent?
  (use-package treemacs-projectile)
  (use-package treemacs-magit)
  (use-package treemacs-icons-dired)
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1))

;; Jump around the window and buffer
(use-package avy :ensure t)
(use-package ace-window :ensure t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )
;;; ORG MODE STUFF all the config I need for org mode is here.
(use-package org-modern :ensure t
  :hook (org-mode))

(use-package org-roam
  :ensure t
  :functions org-roam-roam-db-autosync-mode
  :defines org-roam-node-display-template
  :custom
  (org-roam-directory (file-truename "~/Documents/org-roam/"))
  :bind (("C-c C-n C-l" . org-roam-buffer-toggle)
         ("C-c C-n C-f" . org-roam-node-find)
         ("C-c C-n C-g" . org-roam-graph)
         ("C-c C-n C-i" . org-roam-node-insert)
         ("C-c C-n C-c" . org-roam-capture)
         ;; Dailies
         ("C-c C-n C-j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (use-package org-roam-protocol))

(use-package pdf-tools
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :functions (pdf-tools-install)
  :config
  (pdf-tools-install :no-query))
;;; Markdown mode because i like to view documents that aren't org
;;; ones
(use-package markdown-mode :ensure t
  :init
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist
							 '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))
;;; Programming language specific stuff
(use-package go-mode :ensure t
  :mode ("\\.go\\'" . go-mode)
  :config
  (use-package go-playground :ensure t))

(use-package odin-mode
  :ensure (:host github :repo "MrJCraft/odin-mode"))
;; Fish is my default shell
(use-package fish-mode :ensure t
  :mode  ( "\\.fish\\'" . fish-mode)
  :interpreter "fish"
  ;; I want to figure out how to automatically
  ;; :after prism
  ;; :config
  ;; (remove-hook 'prism-mode-hook 'fish-mode)
  ;; (prism-mode -1)
	)
;;; Doing nix stuff
(use-package nix-mode :ensure t
  :mode ("\\.nix\\'" . nix-mode))
(use-package nix-drv-mode
  :after nix
  :mode "\\.drv\\'")
(use-package nix-repl
  :after nix
  :commands (nix-repl))
;; yarmelo called he wanted json back
(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.ya?ml\\'")
;; Rusting rustlings rust
(use-package rustic
  :ensure t
  :after flycheck-checkers
  :init
  (setq rustic-cargo-bin "cargo")
  (push 'rustic-clippy flycheck-checkers)
  )
;; Haskelling
(use-package haskell-mode
  :mode "\\.hs\\'"
  :config
  (add-hook 'haskell-mode-hook #'subword-mode)

  (define-key haskell-mode-map (kbd "C-c , c") #'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c , s") #'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c , l") #'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "C-c , T") #'haskell-doc-show-type)
  (define-key haskell-mode-map (kbd "C-c , t") #'haskell-mode-show-type-at))

(setq spook--indent-width 2)
(setq-default tab-width spook--indent-width)

(defun spook--nvm-p ()
  (when-let* ((node (string-trim (shell-command-to-string "fish -c 'readlink (which node)'")))
              (nvm-bin-dir
               (and (string-match-p "\/nvm\/" node)
                    (file-name-directory node))))
    nvm-bin-dir))
(setq css-indent-offset spook--indent-width)

;; (use-package js-ts
;;   :mode "\\.js'"
;;   :ensure nil
;;   :config
;;   (setq js-indent-level spook--indent-width)
;;   :hook
;;   (((js-ts-mode
;;      typescript-ts-mode) . subword-mode)))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
	:ensure t
  :config
  (setq web-mode-markup-indent-offset spook--indent-width)
  (setq web-mode-code-indent-offset spook--indent-width)
  (setq web-mode-css-indent-offset spook--indent-width)
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(use-package emmet-mode
	:ensure t
  :hook ((html-mode           . emmet-mode)
         (css-mode            . emmet-mode)
         (js-ts-mode          . emmet-mode)
         (js-jsx-mode         . emmet-mode)
         (typescript-ts-mode  . emmet-mode)
         (tsx-ts-mode         . emmet-mode)
         (web-mode            . emmet-mode))
  :config
  (setq emmet-insert-flash-time 0.001)	; effectively disabling it
  (add-hook 'js-jsx-mode-hook #'(lambda ()
                                  (setq-local emmet-expand-jsx-className? t)))   
  (add-hook 'web-mode-hook #'(lambda ()
                               (setq-local emmet-expand-jsx-className? t))))

;; (spook--defkeymap
;;  "spook-errors" "C-c e"
;;  '("n" . flycheck-next-error)
;;  '("p" . flycheck-previous-error)
;;  '("l" . flycheck-list-errors)
;;  '("e" . flycheck-explain-error-at-point))

(defun spook--setup-ts-js ()
  "Setup Javascript and Typescript for current buffer."
  ;; Add node_modules/.bin of current project to exec-path.
  (if-let (nvm-bin (spook--nvm-p))
      (add-to-list 'exec-path nvm-bin)
    (let ((bin-dir
           (expand-file-name
            "node_modules/.bin/"
            (locate-dominating-file default-directory "node_modules"))))
      (when (file-exists-p bin-dir)
        (add-to-list 'exec-path bin-dir))))

  ;; For 95% of cases this is what I want
  (prettier-format-on-save-mode +1)
  
  (setf flymake-eslint-project-root
        (locate-dominating-file default-directory "package.json")))

(add-hook 'js-ts-mode-hook #'spook--setup-ts-js)

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :ensure nil
  :hook ((typescript-ts-mode . subword-mode))
  :config
  (setq-default typescript-indent-level spook--indent-width)
  (add-hook 'typescript-mode-hook #'spook--setup-ts-js))

(use-package tsx-ts-mode
  :mode "\\.tsx\\'"
  :ensure nil
  :hook ((tsx-ts-mode . subword-mode))
  :config
  (setq-default typescript-indent-level spook--indent-width)
  (add-hook 'typescript-mode-hook #'spook--setup-ts-js))

;; (use-package css-ts-mode
;;   :ensure nil
;;   :mode "\\.s?css\\'")

;;; I will need to edit them at some point.
(use-package bash
  :mode  ( "\\.bash\\'" . bash-mode)
  :interpreter "bash")

;; (use-package yasnippet :ensure t
;;   :hook (prog-mode org-mode))
;; (use-package yasnippet-snippets
;;   :ensure t
;;   :after yasnippet)

(use-package yasnippet :ensure t
  :config
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets :ensure t
  :after yasnippet)

;; Edit comments in a separate buffer suited for them
(use-package separedit :ensure t :config
  ;; Key binding for modes you want edit
  ;; or simply bind ‘global-map’ for all.
  (define-key prog-mode-map        (kbd "C-c '") #'separedit)
  (define-key minibuffer-local-map (kbd "C-c '") #'separedit)
  (define-key help-mode-map        (kbd "C-c '") #'separedit)
  ;; (define-key helpful-mode-map     (kbd "C-c '") #'separedit)

  ;; Default major-mode for edit buffer
  ;; can also be other mode e.g. ‘org-mode’.
  (setq separedit-default-mode 'org-mode)

  ;; Feature options
  ;; (setq separedit-preserve-string-indentation t)
  ;; (setq separedit-continue-fill-column t)
  ;; (setq separedit-write-file-when-execute-save t)
  ;; (setq separedit-remove-trailing-spaces-in-comment t)
  )
;;Give xexps some love with tagedit
;; the paredit-like functions for html-mode
(use-package tagedit :ensure t
  :hook (html-mode)
  :config (tagedit-add-paredit-like-keybindings))
;;;Some lisp stuff
;; (use-package parinfer-rust-mode :ensure t
;;   :hook (emacs-lisp-mode
;; 	 ;; clojure-mode
;; 	 ;; scheme-mode
;; 	 ;; racket-mode
;; 	 ;; lisp-mode
;; 	 ))
;; Both of these require the recipe like formula used for ready-player-one
(use-package evil :ensure t)
(use-package symex :ensure t :config (symex-initialize))
(use-package buffer-ring :ensure (:host github :repo "countvajhula/buffer-ring"))
(use-package transpose-frame :ensure (:host github :repo "emacsorphanage/transpose-frame" :version (lambda (_) ".2.0")))
(use-package rigpa :ensure (:host github :repo "countvajhula/rigpa")
  :after (symex evil buffer-ring))
;; Use sly for Common Lisp interaction
;; The hooks do not seem to be doing much of anything will have to figure that out later
(use-package sly :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (require 'sly-autoloads)
  
  ;; (setq sly-enable-evaluate-in-emacs t)
  ;; This is to prepare for allowing sending values to emacs so that I can then call slynk, alternatively I would just send an interactive form, or wrap a regular emacs function that i send to slynk?
  ;; This last option seems to make more sense, i get the values from emacs, run save-sly-and-die with the arguments that I get from emacs. This way I can call compile file and it would just run this stuff, maybe even create a sbcl script?
  ;;   (defun sbcl-save-sly-and-die ()
  ;;   "Save a sbcl image, even when running from inside Sly.
  ;; This function should only be used in the *inferior-buffer* buffer,
  ;; inside emacs."
  ;;   (mapcar #'(lambda (x)
  ;;               (slynk::close-connection
  ;;                x nil nil))
  ;;           slynk::*connections*)
  ;;   (dolist (thread (remove
  ;;                    (slynk-backend::current-thread)
  ;;                    (slynk-backend::all-threads)))
  ;;     (slynk-backend::kill-thread thread))
  ;;   (sleep 1)
  ;;   (sb-ext:save-lisp-and-die #P"~/your-main-program.exe"
  ;;                             :toplevel #'your-main-function-here
  ;;                             :executable t
  ;;                             :compression t))
  ;; This function requires a path, a main function, and a name for the program.
  ;; These can be acquired from emacs before the function runs and then passed to the script with compile project. Included would also be the project directory and information from sly-asdf.
  ;; sly-asdf comes with the sly-asdf-load-system function, this could be concievably called with compile file but after further thought, why do this? sly-asdf already comes with a reload system which takes into account lisp buffers.

  ;; perhaps create an interactive function that sets the sly lisp implementation with a different memory size.
  (setq sly-lisp-implementations
				'((sbcl ("sbcl" "--dynamic-space-size" "8gb")))))
(cl-defmacro add-sly-contribs (&rest contribs) "A macro to add CONTRIBS to sly."
						 `(progn
								,@(cl-mapcar (lambda (contrib)
															 `(use-package ,(intern (concat "sly-" (symbol-name contrib))) :ensure t
																	:config
																	(add-to-list 'load-path
																							 (expand-file-name
																								(concat "~/.emacs.d/elpaca/builds/sly-" (symbol-name ',contrib))))
																	(require ',(intern (concat "sly-" (symbol-name contrib) "-autoloads")))))
														 contribs)))
(add-sly-contribs asdf quicklisp macrostep named-readtables) ;; TODO: add stepper, which seems to run into an error.
;; Picolisp
;; this documentation stuff is just broken right now and will need to be fixed later.
(use-package plisp-mode
  :ensure t
  :mode ("\\.l\\'" . plisp-mode)
  :custom
  ;; (plisp-documentation-method "/usr/bin/w3m")
  (plisp-documentation-directory "/usr/share/doc/picolisp/")

  ;; Need to make sure that the corfu backend works with this so that
  ;; it can work if it even does work right now hahaha
  (use-package company-plisp
    :defer
    :config (add-to-list 'company-backends '(company-plisp)))
  )
;; LSP Mode
(use-package lsp-mode
  :ensure t
  :commands lsp
  ;; We want to hook on language that have an lsp, langauge like lisp should not be included.
  ;; They seem mess up the syntax highlighting for some reason. Perhaps exception list on hooks would cool to have.
  ;; You might be able to have a this work via remove-hook or advice :after
  ;; :hook (prog-mode . lsp)
  :hook ((go-mode python-mode rust-mode bash-mode fish-mode) . lsp)
  :config
  ;; Fish mode still lacks lsp integration I should try to get this actually working at some point.
  (add-to-list 'lsp-language-id-configuration '(fish-mode . "fish"))
  )
(use-package lsp-ui :ensure t :commands lsp-ui-mode :after lsp-mode)
(use-package lsp-treemacs :ensure t
  :config (lsp-treemacs-sync-mode 1)
  :after (lsp-mode treemacs))

;;; Some completion oriented stuff is below
;; Company Mode
;; (use-package company
;;   :ensure t
;;   :init (global-company-mode))
;; Helm
;; (use-package helm
;;   :ensure t
;;   :config
;;   (helm-mode 1)
;;   ;; :bind ("C-x C-b" . helm-buffers-list)
;;   ;; :config
;;   ;; (use-package helm-lsp
;;   ;;   :ensure t)

;;   (use-package helm-company
;;     :ensure t
;;     ;; :config
;;     ;; :functions (company-complete-common-or-cycle)
;;     ;;   (define-key company-active-map
;;     ;;  		(kbd "TAB")
;;     ;;  		#'company-complete-common-or-cycle)
;;     ;;   (define-key company-active-map
;;     ;; 		(kbd "<backtab>")
;;     ;; 		(lambda ()
;;     ;;                 (interactive)
;;     ;;                 (company-complete-common-or-cycle -1)))
;;     )
;;   )

;; This is all to replace ivy, and helm need configure further https://kristofferbalintona.me/posts/202202211546/.
(use-package vertico
  :ensure t
  :bind (:map vertico-map ("<tab>" . #'vertico-insert)  ; Insert selected candidate into text area
							("<escape>" . #'minibuffer-keyboard-quit) ; Close minibuffer
							;; NOTE 2022-02-05: Cycle through candidate groups
							("C-n" . #'vertico-next)
							("C-p" . #'vertico-previous)
							)
  :config
  (vertico-mode))

(use-package consult
  :ensure t
  ;; let's configure this with meow mode's visit and buffer and outline
  )

(use-package embark
  :ensure t
  ;; TODO: consider moving these binds to meow-setup
  ;; :bind (("C-c C-a" . embark-act))
  )
;; TODO: consider putting this with the embark package
(use-package embark-consult :ensure t
  ;; comes bundled with Embark; no `:ensure t' necessary
  :after (embark consult))

;; Add extensions
(use-package cape :ensure t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  ;; the cape bindings should also be available in the
  :bind ("C-c C-n" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; TODO: the bindings above should be moved to meow mode. as should all bindings here. Alternatively, I may add bindings to meow mode here but the problem with that is conflicts on load. NOTE: I may be able to wrap native meow commands instead of changing meow setup. This is not necessarily a bad idea and could work as an extra contribution to meow mode as a contrib.
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("M-p p d" . cape-dabbrev)
  ;;        ("M-p p h" . cape-history)
  ;;        ("M-p p f" . cape-file)
  ;;        ...)
  ;; :config ;; config might be breaking something here, see if you
  ;; ;; can find an elpaca users configuration for corfu-company backends.
  ;; ;; Use Company backends as Capfs.
  ;; (setq completion-at-point-functions
  ;;    	(mapcar #'cape-company-to-capf
  ;;    		(list #'company-keywords #'company-dabbrev #'company-)))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'complete-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-history)
  ;; (add-hook 'completion-at-point-functions #'cape-company-to-capf)
  (add-hook 'complete-at-point-functions #'cape-elisp-symbol)
  (add-hook 'complete-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  )


(use-package corfu :ensure t
  ;; Optional customizations
  :custom
  (corfu-echo-documentation t)
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary t)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview

  (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))
(use-package lin :ensure t
  ;; consider using a colour from the modus theme our other theme you
  ;; are using so this is not terrible.
  :config
  (customize-set-variable 'lin-face 'lin-cyan)
  (setq lin-mode-hooks
				'(bongo-mode-hook
					dired-mode-hook
					elfeed-search-mode-hook
					git-rebase-mode-hook
					grep-mode-hook
					ibuffer-mode-hook
					ilist-mode-hook
					ledger-report-mode-hook
					log-view-mode-hook
					magit-log-mode-hook
					mu4e-headers-mode-hook
					notmuch-search-mode-hook
					notmuch-tree-mode-hook
					occur-mode-hook
					org-agenda-mode-hook
					pdf-outline-buffer-mode-hook
					proced-mode-hook
					emacs-lisp-mode-hook
					lisp-mode-hook
					tabulated-list-mode-hook))
  (add-to-list 'load-path "~/.emacs.d/manual-packages/lin")
  )

(use-package marginalia :ensure t
  :bind (:map minibuffer-local-map
							("M-A" . marginalia-cycle))
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))
;; Optionally use the `orderless' completion style.
(use-package orderless :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  )

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
				 ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\'` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package wgrep
  :ensure t)

;; Let's get some terminal love for dear emacs!
;; VTerm
;; Okay vterm could not properly run helix or my other tuis, there was
;; a lot more flickering as well. I will use the eat terminal instead.
(use-package vterm
  :ensure t
  :commands vterm
  )
;; Remove the visual-wrap prefix an
;; TODO: consider using fish, or eshell
;; Eat renders some things rather slowly
(use-package eat :ensure t :config
  (add-to-list 'load-path "~/.emacs.d/manual-packages/eat")
  )
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


(use-package projectile :ensure t)

;; the meow setup function could be put in some other place, it could
;; also be change do just a series of let statements possiblely.

(defun meow-setup ()
  "This is includes all the bindings for my modal editor."
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
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
   '("p" . meow-clipboard-yank)
   '("y" . meow-clipboard-save)
   '("u" . "C-u")
   '("b" . consult-project-buffer)
   '("f" . avy-goto-char)
   '("l" . avy-goto-line)
   '("o" . consult-outline)
   '("d" . dirvish)
   ;; '("C-c ," . projectile-command-map)
   ;; '("C-v" exterm)
   )
  ;; (meow-define-state paren
  ;;   "meow state for interacting with smartparens"
  ;;   :lighter " [P]"
  ;;   :keymap meow-paren-keymap);

  ;;  (meow-define-state paren
  ;;    "meow state for interacting with symex"
  ;;    :lighter " [P]"
  ;;    :keymap symex-mode-interface)
  
  ;; meow-define-state creates the variable
  ;; (setq meow-cursor-type-paren 'hollow)
  ;; (meow-setup-keys 'paren
  ;; 		   '( "<escape>" . meow-normal-mode)
  ;; 		   '(""))
  
  ;; (meow-define-keys 'paren
  ;;   '("<escape>" . meow-normal-mode)
  ;;   '("l" . sp-forward-sexp)
  ;;   '("h" . sp-backward-sexp)
  ;;   '("j" . sp-down-sexp)
  ;;   '("k" . sp-up-sexp)
  ;;   '("n" . sp-forward-slurp-sexp)
  ;;   '("b" . sp-forward-barf-sexp)
  ;;   '("v" . sp-backward-barf-sexp)
  ;;   '("c" . sp-backward-slurp-sexp)
  ;;   '("u" . meow-undo))

  (meow-normal-define-key
   '("|" . "M-|")
   '("T" . eat-other-window)
   ;; this should maybe change to switch to
   ;; the eat buffer if is available.
   '(":" . lsp-keymap-prefix) ;; makes it more like helix
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
   '("G" . consult-buffer)
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
   '("M" . meow-join)
   '("m" . embrace-commander)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("Q" . meow-grab)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("S" . meow-kill-append)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("V" . consult-ripgrep)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . consult-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("C-." . ace-window)
   '("<escape>" . ignore)))

;; MMMEOW the best modal editor around with custom additions nya~
(use-package meow
  :ensure t
  :config
  (meow-setup)
  ;; Let us disable meow-global-mode for eat, or we can enable meow mode for certain hooks.
  ;; (meow-global-mode 1)
  ;; :hook ((prog-mode org-mode) . meow-mode)
  (meow-global-mode)
  ;; (remove-hook 'meow-mode #'eat-mode-hook)
  )

(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

;;; init.el ends here
