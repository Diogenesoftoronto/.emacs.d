;;; lexical-binding 
;;; early-init --- summary
;;; Commentary:
;;Elpaca install all elpaca  packages below this block.
;;For more information see: github.com/progfolio/elpaca
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x)) ;; I could
    ;; remove this check but I will keep it here just incase for
    ;; others who may want to try my config or if I am on computer
    ;; with an old version of emacs.
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))
		)
	)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.

;; (elpaca example-package)
;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(use-package org :ensure t)
;; This is RECOMMENDED by elpaca.
;; We will configure the built-ins for emacs here.
(use-package emacs :ensure nil
  :custom
  ;; This breaks the code for some reason
  ;;(require-theme 'modus-themes)
  
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 5)
  
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate
   #'command-completion-default-include-p)
  
  ;; Line numbering default
  (display-line-numbers-current-absolute t)
  (display-line-numbers 'relative)
  ;; Reset default UI to be more minimal
  ;; Disable menu bar, scroll bar, and tool bar at startup
  (menu-bar-mode nil)

  ;; disable scroll bar because eww, though temporary scroll bar would be nice when scrolling
  (scroll-bar-mode nil)
  
  ;; remove the tool bar
  (tool-bar-mode nil)
  ;; I would love to get relative line numbers in emacs

  ;; Inhibit startup message
  (inhibit-startup-message t)

  ;; remove scroll bars
  (set-scroll-bar-mode nil)
  
  ;; Enable visual bell instead of sound
  (visible-bell t)
  
  ;; Repeat stuff, repeat stuff
  ;; You will need to setup a repeat-map to make this work though
  ;; there is a `karthinks' blog-post about it.
  (repeat-mode t)
  
  ;; make prefix wrap, this is the adaptive-mode package now in emacs 30.1
  ;; this will not work with older emacs versions, keep that in mind!
  (global-visual-wrap-prefix-mode t)

  ;; Set up projectile mode
  (projectile-mode t)

  ;; get rid of package.el, this may help compatibility
  (setq package-enable-at-startup nil)

  :config
  ;; Backup stuff so that I no longer have to change git to ignore these.
  ;; let's move backup files to a different directory!
  (load-theme 'modus-vivendi)
  (setq backup-dir "tmp/backups/")
  (setq backup-directory-alist `(( "." . ,(expand-file-name backup-dir user-emacs-directory))))
  ;; Let's move auto saves to another directory as well.
  (setq autosave-dir (expand-file-name "tmp/auto-saves/" user-emacs-directory))
  (make-directory autosave-dir t)
  (setq auto-save-list-file-prefix (expand-file-name "sessions/" autosave-dir)
				auto-save-file-prefix-name-transforms `(".*" ,(expand-file-name "sessions/" autosave-dir) t))
  )
;;; early-init.el ends here

