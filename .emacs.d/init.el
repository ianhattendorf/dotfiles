;;; init.el --- Initialization file for Emacs

;;; Commentary:

;; Emacs startup file

;;; Code:

(add-to-list 'load-path "~/.emacs.d/user-lisp/")

;;; Configure defaults
(customize-set-variable 'user-full-name "Ian Hattendorf")
(customize-set-variable 'user-mail-address "ian@ianhattendorf.com")

(add-to-list 'default-frame-alist '(font . "Fira Code 14"))


;; Enable ligature support on MacOS (mituharu fork only)
(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

;;; Fira Code ligatures (requires Fira Code Symbol font, see: https://github.com/tonsky/FiraCode/issues/211#issuecomment-239058632)

(require 'setup-ligatures)

;; Avoid polluting init.el with customizations
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :noerror)

;; Disable GUI elements
;; Leave menu and tool bar enabled while learning
(when (fboundp 'menu-bar-mode) (menu-bar-mode t))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable splash screen
(setq inhibit-startup-screen t)

;; Increase GC size during init
(setq gc-cons-threshold (* 64 1000 1000))
(add-hook 'after-init-hook #'(lambda ()
			       (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))))

;; Scratch customization
(customize-set-variable 'initial-scratch-message nil)

;; Tramp Mode (remote files)
(setq tramp-default-method "ssh")
(setq explicit-shell-file-name "/bin/bash")

;; Show matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Disable bell
(setq ring-bell-function #'ignore)

;; Display line infomation
(global-linum-mode t)
(line-number-mode 1)
(column-number-mode 1)

;; Git symbolic link visit real file
(setq vc-follow-symlinks t)

;;; Packages
(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(setq package-archive-priorities
      '(("melpa-stable" . 10)
	      ("melpa" . 100)))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; use-package
(eval-when-compile
  (require 'use-package))
;(require 'diminish)
(require 'bind-key)

;;; Theme
(load-theme 'leuven)

;;; buffer-move
(use-package buffer-move
  :ensure t
  :bind (
	 ("<C-s-up>" . buf-move-up)
	 ("<C-s-down>" . buf-move-down)
	 ("<C-s-left>" . buf-move-left)
	 ("<C-s-right>" . buf-move-right)
	 )
  )

;;; whitespace-mode
(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  ;; (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 120)
  (setq whitespace-style '(face spaces tabs space-mark tab-mark empty trailing lines-tail))
  )

;;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  )

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode)
  )

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

;;; NeoTree
(use-package neotree
  :ensure t
  :bind (
         ("<f7>" . neotree-project-dir)
         )
  :config
  (setq neo-autorefresh nil)
  )

;;; Editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1)
  )

;;; winum
(use-package winum
  :ensure t
  :config
  (winum-mode)
  )

;;; smartparens
(use-package smartparens
  :ensure t
  :config
  (use-package smartparens-config)
  (smartparens-global-mode 1)
  )

;;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;;; Counsel/Ivy
(use-package counsel
  :ensure t
  :config
  (counsel-mode 1)
  (if (executable-find "rg")
      ;; Use ripgrep instead of grep
      (setq counsel-grep-base-command
            "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
            counsel-rg-base-command
            "rg -i -M 120 --no-heading --line-number --color never %s ."
            )
    (warn "\nWARNING: Could not find ripgrep executable, defaulting to grep.")
    )
  )

(use-package ivy
  :bind (
	 ("C-c C-r" . ivy-resume)
	 ("<f6>" . ivy-resume)
	 )
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  )

(use-package swiper
  :bind (
	 ("C-s" . swiper)
	 ("C-r" . swiper)
	 )
  )

;;; Drag Stuff
(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-define-keys)
  (drag-stuff-global-mode 1)
  )

;;; flyspell
(use-package flyspell
  :bind (("<f8>" . flyspell-correct-word-generic)
	 ("C-<f8>" . flyspell-correct-previous-word-generic)
	 ("M-<f8>" . flyspell-check-next-highlighted-word))
  :hook ((text-mode . turn-on-flyspell)
	 (prog-mode . flyspell-prog-mode))
  :init
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (flyspell-correct-word-generic))
  )

(use-package flyspell-correct-ivy
  :ensure t
  )

;;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :ensure t
  )

(use-package sh-script
  :config
  (setq sh-basic-offset 2)
  )

;;; markdown-mode
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  )

;;; yaml-mode
(use-package yaml-mode
  :ensure t
  :mode (".yml" ".yaml")
  )

;;; json-mode
(use-package json-mode
  :ensure t
  :mode (".json")
  )

;;; meson-mode
;; Disabled, https://github.com/wentasah/meson-mode/issues/16
(use-package meson-mode
  :disabled
  :ensure t
  :mode "\\meson\\.build\\'"
  :hook (meson-mode . company-mode)
  )

;;; Company
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (add-to-list 'company-backends 'company-ansible)
  (add-to-list 'company-backends 'company-rtags)
  (add-to-list 'company-backends 'company-jedi)
  (add-to-list 'company-backends 'company-flow)

  ;; Delete unused backends
  (setq company-backends (delete 'company-bbdb company-backends))
  (setq company-backends (delete 'company-clang company-backends))
  (setq company-backends (delete 'company-eclim company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends))
  (setq company-backends (delete 'company-xcode company-backends))

  (define-key company-mode-map (kbd "C-;") #'company-complete)
  )

(use-package company-rtags
  :ensure t
  )

(use-package company-ansible
  :ensure t
  )

(use-package company-jedi
  :ensure t
  :config
  (setq jedi:environment-virtualenv
	(list "virtualenv-3" "--system-site-packages"))
  )

;;; flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (use-package flycheck-rtags
    :config
    (defun my-flycheck-rtags-setup ()
      "Configure flycheck-rtags for better experience."
      (flycheck-select-checker 'rtags)
      (setq-local flycheck-check-syntax-automatically nil)
      (setq-local flycheck-highlighting-mode nil))
    (add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
    (add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
    (add-hook 'objc-mode-hook #'my-flycheck-rtags-setup)
    )
  )

(use-package flycheck-rtags
  :ensure t
  )

;;; todo/fixme/etc. highlighting
(font-lock-add-keywords 'prog-mode
			'(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\)" 1 font-lock-warning-face prepend)))

;; ;; tide
;; (use-package tide
;;   :ensure t
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;; 	 (js2-mode . tide-setup)
;; 	 (rjsx-mode . tide-setup)
;;          (before-save . tide-format-before-save))
;;   )

;; ;; rjsx
;; (use-package rjsx-mode
;;   :ensure t
;;   :mode "\\.jsx$"
;;   )

;; ;; js2-mode
;; (use-package js2-mode
;;   :ensure t
;;   :mode ("\\.js\\'" . js2-mode)
;;   )

;; javascript-mode
(use-package js
  :config
  (setq js-indent-level 2)
  )

;; web-mode
(use-package web-mode
  :ensure t
  :mode ("\\.jsx?\\'" "\\.hbs\\'")
  :config
  (setq web-mode-auto-quote-style 2) ; single quotes
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-enable-auto-indentation nil)
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-quotes" . nil))
  (add-to-list 'web-mode-indentation-params '("case-extra-offset" . nil))
  )

;; clang-format
(use-package clang-format
  :ensure t
  :config
  (setq clang-format-style-option "file")
  (use-package cc-mode
    :config
    (define-key c-mode-base-map (kbd "C-c C-f") 'clang-format-region)
    )
  )

;;; Modern C++ font lock
(use-package modern-cpp-font-lock
  :ensure t
  :config
  (modern-c++-font-lock-global-mode t)
  )

;;; RTags
(use-package rtags
  :ensure t
  :config
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (rtags-enable-standard-keybindings)
  )

;;; rpm-spec-mode
(use-package rpm-spec-mode
  :ensure t
  )

;;; diff-hl
(use-package diff-hl
  :ensure t
  :init (global-diff-hl-mode t)
  )

;;; Magit
(use-package magit
  :bind (
	 ("C-x g" . magit-status)
	 )
  :ensure t
  :config
  ; Yubikey support
  (setq magit-process-password-prompt-regexps
      '("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
        ;; match-group 99 is used to identify a host
        "^\\(Enter \\)?[Pp]assword\\( for '\\(?99:.*\\)'\\)?: ?$"
        "^.*'s password: ?$"
        "^Yubikey for .*: ?$"
        "^Enter PIN for '.*': ?$"))
  )

;;; Magit Repositories
(use-package magit-repos
  :config
  (setq magit-repository-directories '(
				       ("~/dotfiles" . 0)
				       ("~/dev/repos" . 1)
				       ))
  )

;;; Magit Forge (wait until more mature)
;; (use-package forge
;;   :ensure t
;;   :config
;;   (delete "~/.authinfo.gpg" auth-sources)
;;   (add-to-list 'auth-sources "~/.authinfo.gpg")
;;   )

;;; Expand Region
(use-package expand-region
  :ensure t
  :commands er/expand-region
  :bind ("C-=" . er/expand-region)
  )

;;; Random functions
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

;; (load-file "~/.emacs.d/flow-for-emacs/flow.el")

;; Company Flow
;; (use-package company-flow
;;   :ensure t
;;   )

(provide 'init)
;;; init.el ends here
(put 'upcase-region 'disabled nil)
