;;; package --- Summary

;;; Commentary:

;;; Code:

;;; Configure defaults
(customize-set-variable 'user-full-name "Ian Hattendorf")
(customize-set-variable 'user-mail-address "ian@ianhattendorf.com")

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

;; Show matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)

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
      '(("melpa-stable" . 100)
	("melpa" . 10)))

(setq package-pinned-packages
      '(
	(use-package . "melpa")
	))

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

;;; flyspell
(use-package flyspell
  :bind (("<f8>" . flyspell-correct-word-generic)
	 ("C-<f8>" . flyspell-correct-previous-word-generic)
	 ("M-<f8>" . flyspell-check-next-highlighted-word))
  :hook ((text-mode . turn-on-flyspell)
	 (prog-mode . flyspell-prog-mode))
  :init
  (setq flyspell-issue-welcome-flag nil)
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

;;; diff-hl
(use-package diff-hl
  :ensure t
  :init (global-diff-hl-mode t)
  )

;;; Magit
(use-package magit
  :ensure t
  )
