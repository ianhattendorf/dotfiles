;;;; init.el

;;; Configure defaults
;; Avoid polluting init.el with customizations
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :noerror)

;; Disable GUI elements
;; Leave menu and tool bar enabled while learning
(when (fboundp 'menu-bar-mode) (menu-bar-mode t))
(when (fboundp 'tool-bar-mode) (tool-bar-mode t))
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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(setq package-archive-priorities
      '(("melpa-stable" . 100)
	("melpa" . 10)))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(customize-set-variable
 'package-selected-packages
 '(use-package
    markdown-mode
    yasnippet
    yasnippet-snippets
    helm
    helm-flyspell
    clang-format
    modern-cpp-font-lock
    rtags
    ycmd
    company
    company-rtags
    company-ansible
    company-ycmd
    flycheck-ycmd
    flycheck-rtags)
 )

(package-install-selected-packages)

;;; use-package
(require 'use-package)

;;; flyspell
(use-package flyspell
  :init
  (progn
    (setq flyspell-issue-welcome-flag nil)
    (add-hook 'text-mode-hook 'turn-on-flyspell)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (global-set-key (kbd "<f8>") 'helm-flyspell-correct)
    (defun flyspell-check-next-highlighted-word ()
      "Custom function to spell check next highlighted word"
      (interactive)
      (flyspell-goto-next-error)
      (helm-flyspell-correct))
    (global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)
    )
  )

;;; yasnippet
(use-package yasnippet
  :config
  (yas-global-mode 1)
  )

;;; Company
(use-package company
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

;;; flycheck
(use-package flycheck
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

;;; todo/fixme/etc. highlighting
(font-lock-add-keywords 'prog-mode
			'(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\)" 1 font-lock-warning-face prepend)))

;;; clang-format
(use-package clang-format
  :config
  (setq clang-format-style-option "file")
  (use-package cc-mode
    :config
    (define-key c-mode-base-map (kbd "C-c C-f") 'clang-format-region)
    )
  )

;;; Modern C++ font lock
(use-package modern-cpp-font-lock
  :config
  (modern-c++-font-lock-global-mode t)
  )

;;; RTags
(use-package rtags
  :config
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (rtags-enable-standard-keybindings)
  )
