;;; init.el --- Initialization file for Emacs

;;; Commentary:

;; Emacs startup file

;;; Code:

(add-to-list 'load-path "~/.emacs.d/user-lisp/")

;;; Configure defaults
(customize-set-variable 'user-full-name "Ian Hattendorf")
(customize-set-variable 'user-mail-address "ian@ianhattendorf.com")
(customize-set-variable 'initial-scratch-message nil)
(put 'upcase-region 'disabled nil)

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
;; Leave menu enabled
(when (fboundp 'menu-bar-mode) (menu-bar-mode t))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable splash screen
(setq inhibit-startup-screen t)

;; Increase GC size during init
(setq gc-cons-threshold (* 64 1000 1000))
(add-hook 'after-init-hook #'(lambda ()
                               (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))))

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
(require 'bind-key)

(load-theme 'leuven)

(font-lock-add-keywords 'prog-mode
                        '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\)" 1 font-lock-warning-face prepend)))

(use-package buffer-move
  :ensure t
  :bind (
         ("<C-s-up>" . buf-move-up)
         ("<C-s-down>" . buf-move-down)
         ("<C-s-left>" . buf-move-left)
         ("<C-s-right>" . buf-move-right)))

(use-package whitespace
  :hook ((prog-mode text-mode) . whitespace-mode)
  :config
  (setq whitespace-line-column 120)
  (setq whitespace-style '(face tabs tab-mark empty trailing lines-tail)))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

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

(use-package neotree
  :defer t
  :ensure t
  :bind ("<f7>" . neotree-project-dir)
  :config
  (setq neo-autorefresh nil))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package winum
  :ensure t
  :config
  (winum-mode))

(use-package smartparens
  :ensure t
  :config
  (use-package smartparens-config)
  (smartparens-global-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package counsel
  :ensure t
  :after ivy
  :config
  (counsel-mode 1)
  (if (executable-find "rg")
      ;; Use ripgrep instead of grep
      (setq counsel-grep-base-command
            "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
            counsel-rg-base-command
            "rg -i -M 120 --no-heading --line-number --color never %s .")
    (warn "\nWARNING: Could not find ripgrep executable, defaulting to grep.")))

(use-package ivy
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

; Disabled, unused
(use-package drag-stuff
  :disabled
  :ensure t
  :config
  (drag-stuff-define-keys)
  (drag-stuff-global-mode 1))

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
    (flyspell-correct-word-generic)))

(use-package flyspell-correct-ivy
  :ensure t)

(use-package flymake
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package sh-script
  :config
  (setq sh-basic-offset 2))

(use-package markdown-mode
  :defer t
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :defer t
  :ensure t
  :mode (".yml" ".yaml"))

(use-package json-mode
  :defer t
  :ensure t
  :mode (".json"))

(use-package meson-mode
  :defer t
  :ensure t
  :mode "\\meson\\.build\\'")

(use-package cmake-mode
  :defer t
  :ensure t)

(use-package go-mode
  :defer t
  :ensure t)

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (add-to-list 'company-backends 'company-ansible)
  ;; Delete unused backends
  (setq company-backends
        (cl-set-difference
         company-backends
         '(company-bbdb
           company-clang
           company-eclim
           company-oddmuse
           company-xcode))))

(use-package company-ansible
  :ensure t)

(use-package eglot
  :ensure t
  :bind ("C-c C-f" . eglot-format)
  :hook ((rust-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)))

(use-package rust-mode
  :defer t
  :ensure t
  :mode ("\\.rs\\'" . rust-mode))

(use-package toml-mode
  :defer t
  :ensure t
  :mode ("\\.toml\\'" . toml-mode))

(use-package cargo
  :defer t
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package js
  :config
  (setq js-indent-level 2))

(use-package web-mode
  :defer t
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
  (add-to-list 'web-mode-indentation-params '("case-extra-offset" . nil)))

(use-package modern-cpp-font-lock
  :ensure t
  :config
  (modern-c++-font-lock-global-mode t))

(use-package rpm-spec-mode
  :defer t
  :ensure t)

(use-package diff-hl
  :ensure t
  :init (global-diff-hl-mode t))

(use-package magit
  :bind (("C-x g" . magit-status))
  :ensure t
  :config
  (setq magit-process-password-prompt-regexps ; Yubikey support
        '("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
          ;; match-group 99 is used to identify a host
          "^\\(Enter \\)?[Pp]assword\\( for '\\(?99:.*\\)'\\)?: ?$"
          "^.*'s password: ?$"
          "^Yubikey for .*: ?$"
          "^Enter PIN for '.*': ?$")))

(use-package magit-repos
  :config
  (setq magit-repository-directories '(("~/dotfiles" . 0) ("~/dev/repos" . 1))))

; Disabled (wait until more mature)
(use-package forge
  :disabled
  :ensure t
  :config
  (delete "~/.authinfo.gpg" auth-sources)
  (add-to-list 'auth-sources "~/.authinfo.gpg"))

(use-package expand-region
  :ensure t
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

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

(provide 'init)
;;; init.el ends here
