;;; init.el --- Initialization file for Emacs

;;; Commentary:

;; Emacs startup file

;;; Code:

;;; use-package
(eval-when-compile
  (require 'use-package))

(use-package emacs
  :init
  ;; Tree-sitter
  ;; Update all: M-: -> (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (markdown "https://github.com/MDeiml/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; Packages
  ; dnf install emacs-json-mode emacs-magit emacs-yaml-mode
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/yaml-mode/")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/json-mode/")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/json-snatcher/")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/magit/")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/async/")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/dash/")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/with-editor/")

  (setq package-enable-at-startup nil)

  ;; straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/d4cd480395f3d593e1987fbe7b26b38cd4918b9c/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; Configure defaults
  (customize-set-variable 'user-full-name "Ian Hattendorf")
  (customize-set-variable 'user-mail-address "ian@ianhattendorf.com")
  (customize-set-variable 'initial-scratch-message nil)

  ;; Avoid polluting init.el with customizations
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file :noerror)

  ;; Disable GUI elements
  ;; Leave menu enabled
  (when (fboundp 'menu-bar-mode) (menu-bar-mode t))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  ;; (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

  ;; Disable splash screen
  (setq inhibit-startup-screen t)

  ;; Native compilation
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-compile-prune-cache t)

  ;; Backups/autosave
  (setq make-backup-files nil)

  ;; Git symbolic link visit real file
  (setq vc-follow-symlinks t)

  (global-set-key (kbd "s-u") 'revert-buffer)
  (global-unset-key (kbd "C-z"))

  ;; Auto revert buffer if unchanged and file on disk changes
  (global-auto-revert-mode)

  ;; Show matching parens
  (show-paren-mode 1)
  (setq show-paren-delay 0)

  ;; Spaces instead of tabs
  (setq-default indent-tabs-mode nil)

  ;; Disable bell
  (setq ring-bell-function #'ignore)

  ;; Smooth scrolling
  (setq pixel-scroll-precision-large-scroll-height 40.0)
  (setq pixel-scroll-precision-interpolate-page t)
  (pixel-scroll-precision-mode)

  ;; Display line infomation
  (global-display-line-numbers-mode 1)
  (line-number-mode 1)
  (column-number-mode 1)

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package avy
  :straight t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)
         ("C-c C-j" . avy-resume))
  :config
  (avy-setup-default))

(use-package company
  :straight t)

(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(substring orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package flycheck
  :straight t
  :init (global-flycheck-mode)
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error)))

(use-package clang-format
  :bind (("C-c u" . clang-format-buffer)
         ("C-M-<tab>" . clang-format-region)))

(use-package c-ts-mode
  :mode ("\\.c\\'" "\\.h\\'"))

(use-package json-ts-mode
  :mode ("\\.json\\'"))

(use-package js-ts-mode
  :custom
  (js-indent-level 2)
  :mode ("\\.jsx?\\'"))

(use-package flow-minor-mode
  :straight '(flow-minor-mode
              :type git
              :host github
              :repo "an-sh/flow-minor-mode")
  :hook (js-ts-mode . flow-minor-enable-automatically))

(use-package yaml-ts-mode
  :mode ("\\.ya?ml\\'"))

(use-package python-ts-mode
  :config
  :mode ("\\.py\\'"))

(use-package rust-ts-mode
  :straight t
  :config
  (setq rust-format-on-save t)
  :mode ("\\.rs\\'"))

(use-package dts-mode
  :straight t
  :mode ("\\.overlay\\'" "\\.dts\\'" "\\.dtsi\\'"))

(use-package lsp-mode
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-ts-mode. lsp)
         (c++-ts-mode. lsp)
         (js-ts-mode. lsp)
         (typescript-ts-mode. lsp)
         (python-ts-mode . lsp)
         (rust-ts-mode. lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

;; ;; optionally if you want to use debugger
;; (use-package dap-mode)
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; ;; optional if you want which-key integration
(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

;; (use-package flymake-eslint
;;   :straight t
;;   :hook (js-ts-mode . flymake-eslint-enable))

(use-package eslint-disable-rule
  :straight t)
 
(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package ef-themes
  :straight t
  :config
  (setq ef-themes-headings ; read the manual's entry or the doc string
        '((0 variable-pitch light 1.9)
          (1 variable-pitch light 1.8)
          (2 variable-pitch regular 1.7)
          (3 variable-pitch regular 1.6)
          (4 variable-pitch regular 1.5)
          (5 variable-pitch 1.4) ; absence of weight means `bold'
          (6 variable-pitch 1.3)
          (7 variable-pitch 1.2)
          (t variable-pitch 1.1)))

  ;; They are nil by default...
;  (setq ef-themes-mixed-fonts t
;        ef-themes-variable-pitch-ui t)

  ;; Read the doc string or manual for this one.  The symbols can be
  ;; combined in any order.
  (setq ef-themes-region '(intense no-extend neutral))

  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)

  ;; Load the theme of choice:
  ;; (load-theme 'ef-cyprus :no-confirm)

  ;; OR use this to load the theme which also calls `ef-themes-post-load-hook':
  ;; (ef-themes-select 'ef-cyprus)

  ;; The themes we provide are recorded in the `ef-themes-dark-themes',
  ;; `ef-themes-light-themes'.

  ;; We also provide these commands, but do not assign them to any key:
  ;;
  ;; - `ef-themes-toggle'
  ;; - `ef-themes-select'
  ;; - `ef-themes-select-dark'
  ;; - `ef-themes-select-light'
  ;; - `ef-themes-load-random'
  ;; - `ef-themes-preview-colors'
  ;; - `ef-themes-preview-colors-current'
  )

(use-package auto-dark
  :straight t
  :after ef-themes
  :config
  (setq auto-dark-light-theme 'ef-cyprus)
  (setq auto-dark-dark-theme 'ef-elea-dark)
  (setq ef-themes-to-toggle '(ef-cyprus ef-elea-dark))
  (auto-dark-mode t))

(provide 'init)
;;; init.el ends here
