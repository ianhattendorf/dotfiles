;;; init.el --- Initialization file for Emacs

;;; Commentary:

;; Emacs startup file

;;; Code:

;;; Configure defaults
(customize-set-variable 'user-full-name "Ian Hattendorf")
(customize-set-variable 'user-mail-address "ian@ianhattendorf.com")
(customize-set-variable 'initial-scratch-message nil)

;; Avoid polluting init.el with customizations
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :noerror)

;; Disable GUI elements
;; Leave menu enabled
(when (fboundp 'menu-bar-mode) (menu-bar-mode t))
;(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable splash screen
(setq inhibit-startup-screen t)

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

;;; Tree-sitter
; Requires branch, not sha
;(setq treesit-language-source-alist
;      '((bash "https://github.com/tree-sitter/tree-sitter-bash" "c0f5797a728fc4ebd78a8b0e436b1494a8ab5f51") ; 0.20.0
;        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "f1e5a09b8d02f8209a68249c93f0ad647b228e6e") ; 0.20.1
;        (json "https://github.com/tree-sitter/tree-sitter-json" "ca3f8919800e3c1ad4508de3bfd7b0b860ce434f") ; master-20230710
;        (markdown "https://github.com/MDeiml/tree-sitter-markdown" "aaf76797aa8ecd9a5e78e0ec3681941de6c945ee") ; 0.1.6
;        (toml "https://github.com/tree-sitter/tree-sitter-toml" "342d9be207c2dba869b9967124c679b5e6fd0ebe") ; master-20220421
;        (rust "https://github.com/tree-sitter/tree-sitter-rust" "2697585ee0a6a07a9c762d8855022f974c831a89") ; 0.20.4
;        (yaml "https://github.com/ikatyang/tree-sitter-yaml" "0e36bed171768908f331ff7dff9d956bae016efb"))) ; master-20210510

; M-: (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (markdown "https://github.com/MDeiml/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;;; Packages
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

;;; use-package
(eval-when-compile
  (require 'use-package))

(use-package avy
  :defer t
  :straight t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)
         ("C-c C-j" . avy-resume))
  :config
  (avy-setup-default))

(use-package consult
  :defer t
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
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
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
  :defer t
  :straight t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :defer t
  :straight t
  :init
  (setq completion-styles '(substring orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :defer t
  :straight t
  :init
  (vertico-mode))

(use-package flymake
  :defer t
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

(use-package json-ts-mode
  :defer t
  :mode (".json"))

(use-package js-ts-mode
  :defer t
  :config
  (setq js-indent-level 2)
  :mode (".js" ".jsx"))

(use-package flow-minor-mode
  :defer t
  :straight '(flow-minor-mode
              :type git
              :host github
              :repo "an-sh/flow-minor-mode")
  :hook (js-ts-mode . flow-minor-enable-automatically))

(use-package yaml-ts-mode
  :defer t
  :mode (".yml" ".yaml"))

(use-package rust-ts-mode
  :straight t
  :defer t
  :config
  (setq rust-format-on-save t)
  :mode (".rs"))

(use-package eglot
  :defer t
  :config
  ; Seems to be the only way to prevent flymake-eslint from being overriden
  ; Unfortunately, https://github.com/orzechowskid/flymake-eslint/issues/23#issuecomment-1675481378
  ; doesn't appear to work (see commented out attempt below) so we prevent Eglot from taking over all modes.
  ; https://github.com/joaotavora/eglot/issues/268#issuecomment-544890756
  ; Doesn't appear to be playing well with other modes, gate behind env var for now
  (when (string= (getenv "ESLINT") "1")
    (setq eglot-stay-out-of '(flymake))
    (add-hook 'eglot--managed-mode-hook (lambda () (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t))))
  :hook ((js-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               `(rust-ts-mode . ("rust-analyzer" :initializationOptions
                              ( :procMacro (:enable t)
                                :cargo ( :buildScripts (:enable t)
                                         :features "all"))))))

;(use-package flymake-eslint
;  :straight t
;  :hook
;  (eglot-managed-mode . (lambda ()
;                          (when (derived-mode-p 'js-ts-mode)
;                            (flymake-eslint-enable)))))

(use-package flymake-eslint
  :straight t
  :hook (js-ts-mode . flymake-eslint-enable))
 
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)))

(load-theme 'leuven)

(provide 'init)
;;; init.el ends here
