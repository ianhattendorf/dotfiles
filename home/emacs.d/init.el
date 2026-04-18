;;; init.el --- Initialization file for Emacs

;;; Commentary:

;; Emacs startup file

;;; Code:

;;; Bootstrap

(eval-when-compile
  (require 'use-package))

(use-package emacs
  :init
  ;; When idle for 15sec run the GC no matter what.
  (defvar k-gc-timer
    (run-with-idle-timer 15 t 'garbage-collect))

  ;; Garbage collect on blur
  (add-function :after
                after-focus-change-function
                (lambda () (unless (frame-focus-state) (garbage-collect))))

  ;; Increase process read buffer
  (setq read-process-output-max (* 4 1024 1024))

  ;; Performance: skip bidi for LTR-only text
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t)

  ;; Defer fontification during fast input
  (setq redisplay-skip-fontification-on-input t)

  ;; straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/09b789a8596cacca6bbff866500373541a85ffa2/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; Configure defaults
  (customize-set-variable 'user-full-name "Ian Hattendorf")
  (customize-set-variable 'user-mail-address "ian@ianhattendorf.com")
  (customize-set-variable 'initial-scratch-message nil)

  ;; Kill ring behavior
  (setq save-interprogram-paste-before-kill t
        kill-do-not-save-duplicates t)

  ;; Avoid polluting init.el with customizations
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file :noerror)

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

  ;; Hide commands in M-x which do not apply to the current mode.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Regex builder: string syntax (no double-escaping)
  (setq reb-re-syntax 'string)

  ;; Prevent ffap DNS lookup freezes
  (setq ffap-machine-p-known 'reject)

  ;; Proportional window resizing on split
  (setq window-combination-resize t)

  ;; After C-u C-SPC, just C-SPC to keep popping marks
  (setq set-mark-command-repeat-pop t)

  ;; Auto-focus help windows
  (setq help-window-select t)

  ;; No phantom cursors in unfocused windows
  (setq-default cursor-in-non-selected-windows nil)
  (setq highlight-nonselected-windows nil))

;;; Completion Framework

(use-package savehist
  :init
  (savehist-mode)
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring kill-ring))
  (defun savehist-unpropertize-kill-ring ()
    (setq kill-ring (mapcar #'substring-no-properties kill-ring)))
  (add-hook 'savehist-save-hook #'savehist-unpropertize-kill-ring))

(use-package saveplace
  :init
  (save-place-mode)
  :config
   (advice-add 'save-place-find-file-hook :after
              (lambda (&rest _)
                (when-let ((win (get-buffer-window (current-buffer) t)))
                  (with-selected-window win
                    (recenter))))))

 ;; (advice-add 'save-place-find-file-hook :after
 ;;              (lambda (&rest _)
 ;;                (when (and buffer-file-name
 ;;                           (eq (window-buffer) (current-buffer)))
 ;;                  (recenter)))))


  ;; (advice-add 'save-place-find-file-hook :after
  ;;             (lambda (&rest _) (when buffer-file-name (recenter)))))

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(substring orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

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
         ("M-g f" . consult-flymake)
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
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
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

(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :straight t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package embark
  :straight t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :straight t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; Navigation and Editing

(use-package winner
  :init
  (winner-mode +1)
  :config
  (defun toggle-delete-other-windows ()
    "Delete other windows, or restore previous layout via `winner-undo'."
    (interactive)
    (if (one-window-p)
        (winner-undo)
      (delete-other-windows)))
  (global-set-key (kbd "C-x 1") #'toggle-delete-other-windows))

(use-package avy
  :straight t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g w" . avy-goto-word-1)
         ("C-c C-j" . avy-resume))
  :config
  (avy-setup-default))

(use-package which-key
  :config
  (which-key-mode))

;;; Diagnostics

(use-package flymake
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

;;; Version Control

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)))

;;; Language Support

;;;; Tree-sitter

(use-package treesit-auto
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;;; C / C++

(use-package clang-format
  :straight t
  :bind (("C-c u" . clang-format-buffer)
         ("C-M-<tab>" . clang-format-region)))

;;;; JavaScript / TypeScript

(use-package js-mode
  :defer t
  :custom
  (js-indent-level 2))

(use-package flow-minor-mode
  :straight '(flow-minor-mode
              :type git
              :host github
              :repo "an-sh/flow-minor-mode")
  :hook (js-ts-mode . flow-minor-enable-automatically))

(use-package eslint-disable-rule
  :straight t)

;;;; Rust

(use-package rust-mode
  :straight t
  :config
  (setq rust-format-on-save t))

;;;; Other Languages

(use-package slint-mode
  :straight t
  :mode ("\\.slint\\'"))

(use-package terraform-mode
  :straight t
  :mode ("\\.tf\\'"))

(use-package dts-mode
  :straight t
  :mode ("\\.overlay\\'" "\\.dts\\'" "\\.dtsi\\'"))

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;;; LSP and DAP

(use-package eglot
  :hook ((c-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (terraform-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  :config
  (defvar eglot-format-on-save-modes
    '(c-mode c-ts-mode c++-mode c++-ts-mode rust-mode rust-ts-mode)
    "Modes for which eglot should format the buffer on save.")
  (defun eglot-format-on-save ()
    "Format buffer via eglot on save for selected modes."
    (when (and (bound-and-true-p eglot--managed-mode)
               (member major-mode eglot-format-on-save-modes))
      (eglot-format-buffer)))
  (add-hook 'before-save-hook #'eglot-format-on-save))

(use-package eglot-booster
  :straight '(eglot-booster
              :type git
              :host github
              :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode))

(use-package dape
  :straight t)

;;; Appearance

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
  (setq auto-dark-themes '((ef-elea-dark) (ef-cyprus)))
  (setq ef-themes-to-toggle '(ef-cyprus ef-elea-dark))
  (auto-dark-mode t))

(put 'downcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here
