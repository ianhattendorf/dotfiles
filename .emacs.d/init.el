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
