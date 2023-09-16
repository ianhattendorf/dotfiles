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

;; Display line infomation
(global-display-line-numbers-mode 1)
(line-number-mode 1)
(column-number-mode 1)

;; Git symbolic link visit real file
(setq vc-follow-symlinks t)

(global-set-key (kbd "s-u") 'revert-buffer)
(global-unset-key (kbd "C-z"))

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

(use-package flymake
  :defer t
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

(use-package json-mode
  :defer t
  :mode (".json"))

(use-package js
  :defer t
  :config
  (setq js-indent-level 2))

(use-package flow-minor-mode
  :defer t
  :straight '(flow-minor-mode
              :type git
              :host github
              :repo "an-sh/flow-minor-mode")
  :hook (js-mode . flow-minor-enable-automatically))

(use-package yaml-mode
  :defer t
  :mode (".yml" ".yaml"))

(use-package eglot
  :defer t
  :config
  ; Seems to be the only way to prevent flymake-eslint from being overriden
  ; Unfortunately, https://github.com/orzechowskid/flymake-eslint/issues/23#issuecomment-1675481378
  ; doesn't appear to work (see commented out attempt below) so we prevent Eglot from taking over all modes.
  ; https://github.com/joaotavora/eglot/issues/268#issuecomment-544890756
  (setq eglot-stay-out-of '(flymake))
  :hook ((eglot-managed-mode . (lambda () (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)))
         (js-mode . eglot-ensure)))

;(use-package flymake-eslint
;  :straight t
;  :hook
;  (eglot-managed-mode . (lambda ()
;                          (when (derived-mode-p 'js-mode)
;                            (flymake-eslint-enable)))))

(use-package flymake-eslint
  :straight t
  :hook (js-mode . flymake-eslint-enable))
 
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)))

(load-theme 'leuven)

(provide 'init)
;;; init.el ends here
