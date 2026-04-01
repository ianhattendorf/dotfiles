;;; early-init.el --- Early initialization  -*- lexical-binding: t; -*-

;;; Commentary:

;; Runs before init.el and before the GUI/package system initialize.

;;; Code:

;; Set garbage collection threshold to 1GB (restored by idle timer in init.el)
(setq gc-cons-threshold #x40000000)

;; Prevent package.el from loading packages before straight.el
(setq package-enable-at-startup nil)

;; Disable GUI elements early to prevent flash of UI
(when (fboundp 'menu-bar-mode) (menu-bar-mode t))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Disable splash screen
(setq inhibit-startup-screen t)

;; Native compilation
(setq native-comp-async-report-warnings-errors 'silent)
(setq native-compile-prune-cache t)

;; lsp-mode plist optimization
(setenv "LSP_USE_PLISTS" "true")

;;; early-init.el ends here
