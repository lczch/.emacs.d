;; -*- lexical-binding: t -*-

;; Mode-line
(require 'init-modeline)
;;; color theme
(use-package color-theme-sanityinc-solarized
  :demand t
  :config
  (setq-default custom-enabled-themes '(sanityinc-solarized-dark))
  (load-theme 'sanityinc-solarized-dark t)
  
  (defun light ()
    "Activate a light color theme."
    (interactive)
    (color-theme-sanityinc-solarized-light))
  
  (defun dark ()
    "Activate a dark color theme."
    (interactive)
    (color-theme-sanityinc-solarized-dark))
  )

;;; column number
(column-number-mode t)

;;; highlight balanced parenthesis
(show-paren-mode t)

;effective emacs item 7; no scrollbar, no menubar, no toolbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;;; set the line space: the space between lines. Extremely important for comfortable!
(setq-default line-spacing 0.2)

;;; init startup window
(setq-default initial-scratch-message
              (concat ";; Happy hacking " (or user-login-name "") " - Emacs loves you!\n\n"))
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; hooks about frame
;; from redguardtoo's configuration
(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")
(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Selectively run either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (select-frame frame)
  (run-hooks (if window-system
                 'after-make-window-system-frame-hooks
               'after-make-console-frame-hooks)))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(defun rw-maximize-frame ()
  "maximize frame"
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized))

(add-hook 'after-make-window-system-frame-hooks 'rw-maximize-frame)

(provide 'init-gui-frame)
