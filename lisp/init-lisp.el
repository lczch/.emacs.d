;;------------------------------------------------------------------------------
;; lisp families
;;------------------------------------------------------------------------------
;; (use-package paredit
;;   :disabled t
;;   :commands (paredit-on paredit-off)
;;   :config
;;   (defun paredit-on ()
;;     (interactive)
;;     (paredit-mode 1))
;;   (defun paredit-off ()
;;     (interactive)
;;     (paredit-mode 0))
  
;;   (define-key paredit-mode-map [remap kill-line] 'paredit-kill)
;;   (define-key paredit-mode-map (kbd "C-j") 'eval-print-last-sexp)
;;   (define-key paredit-mode-map (kbd "RET") 'paredit-newline)
;;   (define-key paredit-mode-map [remap backward-kill-sentence] nil))

;;------------------------------------------------------------------------------
;; define common features of lisp-mode and repl-mode
;;------------------------------------------------------------------------------
(defvar rice-wine-lisp-repl-map 
  (let ((km (make-sparse-keymap)))
    ;; init rice-wine-lisp-repl-map
    (define-key km (kbd "M-RET") 'newline-and-indent)
    (define-key km (kbd "M-<return>") 'newline-and-indent)
    km)
  "keymap holding keybindings which must be avaliable in all repl-mode,
mainly used to modify some original keybindings.
See `rice-wine-lisp-repl-mode'.")

(define-minor-mode rice-wine-lisp-repl-mode
  "Toggle rice-wine-lisp-repl-mode.
This minor mode is mainly used to make some keybindings(in `rice-wine-lisp-repl-map') available in all lisp repl mode"
  :init-value nil
  :keymap rice-wine-lisp-repl-map)

(defvar rice-wine-lisp-emulation-alist
  `((rice-wine-lisp-repl-mode . ,rice-wine-lisp-repl-map))
  "Using `emulation-mode-map-alists' to make `rice-wine-lisp-repl-map' have highest priority against other minor modes.
PS: The form of `emulation-mode-map-alists' is a bit tricky, it does not accept variable holding a keymap. 
For example, `rice-wine-lisp-repl-map' must be evaluated manually. ")

(add-to-list 'emulation-mode-map-alists 'rice-wine-lisp-emulation-alist t) ;; add to the end of emulation-mode-map-alists

(defun rice-wine-lisp-repl-func ()
  "common fetures shared by all lisp repl mode"
  (smartparens-strict-mode))

;;------------------------------------------------------------------------------
;; common-lisp and elisp
;;------------------------------------------------------------------------------
(use-package lisp-mode
  :ensure nil
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("\\.lisp\\'" . lisp-mode)
         ("\\.asd\\'" . lisp-mode))
  :hook (((emacs-lisp-mode lisp-mode) . smartparens-strict-mode)
         ((emacs-lisp-mode lisp-mode) . eldoc-mode))
  :config
  (defvar elisp-company-backends
    '(company-elisp
      ;; 在笔记本上用时很慢, 不知道为什么...
      ;; company-files
      ))

  (defvar slime-company-backends
    '(company-slime company-files))
  
  (defun rice-wine-emacs-lisp-func ()
    (setup-company-mode elisp-company-backends)
    (local-set-key (kbd "C-x C-e") 'pp-eval-last-sexp))

  (defun rice-wine-common-lisp-func ()
    (local-set-key (kbd "C-j") 'slime-eval-print-last-expression)
    (setup-company-mode slime-company-backends)
    ;; (setq indent-line-function 'common-lisp-indent-function)
    ;; (put 'if 'common-lisp-indent-function 2)
    )

  (use-package slime
    :commands (slime slime-mode)
    :hook ((lisp-mode . slime-mode))
    :init
    (require 'slime-autoloads)
    (setq slime-contribs
          '(slime-fancy
            slime-company
            slime-asdf
            slime-autodoc))
    :config
    (setq inferior-lisp-program "sbcl")
    ;; (setq inferior-lisp-program "ros -Q run")
    (setq slime-net-coding-system 'utf-8-unix)

    (defun rice-wine-slime-repl-func ()
      (ricex-wine-lisp-repl-func)
      (setup-company-mode slime-company-backends))

    (add-hook 'slime-repl-mode-hook 'rice-wine-slime-repl-func))
  )




;; (use-package racket-mode
;;   :mode (("\\.rkt\\'" . racket-mode))
;;   :disabled t
;;   :init
;;   (use-package s
;;     :defer t)
;;   :config
;;   (defun rice-wine-racket-func ()
;;     (rice-wine-lisp-func)
;;     (company-mode 1))
  
;;   (add-hook 'racket-mode-hook 'rice-wine-racket-func))

;;------------------------------------------------------------------------------
;; scheme and racket
;;------------------------------------------------------------------------------

(use-package geiser
  :disabled
  :commands (geiser run-racket geiser-mode)
  :config
  (defvar geiser-company-backends
    '(company-capf company-dabbrev))

  (defun rice-wine-geiser-repl-func ()
    (rice-wine-lisp-repl-func)
    (setup-company-mode geiser-company-backends)
    )
  
  (add-hook 'geiser-repl-mode-hook 'rice-wine-geiser-repl-func))

(use-package scheme
  :disabled
  :mode (("\\.scm\\'" . scheme-mode))
  :config
  ;; I don't know when and how the company-mode is turned on. 
  
  (defun rice-wine-scheme-func ()
    (rice-wine-lisp-func)
    (geiser-mode)
    (setup-company-mode geiser-company-backends))

  (add-hook 'scheme-mode-hook 'rice-wine-scheme-func))

(use-package racket-mode
  :mode (("\\.rkt\\'" . racket-mode))
  :config
  ;; use racket-mode's font-lock, and geiser's repl
  (setq racket-mode-map (make-sparse-keymap))

  (defun rice-wine-racket-func ()
    (rice-wine-lisp-func)
    (geiser-mode)
    (setup-company-mode geiser-company-backends))

  (add-hook 'racket-mode-hook 'rice-wine-racket-func))

;;------------------------------------------------------------------------------
;; clojure: not very complete
;;------------------------------------------------------------------------------
(use-package clojure-mode
  :disabled
  :mode (("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljx\\'" . clojurex-mode)
         ("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode))
  :config
  (defvar clojure-company-backends
    '(company-capf company-files))
  
  (defun rice-wine-clojure-func ()
    (rice-wine-lisp-func)
    (setup-company-mode clojure-company-backends)
    (eldoc-mode)
    (local-set-key (kbd "C-j") 'cider-eval-print-last-sexp)
    )

  (use-package cider
    :config
    (defun rice-wine-cider-repl-func ()
      (rice-wine-lisp-repl-func)
      (setup-company-mode clojure-company-backends)
      (eldoc-mode))

    (add-hook 'cider-repl-mode-hook 'rice-wine-cider-repl-func))
  
  (add-hook 'clojure-mode-hook 'rice-wine-clojure-func)
  )

(provide 'init-lisp)
