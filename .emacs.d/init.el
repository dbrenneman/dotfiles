(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq visible-bell t)
(column-number-mode)
(global-display-line-numbers-mode t)
(global-hl-line-mode 1)
(global-visual-line-mode 1)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Source Code Pro" :height 140)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(use-package server
  :ensure t
  :hook (after-init . server-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package doom-themes
  ;;:init (load-theme 'doom-xcode t))
  ;;:init (load-theme 'doom-palenight t))
  :init (load-theme 'doom-one t))

(use-package all-the-icons
  :if (display-graphic-p)
  :config (interactive) (all-the-icons-install-fonts t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :diminish
  :config
  (counsel-mode 1))

;;; Snippets ;;;
(use-package yasnippet
  :ensure t
  :custom (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Spell check code buffers
(add-hook 'prog-mode-hook
    (lambda ()
    (flyspell-prog-mode)
    ))

;; Spell check text buffers
(add-hook 'text-mode-hook
    (lambda ()
    (flyspell-mode)
    ))

(use-package flycheck
  :ensure t
  :diminish
  (global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "s-l")
  :custom
  (lsp-enable-which-key-integration t)
  (lsp-auto-guess-root t)
  (lsp-treemacs-sync-mode 1)
  )

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))
  ;; :custom
  ;; (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :ensure t
  :after lsp)

(use-package lsp-ivy
  :ensure t
  :after lsp)

(use-package company
      :after lsp-mode
      :hook (lsp-mode . company-mode)
      :bind
      (:map company-active-map
            ("<tab>" . company-complete-selection))
      (:map lsp-mode-map
            ("<tab>" . company-indent-or-complete-common))
      :custom
      (company-minimum-prefix-length 1)
      (company-idle-delay 0.0))

(use-package magit
  :ensure t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

(use-package git-gutter-fringe
  :ensure t)

;;; Golang config ;;;
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :custom
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t))))

;;; Rust config ;;;
(use-package rustic
  :ensure t)

(use-package protobuf-mode
  :ensure t)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(doom-modeline ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
