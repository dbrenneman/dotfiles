;; .emacs
;; =========================================================================

;; Disable menu bar, etc...
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Quiet Startup
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

(defun display-startup-echo-area-message ()
  (message ""))

;; use bat as the pager within emacs buffers
(setenv "PAGER" "bat")

;; Use UTF-8 encoding
(prefer-coding-system 'utf-8-unix)
(set-locale-environment "en_US.UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq save-buffer-coding-system 'utf-8-unix)
(setq process-coding-system-alist
  (cons '("grep" utf-8 . utf-8) process-coding-system-alist))

;; Show trailing whitespaces
(setq-default show-trailing-whitespace t)

;; Always show line numbers
(global-display-line-numbers-mode)

(setq-default fill-column 80)
(setq auto-fill-mode 1)

;; visual line mode
(global-visual-line-mode 1) ; 1 for on, 0 for off.

; highlight current line
(global-hl-line-mode 1)

; disable opening of new files in a new frame for nextstep/macos
(setq ns-pop-up-frames nil)

; highlight parenthesis pairs
(show-paren-mode 1)
(setq paren-sexp-mode 'never)

;; turn on automatic bracket insertion by pairs.
(electric-pair-mode 1)

;; dont show passwords in clear text
(add-hook 'comint-output-filter-functions
                    'comint-watch-for-password-prompt)

;; last lines should end in a carriage return
(setq require-final-newline t)
;; do not add new lines with arrow down at end of buffer
(setq next-line-add-newlines nil)
; highlight during query
(setq query-replace-highlight t)
; highlight incremental search
(setq search-highlight t)

;; Send kill ring buffer text to macos and place macos clipboard on kill ring buffer.
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; Answer yes or no questions with y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; always revert buffers if their files change on disk to reflect new changes
(global-auto-revert-mode 1)

;; Store backup files in a central location
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.files/bak/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   )       ; use versioned backups

;; Keybindings
;;
(global-set-key "\C-c1" 'find-grep-dired)
(define-key global-map "\M-g" 'goto-line)
(define-key global-map "\M-%"  'query-replace)
(define-key global-map "\M- " 'hippie-expand)
(define-key global-map "\M-j" 'join-line)

;Make the prompt read only
(setq comint-prompt-read-only t)

;; no beep
(setq ring-bell-function 'ignore)

(setq scroll-step 1)

(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)
       (setq font-lock-support-mode 'font-lock-mode)
       (setq lazy-lock-defer-on-scrolling nil)
       (setq lazy-lock-defer-time 1)
       (setq lazy-lock-stealth-time 20)
       (setq lazy-lock-stealth-lines 250)
       (setq lazy-lock-stealth-verbose nil)
       (setq font-lock-stealth-time 20)
       (setq font-lock-stealth-lines 250)
       (require 'font-lock)
))

;; Truncate lines, see http://www.emacswiki.org/cgi-bin/wiki/TruncateLines
(setq truncate-partial-width-windows nil)

;; Package setup
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa-stb" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/"))
      tls-checktrust t
      tls-program '("gnutls-cli --x509cafile %t -p %p %h")
      gnutls-verify-error t)

(package-initialize)
(require 'package)

(defvar package-list)
(setq package-list
      '(
        ;;; General. ;;;
	diminish
	flycheck                ;; Linter.
        yasnippet               ;; Snippet management.
	yasnippet-snippets
	whitespace
	fill-column-indicator
	highlight-indent-guides
	lsp-mode                ;; Language Server Protocol Support
	lsp-ui
	company
        multiple-cursors        ;; Multi cursor.
        switch-buffer-functions ;; Add hook when switchin buffers.
	projectile
	git-gutter              ;; Display / manage git changes.
	magit                   ;; Git client.
	vertico
	marginalia
	savehist
	orderless
	go-mode                 ;; Go major mode.
	ag

        ;;; Themes. ;;;
        monokai-theme
        solarized-theme
        powerline

        ;;; Various modes. ;;;
        dockerfile-mode
        markdown-mode
	protobuf-mode
	yaml-mode
        json-mode
	terraform-mode
	toml-mode

	use-package
))

;; Fetch the list of packages available.
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages.
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; run an emacs server
(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;; Set up key environment variables
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "GOPRIVATE" "EDITOR" "PAGER"))
  (exec-path-from-shell-initialize))

;;; Load & configure themes. ;;;
;; Functions allow to easily switch between dark/light themes.
(defun dark-theme()
  (interactive)                    ;; Allow function call from M-x.
  (disable-theme 'solarized-light) ;; Disable light theme.
  (load-theme 'monokai t)          ;; Load Monokai.
  (enable-theme 'monokai)          ;; Enable Monokai.
  (powerline-default-theme)        ;; Powerline layout.
  (custom-set-faces                ;; Tweak faces.
   '(default ((t (:background "#101010"))))                                                          ;; Slightly increase contrast.
   '(flycheck-error   ((t (:background "#FF6E64" :foreground "#990A1B" :underline t :weight bold)))) ;; Improve flycheck render.
   '(flycheck-info    ((t (:background "#69B7F0" :foreground "#00629D" :underline t :weight bold)))) ;; Improve flycheck render.
   '(flycheck-warning ((t (:background "#DEB542" :foreground "#7B6000" :underline t :weight bold)))) ;; Improve flycheck render.
   )
  )
(defun light-theme()
  (interactive)                    ;; Allow function call from M-x.
  (disable-theme 'monokai)         ;; Disable dark theme.
  (load-theme 'solarized-light t)  ;; Load Solarized.
  (enable-theme 'solarized-light)  ;; Enable Solarized.
  (powerline-default-theme)        ;; Powerline layout.
  (custom-set-faces                ;; Reset default faces for solarized.
   '(default ((t (:background "#FDF6E3"))))
   '(flycheck-error   ((t (:background "#FF6E64" :foreground "#990A1B" :underline t :weight bold))))
   '(flycheck-info    ((t (:background "#69B7F0" :foreground "#00629D" :underline t :weight bold))))
   '(flycheck-warning ((t (:background "#DEB542" :foreground "#7B6000" :underline t :weight bold))))
   )
  )
;; Default to dark theme.
(dark-theme)

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  )

;; Use the `orderless' completion style.
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  )

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(require 'diminish)

;;; Highlight Whitespace ;;;
(require 'whitespace)
(diminish 'global-whitespace-mode)
(setq whitespace-style '(face empty tabs trailing))
(global-whitespace-mode t)

;;; Highlight Indent Guides ;;;
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-responsive 'top)
(setq highlight-indent-guides-delay 0)

;;; Fill Column Indicator ;;;
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;;; Projectile ;;;
(projectile-mode +1)
(diminish 'projectile-mode)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;; Git gutter config. ;;;
(global-git-gutter-mode +1)
(diminish 'git-gutter-mode)

;;; Magit config. ;;;
(remove-hook 'find-file-hook 'vc-find-file-hook) ;; disable vc-mode
(global-set-key (kbd "C-x g") 'magit-status)

;;; Ediff config. ;;;
(setq-default ediff-highlight-all-diffs 'nil) ;; Only hilight current diff:
(setq ediff-diff-options "-w")                ;; Turn off whitespace checking:
(setq ediff-show-clashes-only t)              ;; Default to conflict diff.

;;; Flycheck config. ;;;
(add-hook 'after-init-hook 'global-flycheck-mode)            ;; Enable flycheck everywhere.
(global-set-key (kbd "C-c <up>")   'flycheck-next-error)     ;; Ctrl-up   to go to next error.
(global-set-key (kbd "C-c <down>") 'flycheck-previous-error) ;; Ctrl-down to go to previous error.
(global-set-key (kbd "C-c l")      'flycheck-list-errors)    ;; Ctrl-l    to display error list.
(setq flycheck-check-syntax-automatically '(mode-enabled idle-change save))
(setq flycheck-display-errors-delay 1)
(setq flycheck-idle-buffer-switch-delay 10)
(setq flycheck-idle-change-delay 10)


;;; LSP ;;;
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :config (progn
           ;; use flycheck, not flymake
           (setq lsp-prefer-flymake nil))
  :custom
  (lsp-auto-guess-root t)
  (lsp-register-custom-settings
   '(
     ("gopls.completeUnimported" t t)
     ("gopls.deepCompletion" t t)
     ("gopls.staticcheck" t t)
     ("gopls.analyses"
       '(
	 ("fieldalignment" t)
	 ("shadow" t)
	 )
       )
     )
   )
  (lsp-register-client
  (make-lsp-client :new-connection (lsp-stdio-connection '("/usr/local/bin/terraform-ls" "serve"))
                   :major-modes '(terraform-mode)
                   :server-id 'terraform-ls))

  (add-hook 'terraform-mode-hook #'lsp)

)
(setq lsp-eldoc-render-all t)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(setq lsp-ui-doc-enable nil
      lsp-ui-flycheck-enable t)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0.6)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  )
(add-hook 'after-init-hook 'global-company-mode)
(diminish 'company-mode)

(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

;;; Snippets ;;;
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode)
  )

(diminish 'yas-minor-mode)

;;; Golang config ;;;
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (add-hook 'go-mode-hook 'highlight-indent-guides-mode)

  :bind (
         ("C-c C-j" . lsp-find-definition)
         ("C-c C-d" . lsp-describe-thing-at-point)
         )
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  )

;;; End of Golang config ;;

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
;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#101010"))))
 '(flycheck-error ((t (:background "#FF6E64" :foreground "#990A1B" :underline t :weight bold))))
 '(flycheck-info ((t (:background "#69B7F0" :foreground "#00629D" :underline t :weight bold))))
 '(flycheck-warning ((t (:background "#DEB542" :foreground "#7B6000" :underline t :weight bold)))))

(provide '.emacs)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(marginalia yasnippet-snippets yaml-mode vertico use-package toml-mode terraform-mode switch-buffer-functions solarized-theme protobuf-mode projectile powerline orderless multiple-cursors monokai-theme magit lsp-ui json-mode highlight-indent-guides go-mode git-gutter flycheck fill-column-indicator exec-path-from-shell dockerfile-mode diminish company ag)))
