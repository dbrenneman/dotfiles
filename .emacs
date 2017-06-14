;; .emacs
;; =========================================================================

;; Setup "path"
(defun set-exec-path-from-shell-PATH ()
    "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
    (interactive)
    (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
          (setq exec-path (split-string path-from-shell path-separator))))
(setenv "PATH" (concat "/Users/dbrenneman/Python/CPython-2.7.10/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/Users/dbrenneman/Python/CPython-2.7.10/bin")
(setenv "PATH" (concat "/opt/twitter/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/opt/twitter/bin")

;; use cat as the pager within emacs buffers
(setenv "PAGER" "cat")

;; Disable menu bar, etc...
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq inhibit-startup-message t)
(winner-mode 1)

;;; enable mouse  ;;;
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)
(global-set-key [mouse-4] (lambda ()
 			    (interactive)
 			    (scroll-down 5)))
(global-set-key [mouse-5] (lambda ()
 			    (interactive)
 			    (scroll-up 5)))
;;; End of mouse setup ;;;

;; Use UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq current-language-environment "UTF-8")
(setenv "LC_CTYPE" "UTF-8")

;; force emacs to always use spaces instead of tab characters
(setq-default indent-tabs-mode nil);

;; set default tab width to 2 spaces
(setq tab-width 2)
(setq python-indent 2)
(setq python-indent-offset 2)
;; Show trailing whitespaces

(setq-default show-trailing-whitespace t)

;; Package setup
(add-to-list 'load-path "~/.emacs.d/plugins")
(require 'package)
(setq
 use-package-always-ensure t
 package-archives '(("org" . "http://orgmode.org/elpa/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; run an emacs server
(server-start)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)


;; Snippets!

(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; set default font size
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 100 :family "Menlo")))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Unique Buffer Names - makes navigation of open buffers easier
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; Answer yes or no questions with y or n
(fset 'yes-or-no-p 'y-or-n-p)

;;Place all backup copies of files in a common location
(defconst use-backup-dir t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs-meta/backups/")))
      version-control t                ; Use version numbers for backups
      kept-new-versions 8             ; Number of newest versions to keep
      kept-old-versions 2              ; Number of oldest versions to keep
      delete-old-versions t            ; Ask to delete excess backup versions?
      backup-by-copying-when-linked t) ; Copy linked files, don't rename.

;; always revert buffers if their files change on disk to reflect new changes
(global-auto-revert-mode 1)

;; (eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
(setq vc-handled-backends 'nil)

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

(line-number-mode 1)
(column-number-mode 1)
(setq-default fill-column 80)
(setq auto-fill-mode 1)
(setq scroll-step 1)

; visual line mode
(global-visual-line-mode 1) ; 1 for on, 0 for off.

; highlight current line
(global-hl-line-mode 1)

; disable opening of new files in a new frame for nextstep/macos
(setq ns-pop-up-frames nil)

; highlight parenthesis pairs
(show-paren-mode 1)
(setq paren-sexp-mode 'never)

;; turn on automatic bracket insertion by pairs. New in emacs 24
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

;;
;; ido configuration
;;
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(icomplete-mode 1)

(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-default-buffer-method 'samewindow)
(setq ido-show-dot-for-dired 1)
(setq ido-confirm-unique-completion 0)
(setq ido-ignore-extensions 1)

(add-to-list 'ido-ignore-buffers "^ ")
(add-to-list 'ido-ignore-buffers "*Messages*")
(add-to-list 'ido-ignore-buffers "*Buffer*")
(add-to-list 'ido-ignore-buffers "*Completions*")
(add-to-list 'ido-ignore-buffers "*About*")

(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)
(global-set-key [(f12)] 'ibuffer)


;; Python Mode Setup

;; Python Auto Syntax Error Highlight
(use-package flycheck
  :ensure t
  :defer t
  :preface (progn
             (defun check-source-predicate ()
               (and (executable-find "check.pex")
                    (buffer-file-name)
                    (string-match "workspace/source/.*\.py$" (buffer-file-name)))))
  :init
  (progn
    (add-hook 'prog-mode-hook 'flycheck-mode)
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  :config
  (progn
    (flycheck-define-checker source-check
      "A syntax checker for python source code in Source, using `check.pex'"
      :command ("check.pex" source)
      ;;; errors are reported like this:
      ;;; <ID>:ERROR   <file name>:<line> <message>
      :error-patterns ((error line-start (id (1+ nonl)) ":ERROR" (1+ nonl) ":" line (message) line-end)
                       (warning line-start (id (1+ nonl)) ":WARNING" (1+ nonl) ":" line (message) line-end))
      :predicate check-source-predicate
      :modes (python-mode))
    (add-to-list 'flycheck-checkers 'source-check)))

;; Scala Setup
(use-package ensime
  :commands ensime ensime-mode)
(add-hook 'scala-mode-hook 'ensime-mode)

;;; Golang config ;;;
;; Load go-mode
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(global-set-key (kbd "M-*") 'pop-tag-mark)

;;(add-to-list 'yas-snippet-dirs "~/.emacs.files/yasnippet-go")
(yas-global-mode 1)


(add-hook 'go-mode-hook
	  (lambda()
	    ;; Convenient binding for go
	    (global-set-key (kbd "C-c C-i") 'go-goto-imports)
	    (global-set-key (kbd "C-c C-e") 'go-rename)
	    (global-set-key (kbd "C-c d") 'godoc-at-point)
	    (global-set-key (kbd "C-c c") '(lambda() (interactive) (go-coverage "coverprofile")))

	    ;; Go helper for compilation
	    (global-set-key (kbd "C-c f") 'save-and-compile-program)
	    (global-set-key (kbd "C-c C-t") 'save-and-test-program)
	    (global-set-key (kbd "C-c t")   'save-and-make-test-program)
	    (global-set-key (kbd "C-c b")   'save-and-make-clean-program)
	    (global-set-key (kbd "C-c C-m") 'save-and-make-program)

	    )
	  )

;; Kill compilation upon recompile.
(setq compilation-always-kill t)

(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-set-key (kbd "C-c C-r") 'my-recompile)
(global-set-key (kbd "C-c C-k") 'kill-compilation)
(global-set-key (kbd "C-c C-l") 'linum-mode)

(defun end-of-line-compile()
  (setq curbuf (current-buffer))
  (pop-to-buffer "*compilation*")
  (end-of-buffer)
  (pop-to-buffer curbuf)
  )

;; save all files then run M-x compile
(defun my-recompile()
        "Save any unsaved buffers and compile"
        (interactive)
        (save-some-buffers t)
	(end-of-line-compile)
        (recompile)
	(end-of-line-compile))

(defun save-and-compile-program()
        "Save any unsaved buffers and compile"
        (interactive)
        (save-some-buffers t)
        (compile "bash -c 'go install && go build -o /tmp/a.out && /tmp/a.out'")
	(end-of-line-compile))

(defun save-and-test-program()
        "Save any unsaved buffers and compile"
        (interactive)
        (save-some-buffers t)
        (compile "go test -v -cover -coverprofile=coverprofile -covermode=count")
	(end-of-line-compile))

(defun save-and-make-test-program()
        "Save any unsaved buffers and compile"
        (interactive)
        (save-some-buffers t)
        (compile "make test SKIP_FMT=1 NOPULL=1 TEST_OPTS='-v .'")
	(end-of-line-compile))


(defun save-and-make-clean-program()
        "Save any unsaved buffers and compile"
        (interactive)
        (save-some-buffers t)
        (compile "make clean")
	(end-of-line-compile))

(defun save-and-make-program()
        "Save any unsaved buffers and compile"
        (interactive)
        (save-some-buffers t)
        (compile "make start NOPULL=1")
	(end-of-line-compile))

(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook 'go-guru-hl-identifier-mode)

;;; End of Golang config ;;

;;; Setup auto-complete ;;;
(require 'auto-complete)
(require 'go-autocomplete)
(require 'auto-complete-config)
;;(setq ac-source-yasnippet nil)
(ac-config-default)
(setq ac-delay 0.1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dockerfile-mode protobuf-mode magit go-mode go-autocomplete flycheck exec-path-from-shell ensime))))

;; Setup modes.
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"      . web-mode))
(add-to-list 'auto-mode-alist '("\\.mk\\'"      . makefile-mode))

;; Load powerline.
(powerline-default-theme)

(global-set-key (kbd "C-x g") 'magit-status)
