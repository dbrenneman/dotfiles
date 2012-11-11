;; .emacs
;; =========================================================================
(add-to-list 'load-path
             "~/.emacs.d/plugins/")

;; Use UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq current-language-environment "UTF-8")

;; force emacs to always use spaces instead of tab characters
(setq-default indent-tabs-mode nil);

;; set default tab width to 4 spaces
(setq default-tab-width 4);
(setq tab-width 4);

;; Unique Buffer Names - makes navigation of open buffers easier
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; display time and date in the modeline
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

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

;; load saved desktop on startup and save buffers to desktop on exit
(load "desktop")
(desktop-save-mode 1)
(desktop-read)
(setq desktop-load-locked-desktop t)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))

;; save our place when closing a file
(require 'saveplace)
(setq-default save-place t)

;; always revert buffers if their files change on disk to reflect new changes
(global-auto-revert-mode 1)

;; Use SSH for remote-shell
(setq remote-shell-rpogram "ssh")
;;TRAMP should default to ssh
(setq tramp-default-method "ssh")

;; Magit
(add-to-list 'load-path
             "~/.emacs.d/plugins/magit")
(require 'magit)

;; Functions for configuring window geometry, placement and navigation

;; move around between windows easier
;; this makes it so you can use 'meta-arrow' to move focus between windows in a frame
(windmove-default-keybindings 'meta)

;; Window shifting. C-x-o lets us go forward a window (or several). This
;; one lets us go back one or more windows. From Glickstein.
(defun other-window-backward (&optional n)
  "Select previous Nth window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))
;; now bind it to C-x p
(global-set-key "\C-x\p" 'other-window-backward)
;; end window shifting.

;; -- Clipboard menu handling --
(menu-bar-enable-clipboard)
(setq x-select-enable-clipboard t)

;; Keybindings
;;
(global-set-key "\C-c1" 'find-grep-dired)
(global-set-key "\C-c2" 'magit-status)

(define-key global-map "\M-g" 'goto-line)
(define-key global-map "\M-%"  'query-replace-regexp)
(define-key global-map "\M- " 'hippie-expand)
(define-key global-map "\M-j" 'join-line) 
(define-key global-map "\M-m" 'xterm-mouse-mode) 
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)
(global-set-key (kbd "S-<left>") 'windmove-left)          ; move to left windnow
(global-set-key (kbd "S-<right>") 'windmove-right)        ; move to right window
(global-set-key (kbd "S-<up>") 'windmove-up)              ; move to upper window
(global-set-key (kbd "S-<down>") 'windmove-down)          ; move to downer window

;; Disable menu bar, etc...
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;Make the prompt read only
(setq comint-prompt-read-only t)

;; no beep
(setq ring-bell-function 'ignore)

(line-number-mode 1)
(column-number-mode 1)
(setq-default fill-column 80)
(setq auto-fill-mode 1)
(setq scroll-step 1)
(setq inhibit-startup-message t)

; visual line mode
(global-visual-line-mode 1) ; 1 for on, 0 for off.

; highlight current line
(global-hl-line-mode 1)

; disable opening of new files in a new frame for nextstep/macos
(setq ns-pop-up-frames nil)

; highlight parenthesis pairs
(show-paren-mode 1)
(setq paren-sexp-mode 'never)

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

;; recent files
;;recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(global-set-key [(meta f12)] 'recentf-open-files)

;; define a function to use recently opened files via ido
(defun db-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t))))

(global-set-key [(meta f11)] 'db-ido-choose-from-recentf)


;; Remember and Org Modes
(require 'org)
(require 'remember)
;(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Yasnippet
(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)

;; js2-mode for JavaScript development
(load "~/.emacs.d/plugins/js2-20090723b.el")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; lintnode for syntax checking JavaScript
(add-to-list 'load-path "~/.emacs.d/plugins/lintnode")
(require 'flymake-jslint)
;; Make sure we can find the lintnode executable
(setq lintnode-location "~/.emacs.d/plugins/lintnode")
;; JSLint can be... opinionated
(setq lintnode-jslint-excludes (list 'nomen 'plusplus 'onevar 'white))
;; Start the server when we first open a js file and start checking
(add-hook 'js-mode-hook
          (lambda ()
            (lintnode-hook)))

;; js-comint JavaScript REPL
;; M-x run-js or M-x send-region
(require 'js-comint)
;; Use node as our repl
(setq inferior-js-program-command "node")
 
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)))
;; Deal with some prompt nonsense
;;        (add-to-list 'comint-preoutput-filter-functions
;;                     (lambda (output)
;;                       (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
;;                                                 (replace-regexp-in-string ".*1G.*3G" "&gt;" output))))

;; Python programming features
;; Python Mode Setup
(add-to-list 'load-path "~.emacs.d/plugins/python")
(require 'python)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.cpy$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.vpy$" . python-mode))
(add-hook 'python-mode-hook
          (lambda ()
            (set (make-variable-buffer-local 'beginning-of-defun-function)
                 'py-beginning-of-def-or-class)
            (setq outline-regexp "def\\|class ")
            (define-key py-mode-map "\C-c#" 'comment-region)
            (eldoc-mode 1)
            (set-variable 'indent-tabs-mode nil)
            (define-key py-mode-map "\C-m" 'newline-and-indent)
            (electric-pair-mode)
            (linum-mode)
            ))

;; ipython shell
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; pymacs
(require 'pymacs)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")


;; Autofill inside of comments
(defun python-auto-fill-comments-only ()
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (python-in-string/comment)))))
(add-hook 'python-mode-hook
          (lambda ()
            (python-auto-fill-comments-only)))

;; Python Auto Syntax Error Highlight
;; courtesy of Chris McDonough
 (when (load "flymake" t)  
   (defun flymake-pyflakes-init ()  
     (let* ((temp-file (flymake-init-create-temp-buffer-copy  
                'flymake-create-temp-inplace))  
        (local-file (file-relative-name  
             temp-file  
             (file-name-directory buffer-file-name))))  
       (list "~/.emacs.d/pycheckers" (list local-file))))  
   (add-to-list 'flymake-allowed-file-name-masks  
            '("\\.py\\'" flymake-pyflakes-init)))  
 (add-hook 'find-file-hook 'flymake-find-file-hook)
(add-hook 'python-mode-hook (lambda () (unless (eq buffer-file-name nil) (flymake-mode 1))))

;; enhancements for displaying flymake errors
;; (require 'flymake-cursor)

;; (defadvice flymake-goto-next-error (after display-message activate compile)
;;   "Display the error in the mini-buffer rather than having to mouse over it"
;;   (show-fly-err-at-point))

;; (defadvice flymake-goto-prev-error (after display-message activate compile)
;;   "Display the error in the mini-buffer rather than having to mouse over it"
;;   (show-fly-err-at-point))

;; Use archive mode to open Python eggs
(add-to-list 'auto-mode-alist '("\\.egg\\'" . archive-mode))

;; Shell mode hacks cargo-culted from Ryan Barrett
;;

(defvar my-local-shells
  '("*shell*" "*shell1*" "*shell2*" "*shell3*"))
(defvar my-shells (append my-local-shells))

(require 'tramp)

(custom-set-variables
 '(tramp-default-method "ssh")          ; uses ControlMaster
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output nil) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)     ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 '(comint-buffer-maximum-size 20000)    ; max length of the buffer in lines
 '(comint-prompt-read-only nil)         ; if this is t, it breaks shell-command
 '(comint-get-old-input (lambda () "")) ; what to run when i press enter on a
                                        ; line above the current prompt
 '(comint-input-ring-size 5000)         ; max shell history size
)

(setenv "PAGER" "cat")

;; truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(defun make-my-shell-output-read-only (text)
  "Add to comint-output-filter-functions to make stdout read only in my shells."
  (if (member (buffer-name) my-shells)
      (let ((inhibit-read-only t)
            (output-end (process-mark (get-buffer-process (current-buffer)))))
        (put-text-property comint-last-output-start output-end 'read-only t))))
(add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)

(defun my-dirtrack-mode ()
  "Add to shell-mode-hook to use dirtrack mode in my shell buffers."
  (when (member (buffer-name) my-shells)
    (shell-dirtrack-mode 0)
    (set-variable 'dirtrack-list '("^.*[^ ]+:\\(.*\\)>" 1 nil))
    (dirtrack-mode 1)))
(add-hook 'shell-mode-hook 'my-dirtrack-mode)

; interpret and use ansi color codes in shell output windows
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun set-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
  (set (make-local-variable 'scroll-conservatively) 10))
(add-hook 'shell-mode-hook 'set-scroll-conservatively)

(defun make-comint-directory-tracking-work-remotely ()
  "Add this to comint-mode-hook to make directory tracking work
while sshed into a remote host, e.g. for remote shell buffers
started in tramp. (This is a bug fix backported from Emacs 24:
http://comments.gmane.org/gmane.emacs.bugs/39082"
  (set (make-local-variable 'comint-file-name-prefix)
       (or (file-remote-p default-directory) "")))
(add-hook 'comint-mode-hook 'make-comint-directory-tracking-work-remotely)

(defun enter-again-if-enter ()
  "Make the return key select the current item in minibuf and shell history isearch.
An alternate approach would be after-advice on isearch-other-meta-char."
  (when (and (not isearch-mode-end-hook-quit)
             (equal (this-command-keys-vector) [13])) ; == return
    (cond ((active-minibuffer-window) (minibuffer-complete-and-exit))
          ((member (buffer-name) my-shells) (comint-send-input)))))
(add-hook 'isearch-mode-end-hook 'enter-again-if-enter)

(defadvice comint-previous-matching-input
    (around suppress-history-item-messages activate)
  "Suppress the annoying 'History item : NNN' messages from shell history isearch.
If this isn't enough, try the same thing with
comint-replace-by-expanded-history-before-point."
  (let ((old-message (symbol-function 'message)))
    (unwind-protect
      (progn (fset 'message 'ignore) ad-do-it)
    (fset 'message old-message))))

(defadvice comint-send-input (around go-to-end-of-multiline activate)
  "When I press enter, jump to the end of the *buffer*, instead of the end of
the line, to capture multiline input. (This only has effect if
`comint-eol-on-send' is non-nil."
  (flet ((end-of-line () (end-of-buffer)))
    ad-do-it))

;; for other code, e.g. emacsclient in TRAMP ssh shells and automatically
;; closing completions buffers, see the links above.

;;
;; Appearance configuration
;;==========================
;; Make it pretty
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized" t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn-emacs" t)
(load-theme 'solarized-dark t)
;; (load-theme 'zenburn t)
