;; .emacs
;; =========================================================================

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

;;
;; Appearance configuration
;;==========================
;; Make it pretty
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized" t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn-emacs" t)
;; (load-theme 'solarized-dark t)
(load-theme 'zenburn t)


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
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Yasnippet
(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)


;; Python programming features

;; Python Mode Setup
(require 'python-mode)
(autoload 'python-mode "python-mode" "Python mode." t)
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
            (set-variable 'py-indent-offset 4)
            (set-variable 'py-smart-indentation nil)
            (set-variable 'indent-tabs-mode nil)
            (define-key py-mode-map "\C-m" 'newline-and-indent)
            (electric-pair-mode)
            (linum-mode)
            ))

;;python mode: go to the next code block
(defun py-next-block ()
   "go to the next block.  Cf. `forward-sexp' for lisp-mode"
   (interactive)
   (py-mark-block nil 't)
   (back-to-indentation))

;; Autofill inside of comments
(defun python-auto-fill-comments-only ()
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (python-in-string/comment)))))
(add-hook 'python-mode-hook
          (lambda ()
            (python-auto-fill-comments-only)))

;; Let python-mode know about xpdb and generator expressions.
(setq py-pdbtrack-input-prompt "\n[(<]*x?pdb[>)]+ "
      py-pdbtrack-stack-entry-regexp
      (concat "^> \\(.*\\)(\\([0-9]+\\))"
	      "\\([?a-zA-Z0-9_]+\\|<genexpr>\\)()"))


;; Set up Pymacs
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(require 'pymacs)


;; set up pycomplete
(require 'pycomplete)


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

;; Additional functionality that makes flymake error messages appear
;; in the minibuffer when point is on a line containing a flymake
;; error. This saves having to mouse over the error, which is a
;; keyboard user's annoyance

;;flymake-ler(file line type text &optional full-file)
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the
message in the minibuffer"
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
	  (let ((err (car (second elem))))
	    (message "%s" (fly-pyflake-determine-message err)))))))

(defun fly-pyflake-determine-message (err)
  "pyflake is flakey if it has compile problems, this adjusts the
message to display, so there is one ;)"
  (cond ((not (or (eq major-mode 'Python) (eq major-mode 'python-mode) t)))
	((null (flymake-ler-file err))
	 ;; normal message do your thing
	 (flymake-ler-text err))
	(t ;; could not compile err
	 (format "compile error, problem on line %s" (flymake-ler-line err)))))

(defadvice flymake-goto-next-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-mode (before post-command-stuff activate compile)
  "Add functionality to the post command hook so that if the
cursor is sitting on a flymake error the error information is
displayed in the minibuffer (rather than having to mouse over
it)"
  (set (make-local-variable 'post-command-hook)
       (cons 'show-fly-err-at-point post-command-hook))) 

;; ;; support for ReStructured text
;; (require 'rst)
;; (add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))
;; (add-to-list 'auto-mode-alist '("\\.txt$" . rst-mode))
;; (add-to-list 'auto-mode-alist '("\\.rest$" . rst-mode))
;; ;; add some syntax highlighting to restructured text documents
;; (setq rst-mode-lazy nil)
;; ;; automatically update the TOC in .rst documents
;; (add-hook 'rst-adjust-hook 'rst-toc-update)
;; ;; enable automatic completion of paired chars and spell checking for ReST documents
;; (add-hook 'rst-mode-hook (lambda () (textmate-paren-mode) (flyspell-mode) (linum-mode)))


;; ;; Python Doctest mode setup
;; (require 'doctest-mode)
;; (autoload 'doctest-mode "doctest-mode" "Python Doctest mode." t)
;; (add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))
;; ;; enable autocompletion of paired characters and spell checking in doctest mode
;; (add-hook 'doctest-mode-hook (lambda () (textmate-paren-mode) (flyspell-prog-mode) (linum-mode)))


;; Use archive mode to open Python eggs
(add-to-list 'auto-mode-alist '("\\.egg\\'" . archive-mode))

