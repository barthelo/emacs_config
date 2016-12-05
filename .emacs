;;; ADD REPOSITORIES
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
             ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;;; GENERAL EMACS SETTINGS
;; YES NO TO Y N
(defalias 'yes-or-no-p 'y-or-n-p)

;; FILLING CONFIGURATION
(setq set-fill-column 80)
(add-hook 'org-mode-hook 'auto-fill-mode)

;; ENABLE PARENTHESIS MODE
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; AGRESSIVE IDENTATION
(require 'aggressive-indent)
(add-hook 'prog-mode-hook 'aggressive-indent-mode)

;; HIGHLIGHT PARENTHESIS
(require 'highlight-parentheses)
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)

;; HIGHLIGHT NUMBERS
(require 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; DISABLE BACKUP AND AUTOSAVE FILES
(setq make-backup-files nil)
(setq auto-save-default nil)

;; TABS WHITESPACES INDENTATION
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default c-default-style "bsd")
(setq tab-always-indent 'complete)

;; WINDOW SWITCHING
(global-set-key [C-tab] 'other-window)

;; WHITESPACE MODE
(global-whitespace-mode t)

;; DISPLAY LINE NUMBERS
;(global-linum-mode t)

;; HIGHLIGHT CURRETN LINE
(global-hl-line-mode t)

;; HIDE TOOLBAR AND MENUBAR
(tool-bar-mode -1)
(menu-bar-mode -1)

;; NO STARTUP SCRREN
(setq inhibit-startup-message t)

;; HELM
(require 'helm)
(helm-mode 1)
(setq helm-mode-fuzzy-match t
      helm-split-window-in-side-p t)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-y")  'helm-select-action) ; list actions using C-z
(global-set-key (kbd "M-x") 'helm-M-x) ; Helm's M-x
(global-set-key (kbd "M-y") 'helm-show-kill-ring) ; Helm's killring
(global-set-key (kbd "C-x C-f") 'helm-find-files) ; Helm's find file
(global-set-key (kbd "C-x b") 'helm-mini) ; Helm's branch
(global-set-key (kbd "C-s") 'helm-occur) ; Helm's occure

;; FLYSPELL
(require 'flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)

;; EXPAND REGION
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; ORG-MODE
(setq org-todo-keywords
      '((Sequence "TODO" "WAITING" "DONE")))
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("WAITING" :foreground "chocolate" :weight bold)
              ("DONE" :foreground "green" :weight bold))))
;; add timestamp after finisch todo
(setq org-log-done 'time)
;; add not after finishing todo
(setq org-log-done 'note)

(setq org-default-notes-file "~/Downloads/org_file.org")
(define-key global-map "\C-cc" 'org-capture)

;; SETTING FOR AVY
(require 'avy)
(global-set-key (kbd "M-s") 'avy-goto-char-2)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)

;; THEME SETTINGS
(require 'monokai-theme)

;; DIRED
(setq dired-dwim-target t)

;; SMARTPARENS
(require 'smartparens-config)
(smartparens-global-mode)

;; SEXP HANDLING
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-M-d") 'kill-sexp)

;;; PROGRAM SPECIFIC SETTINGS
;; YASNIPPET
(require 'yasnippet)
(yas-global-mode 1)

;; NUMBER INCREMENT
(require 'evil-numbers)
(global-set-key (kbd "C-c C-=") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c C--") 'evil-numbers/dec-at-pt)

;; MULTIPLE CURSORS
(require 'multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)

;; MATLAB
(require 'matlab)
(setq matlab-indent-function t)
(setq matlab-shell-command "/home/eugen/Programme/MATLAB/R2013b/bin/matlab")

;;; C-MODE SPECIFIC SETTINGS

;; COMPANY
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;; (setq company-backends (delete 'company-semantic company-backends))
(setq company-idle-delay 0.1)
(setq company-show-numbers t)
(setq company-minimum-prefix-length 2)
;; (setq company-dabbrev-code-ignore-case t)
;; (setq company-clang-arguments
;;       (mapcar
;;        (lambda (item)
;;          (concat "-I" item))
;;        (split-string "/usr/include
;; /usr/local/include
;; /usr/lib/clang/3.8.0/include
;; /opt/ti/mspgcc/msp430-elf/include")))

;; SEMANTIC
;; (semantic-mode 1)
;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)
;; (global-semantic-idle-summary-mode 1)
;; (global-semantic-highlight-func-mode 1)
;; (global-semantic-stickyfunc-mode 1)
;; (global-semantic-decoration-mode 1)
;; (setq-mode-local c-mode
;;                  semanticdb-find-default-throttle
;;                  '(file local unloaded recursive))

;; FLYCHECK
(require 'flycheck)
(add-hook 'c-mode-common-hook 'flycheck-mode)

;; IRONY
(require 'irony)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(setq company-irony-ignore-case t)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; tab settings
;; (defun irony--check-expansion ()
;;   (save-excursion
;;     (if (looking-at "\\_>") t
;;       (backward-char 1)
;;       (if (looking-at "\\.") t
;;         (backward-char 1)
;;         (if (looking-at "->") t nil)))))
;; (defun irony--indent-or-complete ()
;;   "Indent or Complete"
;;   (interactive)
;;   (cond ((and (not (use-region-p))
;;               (irony--check-expansion))
;;          (message "complete")
;;          (company-complete-common))
;;         (t
;;          (message "indent")
;;          (call-interactively 'c-indent-line-or-region))))
;; (defun irony-mode-keys ()
;;   "Modify keymaps used by `irony-mode'."
;;   (local-set-key (kbd "TAB") 'irony--indent-or-complete)
;;   (local-set-key [tab] 'irony--indent-or-complete))
;; (add-hook 'c-mode-common-hook 'irony-mode-keys)

;; flycheck setup
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; XCSCOPE
(add-hook 'c-mode-hook 'cscope-minor-mode)

(defun my-flycheck-setup ()
  (interactive)
  (flycheck-select-checker 'clang)
  (setq flycheck-clang-include-path
        '("/usr/include" "/usr/local/include /usr/lib/clang/3.8.0/include"))
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))
;; (add-hook 'c-mode-common-hook '(my-flycheck-setup))

;; SMART MODE LINE
(require 'smart-mode-line)
;; (smart-mode-line-enable t)

;; PROJECTILE
(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)

;; MAGIT
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;;; MY FUNCITONS
(defun my-current-copy-line ()
"Copy current line to kill-ring keeping point's position"
  (interactive)
  (clipboard-kill-ring-save
   (line-beginning-position)
   (line-end-position))
  (message "Line copied to kill ring"))
(global-set-key (kbd "C-c k") 'my-current-copy-line)

(defun my-comment-dwin (&optional arg)
"If no region is marked, comment line. Otherwise, comment region."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'my-comment-dwin)

(defun my-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "deutsch8") "english" "deutsch8")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)))

(defun my-beginning-of-line()
  (interactive)
  (setq my-current-point-position (point))
  (back-to-indentation)
  (when (or (> (point) my-current-point-position)
            (= (point) my-current-point-position))
    (move-beginning-of-line nil))
  )
(global-set-key (kbd "C-a") 'my-beginning-of-line)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (company-irony flycheck-irony irony-eldoc irony elpy company yasnippet yafolding undo-tree speed-type smartparens smart-mode-line-powerline-theme realgud pos-tip multiple-cursors morlock monokai-theme matlab-mode magit list-utils linum-relative levenshtein highlight-parentheses highlight-numbers highlight helm-projectile helm-cscope goto-chg fuzzy flycheck expand-region evil-numbers ecb diredful dired-open color-theme cmake-font-lock avy auto-complete-clang auto-complete-c-headers aggressive-indent ac-octave))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )