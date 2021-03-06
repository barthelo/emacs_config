;;; ADD REPOSITORIES
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
             ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; USE-PACKAGE
(require 'use-package)
(require 'diminish)

;;; GENERAL EMACS SETTINGS
;; YES NO TO Y N
(defalias 'yes-or-no-p 'y-or-n-p)

;; FILLING CONFIGURATION
(setq set-fill-column 80)
(add-hook 'org-mode-hook 'auto-fill-mode)

;; ENABLE PARENTHESIS MODE
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; DISPLAY TIME IN MODE LINE
(display-time-mode 1)

;; DISPLAY BATTERY STATUS IN MODE LINE
(display-battery-mode 1)

;; DISPLAY FILE SIZE IN MODE LINE
(size-indication-mode 1)

;; DISPLAY COLUMN NUMBER IN MODE LINE
(column-number-mode 1)

;; AGRESSIVE IDENTATION
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config (add-hook 'prog-mode-hook 'aggressive-indent-mode))

;; HIGHLIGHT PARENTHESIS
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :ensure highlight-parentheses
  :config (add-hook 'prog-mode-hook 'highlight-parentheses-mode))

;; HIGHLIGHT NUMBERS
(use-package highlight-numbers
  :config (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; DISABLE BACKUP AND AUTOSAVE FILES
(setq make-backup-files nil)
(setq auto-save-default nil)

;; TABS WHITESPACES INDENTATION
(setq-default indent-tabs-mode nil)
(setq-default tab-width 3)
(setq-default c-basic-offset 3)
(setq-default c-default-style "bsd")
(setq tab-always-indent 'complete)

;; WINDOW SWITCHING
(global-set-key [C-tab] 'other-window)

;; WHITESPACE MODE
(global-whitespace-mode t)
(setq whitespace-line-column 80)
(setq whitespace-style (quote (face trailing tabs spaces newline empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark)))
(diminish 'global-whitespace-mode)

;; DISPLAY LINE NUMBERS
(global-linum-mode t)

;; HIGHLIGHT CURRETN LINE
(global-hl-line-mode t)

;; HIDE TOOLBAR AND MENUBAR
(tool-bar-mode -1)
(menu-bar-mode -1)

;; NO STARTUP SCRREN
(setq inhibit-startup-message t)

;; AUTO REVERT MODE
(global-auto-revert-mode 1)

;; AUTO REFRESH DIRED
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; HELM
(use-package helm
  :diminish helm-mode
  :config (progn (helm-mode 1)
                 (setq helm-mode-fuzzy-match t)
                 (setq helm-split-window-in-side-p t))
  :bind (("M-x" . helm-M-x) ; Helm's M-x
         ("M-y" . helm-show-kill-ring) ; Helm's killring
         ("C-x C-f" . helm-find-files) ; Helm's find file
         ("C-x b" . helm-mini) ; Helm's branch
         ("C-x C-b" . helm-mini) ; Helm's branch
         ("C-s" . helm-occur)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-y" . helm-select-action)
         :map helm-find-files-map
         ("C-<backspace>" . helm-find-files-up-one-level)))

;; FLYSPELL
(use-package flyspell
  :config
  (progn (add-hook 'text-mode-hook 'flyspell-mode)
         (defun my-switch-dictionary()
           (interactive)
           (let* ((dic ispell-current-dictionary)
                  (change (if (string= dic "deutsch8") "english" "deutsch8")))
             (ispell-change-dictionary change)
             (message "Dictionary switched from %s to %s" dic change)))))

;; EXPAND REGION
(use-package expand-region
  :diminish expand-region
  :bind ("C-=" . er/expand-region))

;; SETTING FOR AVY
(use-package avy
  :diminish avy
  :bind (("M-s" . avy-goto-char-2)
         ("M-g M-g" . avy-goto-line)))

;; THEME SETTINGS
(use-package monokai
  :init (progn
          (load-theme 'monokai t)
          (set-face-attribute 'region nil :background monokai-green :foreground monokai-gray)))

;; DIRED
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh")

;; SMARTPARENS
(use-package smartparens-config
  :diminish smartparens-mode
  :config (smartparens-global-mode))

(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-M-d") 'kill-sexp)

;; FIX WORD
(use-package fix-word
  :bind (("M-u" . fix-word-upcase)
         ("M-l" . fix-word-downcase)
         ("M-c" . fix-word-capitalize)))

;;; PROGRAM SPECIFIC SETTINGS
;; PROJECTILE
(use-package projectile
  :diminish projectile-mode
  :ensure projectile
  :ensure helm
  :config
  (progn (projectile-global-mode)
         (helm-projectile-on)
         (setq projectile-project-compilation-cmd "make -C ./code/build all")
         (setq projectile-project-run-cmd "make -C ./code/build flash")
         (setq compilation-scroll-output t)))
;; (progn (projectile-global-mode)
;;        (setq projectile-completion-system 'helm)
;;        (helm-projectile-on)
;;        (setq projectile-switch-project-action 'helm-projectile))

;; MAGIT
(use-package magit
  :bind ("C-x g" . magit-status))

;; YASNIPPET
(use-package yasnippet
  :diminish yas-minor-mode
  :config (yas-global-mode 1))

;; COMPANY
(use-package company
  :diminish company-mode
  :config
  (progn (add-hook 'after-init-hook 'global-company-mode)
         (setq company-idle-delay 0.1)
         (setq company-show-numbers t)
         ;; (setq company-dabbrev-code-ignore-case t)
         (setq company-minimum-prefix-length 2)))

;; NUMBER INCREMENT
(use-package evil-numbers
  :bind (("C-c C-=" . evil-numbers/inc-at-pt)
         ("C-c C--" . evil-numbers/dec-at-pt)))

;; MULTIPLE CURSORS
(use-package multiple-cursors
  :bind (("C-c m c" . mc/edit-lines)
         ("C-c m n" . mc/mark-next-like-this)
         ("C-c m p" . mc/mark-previous-like-this)))

;;; C-MODE SPECIFIC SETTINGS
;; FLYCHECK
(use-package flycheck
  :config
  (progn (add-hook 'c-mode-common-hook 'flycheck-mode)
         (defun my-flycheck-setup ()
           (interactive)
           (flycheck-select-checker 'clang)
           (setq flycheck-clang-include-path
                 '("/usr/include" "/usr/local/include /usr/lib/clang/3.8.0/include"))
           (setq-local flycheck-highlighting-mode nil)
           (setq-local flycheck-check-syntax-automatically nil))
         (add-hook 'c-mode-common-hook '(my-flycheck-setup))))

;; IRONY
(use-package irony
  :config
  (progn (add-hook 'c-mode-hook 'irony-mode)
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
           '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))

;; XCSCOPE
(use-package xcscope
  :config (add-hook 'c-mode-hook 'cscope-minor-mode))

;; HELM CSCOPE
(use-package helm-cscope
  :config
  (progn (add-hook 'c-mode-hook 'helm-cscope-mode)
         (add-hook 'c++-mode-hook 'helm-cscope-mode))
  :bind (("C-c s g" . helm-cscope-find-global-definition)
         ("C-c s t" . helm-cscope-find-this-text-string)
         ("C-c s e" . helm-cscope-find-egrep-pattern)
         ("C-c s s" . helm-cscope-find-this-symbol)
         ("C-c s =" . helm-cscope-find-assignments-to-this-symbol)
         ("C-c s c" . helm-cscope-find-calling-this-funtcion)
         ("C-c s f" . helm-cscope-find-this-file)
         ("C-c s i" . helm-cscope-find-files-including-file)
         ("C-c s d" . helm-cscope-find-called-function)))

;;; MATLAB Mode
;; (use-package matlab-mode
;;   :init (setq matlab-shell-command
;;               "/home/eugen/Programme/MATLAB/R2013b/bin/matlab")
;;   :config (matlab-cedet-setup))

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

(defun my-beginning-of-line()
  (interactive)
  (setq my-current-point-position (point))
  (back-to-indentation)
  (when (or (> (point) my-current-point-position)
            (= (point) my-current-point-position))
    (move-beginning-of-line nil)))
(global-set-key (kbd "C-a") 'my-beginning-of-line)
