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
(use-package helm
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
  :bind ("C-=" . er/expand-region))

;; SETTING FOR AVY
(use-package avy
  :bind (("M-s" . avy-goto-char-2)
<<<<<<< HEAD
         ("M-g M-g" . avy-goto-line)))

;; THEME SETTINGS
(use-package monokai-theme
  :config (progn
            (set-face-attribute 'region nil :background monokai-green :foreground monokai-gray)
            (load-theme 'monokai t)))
=======
         ("M-g M-g" . avy-goto-liney)))

;; THEME SETTINGS
(use-package monokai-theme
  :config (load-theme 'monokai t))
>>>>>>> c722307f2f0e50bd6d471370f70d61700954f5c6

;; DIRED
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh")

;; SMARTPARENS
(use-package smartparens-config
  :diminish smartparens-mode
  :config (smartparens-global-mode))

;; SEXP HANDLING
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-M-d") 'kill-sexp)

;; FIX WORD
(use-package fix-word
  :bind (("M-u" . fix-word-upcase)
         ("M-l" . fix-word-downcase)
         ("M-c" . fix-word-capitalize)))

<<<<<<< HEAD
=======
;; GOLDEN RATION WINDOW RESIZE
(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config (golden-ratio-mode 1))
>>>>>>> c722307f2f0e50bd6d471370f70d61700954f5c6

;;; PROGRAM SPECIFIC SETTINGS
;; PROJECTILE
(use-package projectile
  :diminish projectile-mode
  :ensure projectile
  :ensure helm
  :config
  (progn (projectile-global-mode)
<<<<<<< HEAD
         (helm-projectile-on)
         (setq projectile-project-compilation-cmd "make -C ./code/build all")
         (setq projectile-project-run-cmd "make -C ./code/build flash")
         (setq compilation-scroll-output t)))

;; (progn (projectile-global-mode)
;;        (setq projectile-completion-system 'helm)
;;        (helm-projectile-on)
;;        (setq projectile-switch-project-action 'helm-projectile))
=======
         (setq projectile-completion-system 'helm)
         (helm-projectile-on)
         (setq projectile-switch-project-action 'helm-projectile)))
(require 'projectile)
>>>>>>> c722307f2f0e50bd6d471370f70d61700954f5c6

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
<<<<<<< HEAD

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
(use-package matlab-mode
  :init (setq matlab-shell-command
              "/home/eugen/Programme/MATLAB/R2013b/bin/matlab")
  :config (matlab-cedet-setup))

=======

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
>>>>>>> c722307f2f0e50bd6d471370f70d61700954f5c6

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
<<<<<<< HEAD
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" default)))
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (yasnippet yafolding use-package undo-tree smartparens smart-mode-line-powerline-theme realgud pos-tip multiple-cursors morlock monokai-theme magit list-utils linum-relative levenshtein irony-eldoc highlight-parentheses highlight-operators highlight-numbers highlight helm-projectile helm-cscope goto-chg fuzzy flycheck-irony fix-word expand-region evil-numbers ecb disaster diredful dired-open darkokai-theme company-irony cmake-font-lock avy anzu aggressive-indent)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
=======
>>>>>>> c722307f2f0e50bd6d471370f70d61700954f5c6
