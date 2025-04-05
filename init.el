;; -*- lexical binding: t; -*
;; -*- lexical-binding: t; -*-

(setq
 inhibit-startup-message t

 auto-save-default nil

 make-backup-files nil

 set-mark-command-repeat-pop t

 large-file-warning-threshold nil

 vc-follow-symlinks t

 visible-bell nil

 ad-redefinition-action 'accept

 global-auto-revert-non-file-buffers t

 native-comp-async-report-warnings-errors nil)

;; Modes
(repeat-mode                  1)  ; Enable repeating keymaps
(menu-bar-mode                0)  ; Hide the menu bar
(tool-bar-mode                0)  ; Hide the tool bar
(savehist-mode                1)  ; Save minibuffer history
(scroll-bar-mode              0)  ; Hide the scroll bar
(xterm-mouse-mode             1)  ; Enable mouse events in terminal Emacs
(display-time-mode            1)  ; Display time in mode line / tab bar
(fido-vertical-mode           1)  ; Improved vertical minibuffer completions
(column-number-mode           1)  ; Show column number on mode line
(tab-bar-history-mode         1)  ; Remember previous tab window configurations
(auto-save-visited-mode       1)  ; Auto-save files at an interval
(global-visual-line-mode      1)  ; Visually wrap long lines in all buffers
(global-auto-revert-mode      1)  ; Refresh buffers with changed local files


;; Delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Font configuration (you had a misplaced parenthesis and missing macro)
(when (fboundp 'on-platform-do)
  (on-platform-do
   ((windows cygwin) (set-face-attribute 'default nil :font "Fira Mono:antialias=subpixel" :height 130)) ; Win
   (osx              (set-face-attribute 'default nil :font "Fira Mono" :height 170))                    ; macOS
   (linux            (set-face-attribute 'default nil :font "Iosevka"  :height 100))))                   ; Linux

;; Theme
(load-theme 'wombat t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Init package sources
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")
        ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)  ;; typo lol

;; Ivy-mode
(use-package ivy
  :diminish
  :bind (
         ("C-s" . swiper)
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

;; Evil-mode
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-respect-visual-line-mode t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

;; Evil Collection
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))

(use-package neotree
             :config
             (evil-ex-define-cmd "Ex" 'neotree-toggle))
