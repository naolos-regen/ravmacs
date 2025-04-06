;; -*- lexical binding: t; -*

;; most-important-relative line numbers :) and glob

(setq-default indent-tabs-mode t)
(setq-default tab-width        7)


(setq display-line-numbers-type 'relative)

(global-display-line-numbers-mode 1)

(set-face-attribute 'line-number-current-line nil
		    :weight 'bold
		    :foreground "orange")

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
   (osx              (set-face-attribute 'default nil :font "Fira Mono"  :height 70))                    ; macOS
   (linux            (set-face-attribute 'default nil :font "Iosevka"  :height 100))))                   ; Linux

;; Theme
(load-theme 'wombat t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Set load-path to include core and modules
(add-to-list 'load-path "~/.config/ravmacs/core")
(add-to-list 'load-path "~/.config/ravmacs/modules")
(add-to-list 'load-path "~/.config/ravmacs/formatters")

;; load core configs
(require 'packages)
(require 'evil-config)

;; load modules
(require 'completion)

;; load formatters
(require 'c_formatter_42)

;; Extra Evil command
(evil-ex-define-cmd "Ex" 'counsel-find-file)
