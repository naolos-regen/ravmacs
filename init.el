;; -*- lexical binding: t; -*

;; most-important-relative line numbers :) and glob

(setq-default indent-tabs-mode t)
(setq-default tab-width        8)

(setq display-line-numbers-type 'relative)

(global-display-line-numbers-mode t)

(set-face-attribute 'line-number-current-line nil
		    :weight 'bold
		    :foreground "orange")

(dolist (mode '(org-mode-hook
		  term-mode-hook
		  eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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
(load-theme 'leuven-dark t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Set load-path to include core and modules
(add-to-list 'load-path "~/.config/ravmacs/core")
(add-to-list 'load-path "~/.config/ravmacs/modules")
(add-to-list 'load-path "~/.config/ravmacs/formatters")
(add-to-list 'load-path "~/.config/ravmacs/lsp")

;; load core configs
(require 'packages)
(require 'evil-config)
(require 'wk)
(require 'gen-conf)

;; load modules
(require 'completion)

;; load lsp
(require 'conf)

;; load formatters
(require 'c-formatter-42)
(setq c-formatter-42-exec "$HOME/.local/bin/c_formatter_42")
(setq c-formatter-42-set-equalprg 1)
(setq c-formatter-42-format-on-save 1)
(setq norminette-command  "$HOME/.local/bin/norminette")

;; Extra Evil command
(evil-ex-define-cmd "Ex" 'counsel-find-file)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ccls company-box counsel evil-collection general haskell-mode
	   helpful hydra ivy-rich lsp-ui neotree rainbow-delimiters
	   zig-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
