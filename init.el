;; -*- lexical binding: t; -*-

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

;; modes
(repeat-mode 		 	1)	; Enable repeating keymaps
(menu-bar-mode 		 	0)	; Hide the menu bar
(tool-bar-mode 		 	0)	; Hide the tool bar
(savehist-mode 		 	1)	; Save minibuffer history
(scroll-bar-mode	 	0)	; Hide the scroll bar
(xterm-mouse-mode 	 	1)	; Enable mouse events in terminal Emacs
(display-time-mode 	 	1)	; display time in mode line / tab bar
(fido-vertical-mode 	 	1)	; Improved vertical minibuffer completions
(column-number-mode 	 	1)	; Show column number on mode line
(tab-bar-history-mode 	 	1)	; Remember previous tab window configurations
(auto-save-visited-mode  	1)	; Auto-save files at an interval
(global-visual-line-mode 	1)	; Visually wrap long lines in all buffers
(global-auto-revert-mode 	1)	; Refresh buffers with changed local files
(setq-default indent-tabs-mode 	nil 
	      tab-width 	6)

;; DEL trailing whitespace before saving buffers
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set the Font
(on-platform-do 
  ((windows cygwin (set-face-attribute 'default nil :font "Fira Mono:antialias=subpixel" :height 130)) ; WinOS
  (osx 		   (set-face-attribute 'default nil :font "Fira Mono" 		         :height 170)) ; MacOS
  (linux 	   (set-face-attribute 'default nil :font "Iosevka" 			 :height 100)) ; Linux
)

;; Theme

(load-theme 'wombat)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Init package sources
(require 'package)

(setq package-archives '(
			 
			 ("melpa" . "https://melpa.com/packages/")
			 ("org"   . "https://orgmode.com/elpa/"  )
			 ("elpa"  . "https://elpa.gnu.org/packages")

			)
)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents)
)

;; Init-non-Linux
(unless (package-installed-p 'use-package)
  	(package-install     'use-package)
)
(require 'use-package)
(setq use-package-always ensute t)


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
		    ("C-d" . ivy-reverse-i-search-kill)
		   )
	     :config
	     (ivy-mode 1)
)

;; Set Vim Keybindings
(use-package evil
	
	     :init
	     (setq evil-want-integration 	 t)
	     (setq evil-want-keybinding  	 nil)
	     (setq evil-want-C-u-scroll  	 t)
	     (setq evil-want-C-i-jump    	 nil)
	     (setq evil-respect-visual-line-mode t)
	     :config
	     (evil-mode 1)
	     (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
	     (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

	     ;; use visual line motions even outside of visual-line mode buffers
	     (evil-global-set-key 'motion "j" 'evil-next-visual-line)
	     (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

)

;; More vim Keys
(use-package evil-collection 
	     :after evil
	     :custom
	     (evil-collection-outline-bind-tab-p nil)
	     :config
	     (evil-collection-init)
)


