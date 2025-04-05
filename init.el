;; -*- lexical binding: t; -*-

(setq 
  	inhibit-startup-message t

	auto-save-default nil

	make-backup-files nil

	set-mark-command-repeat-pop t

	large-file-warning-threshold nil

	vc-follow-symlinks t

	ad-redefinition-action 'accept

	global-auto-revert-non-file-buffers t

	native-comp-async-report-warnings-errors nil)

(repeat-mode 		 	1)	;; Enable repeating keymaps
(menu-bar-mode 		 	0)	;; Hide the menu bar
(tool-bar-mode 		 	0)	;; Hide the tool bar
(savehist-mode 		 	1)	;; Save minibuffer history
(scroll-bar-mode	 	0)	;; Hide the scroll bar
(xterm-mouse-mode 	 	1)	;; Enable mouse events in terminal Emacs
(display-time-mode 	 	1)	;; display time in mode line / tab bar
(fido-vertical-mode 	 	1)	;; Improved vertical minibuffer completions
(column-number-mode 	 	1)	;; Show column number on mode line
(tab-bar-history-mode 	 	1)	;; Remember previous tab window configurations
(auto-save-visited-mode  	1)	;; Auto-save files at an interval
(global-visual-line-mode 	1)	;; Visually wrap long lines in all buffers
(global-auto-revert-mode 	1)	;; Refresh buffers with changed local files

(setq-default indent-tabs-mode 	nil 
	      tab-width 	6)

;; DEL trailing whitespace before saving buffers
(add-hook 'before-save-hook 'delete-trailing-whitespace)
