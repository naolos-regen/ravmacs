;; LSP-mode

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")

  ;; disable ccls formatting here
  (setq lsp-clients-ccls-initialization-options
        '(:index (:comments 2)
          :completion (:detailedLabel t)
          :clang (:extraArgs ["-Wall" "-Wextra"])))

  :hook (
         (haskell-mode . lsp)
         (c-mode       . lsp)
         (c++-mode     . lsp)
         (ada-mode     . lsp)
         )
  :config
  (lsp-enable-which-key-integration t))

(setq lsp-enable-on-type-formatting nil)
(setq lsp-enable-indentation nil)
(setq lsp-enable-formatting nil)


;; completions with company-mode

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
		("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	 ("<tab>"   . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (companu-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Enchancements
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(setq lsp-ui-doc-position        'bottom)
(setq lsp-ui-sideline-enable     nil)
(setq lsp-ui-sideline-show-hover nil)

(require 'oh-haskell)
(require 'oh-c)
(require 'oh-zig)
(require 'ada-modulo)
(provide 'conf)
