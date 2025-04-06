(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key]      . helpful-key))

(use-package general
             :config
             (general-create-definer rune/leader-keys
                                     :keymaps '(normal insert visual emacs)
                                     :prefix "SPC"
                                     :global-prefix "C-SPC")

             (rune/leader-keys
               "t"  '(:ignore t :which-key "toggles")
               "tt" '(counsel-load-theme :which-key "chose theme")))

(use-package hydra)

(defhydra hydra-zoom (global-map "<f3>")
  "zoom"
  ("j" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(provide 'gen-conf)
