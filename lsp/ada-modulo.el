(add-to-list 'load-path "~/.config/ravmacs/lsp/ada")
;; Current ada-mode has more useful features, but it don't worky :(
(autoload 'ada-mode "ada-mode")

(cl-loop for ext in '("\\.gpr$" "\\.ada$" "\\.ads$" "\\.adb$")
	  do (add-to-list 'auto-mode-alist (cons ext 'ada-mode)))


(provide 'ada-modulo)
