;; PERSONAL THEAMING

;; Increase font-size for 2k displays
(set-face-attribute 'default (selected-frame) :height 140)

;; Better buffer names
(require 'uniquify
	 :config
	 (setq uniquify-buffer-name-style 'forward))

;; Font
(set-face-attribute 'default nil
		    :font "JetBrains Mono")

;; UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

;; Visual line wrap
(setq-default fill-column 80)

(use-package visual-fill-column
  :config
  (setq-default visual-fill-column-center-text t)
  )

(use-package adaptive-wrap
  :init (adaptive-wrap-prefix-mode 1))

;; Themes
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))

;; Disable all other themes to avoid awkward blending:
(mapc #'disable-theme custom-enabled-themes)

(use-package nova-theme
  :config (load-theme 'nova t))

;; Emojis
(use-package emojify
  :hook (after-init . global-emojify-mode)
  :config
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode)))
