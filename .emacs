;; -*- mode: elisp -*-

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Sane defaults
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      load-prefer-newer t
      frame-inhibit-implied-resize t)
(setq ring-bell-function 'ignore)

(setq initial-frame-alist
      '((width . 95) (height . 40)))

;; Increase font-size for 2k displays
(set-face-attribute 'default (selected-frame) :height 140)

;; Mac-specific config
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'control)
  (setq mac-right-option-modifier 'nil)
  )


;; Better buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Start week on Monday
(setq calendar-week-start-day 1)

;; Enable transient mark mode
(transient-mark-mode 1)

;; Encryption
(require 'epa-file)
(epa-file-enable)
(setq epa-file-encrypt-to '("monkeyandres@protonmail.com"))

;; Font
(set-face-attribute 'default nil
		    :font "JetBrains Mono")

;; UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

;; Spell checking
(setq ispell-program-name "hunspell")

(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist
      ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
      ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

(when (boundp 'ispell-hunspell-dictionary-alist)
  (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; MELPA
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Try new packages
(use-package try)

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

;; Hot keys autocompletion
(use-package which-key
  :init (which-key-mode))

;; Autocomplete
(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-resize t)
  (setq vertico-cycle t))

;; Store history of completions
(use-package savehist
  :init
  (savehist-mode))

;; Better completions
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Extra info for Vertico completions
(use-package marginalia
  :init
  (marginalia-mode))

;; Visual line wrap
(setq-default fill-column 80)

(use-package visual-fill-column
  :config
  (setq-default visual-fill-column-center-text t)
  )

(use-package adaptive-wrap
  :init (adaptive-wrap-prefix-mode 1))

;; God Mode (couldn't get used to it)
;; (use-package god-mode
;;   :config
;;   (global-set-key (kbd "<escape>") #'god-mode-all)
;;   (global-set-key (kbd "C-x C-1") #'delete-other-windows)
;;   (global-set-key (kbd "C-x C-2") #'split-window-below)
;;   (global-set-key (kbd "C-x C-3") #'split-window-right)
;;   (global-set-key (kbd "C-x C-0") #'delete-window)
;;   (custom-set-faces
;;    '(god-mode-lighter ((t (:inherit error)))))
;;   )

;; Org mode
(require 'org)

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq org-startup-indented t)

;; Start visual-line-mode for every org file
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(add-hook 'org-mode-hook #'visual-line-mode)

;; Remove bookmarks when refile and capture
(setq org-bookmark-names-plist nil)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(p)" "BLOCKED(b@/!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
	      (sequence "IDEA(i)" "DRAFT(d)" "WRITING(w)" "|" "PUBLISHED(p)"))))

(setq org-log-into-drawer t)

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "#DF8C8C" :weight bold)
              ("NEXT" :foreground "#83AFE5" :weight bold)
	      ("IN-PROGRESS" :foreground "#DADA93" :weight bold)
              ("BLOCKED" :foreground "#D18EC2" :weight bold)
              ("DONE" :foreground "#A8CE93" :weight bold)
              ("CANCELLED" :foreground "#9A93E1" :weight bold)
	      
	      ("IDEA" :foreground "#DF8C8C" :weight bold)
	      ("DRAFT" :foreground "#83AFE5" :weight bold)
	      ("WRITING" :foreground "#DADA93" :weight bold)
	      ("PUBLISHED" :foreground "#A8CE93" :weight bold))))

;; Shift + Arrow don't count as state change (used to correct task statuses)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-default-notes-file "~/Documents/org/inbox.org")

(setq org-agenda-files
      '("~/Documents/org/inbox.org"
	"~/Documents/org/projects.org.gpg"
	"~/Documents/org/work.org.gpg"
	"~/Documents/org/blog.org.gpg"))

(setq org-capture-templates '(("t" "Todo" entry (file+headline "~/Documents/org/inbox.org" "Tasks") "* TODO %i%?")
                              ("n" "Note" entry (file+headline "~/Documents/org/inbox.org" "Notes") "* %i%?")
			      ("j" "Journal Entry" plain (file+olp+datetree "~/Documents/org/journal.org.gpg") "%i%?" :empty-lines 1)))

(setq org-refile-targets '((nil :maxlevel . 9)
			   ("~/Documents/org/projects.org.gpg" :maxlevel . 9)
                           ("~/Documents/org/someday.org.gpg" :maxlevel . 9)
			   ("~/Documents/org/work.org.gpg" :maxlevel . 9)
   			   ("~/Documents/org/notes.org.gpg" :maxlevel . 9)
			   ("~/Documents/org/blog.org.gpg" :maxlevel . 9)))

(setq org-agenda-custom-commands
   '(("g" "Global view for today"
      ((agenda ""
	       ((org-agenda-span 'day)
		(org-agenda-overriding-header "Agenda for the day")))
       (todo "IN-PROGRESS|WRITING"
	     ((org-agenda-overriding-header "Tasks already in progress")))
       (tags-todo "PRIORITY=\"A\""
		  ((org-agenda-overriding-header "Top priority tasks")
		   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
       (todo "BLOCKED"
	     ((org-agenda-overriding-header "BLOCKED tasks")))
       (tags "+weaknesses+PRIORITY=\"A\""
	     ((org-agenda-overriding-header "Pending weaknesses")))
       (todo "NEXT"
	     ((org-agenda-overriding-header "Possible NEXT tasks")))
      nil))))

;; Remove all scheduled tasks from TODO views in Agenda
(setq org-agenda-todo-ignore-scheduled 'all)

;; Use full outline paths for refile targets
(setq org-refile-use-outline-path t)

;; Allow full paths for better completions
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Enable the following when searching through archived stuff 
;;(setq org-sparse-tree-open-archived-trees t)
<<<<<<< HEAD
=======

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("6ca5f925de5c119694dbe47e2bc95f8bad16b46d154b3e2e0ae246fec4100ec5" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "b54bf2fa7c33a63a009f249958312c73ec5b368b1094e18e5953adb95ad2ec3a" default))
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(package-selected-packages
   '(god-mode marginalia orderless vertico emojify nova-theme atom-one-dark-theme dracula-theme dash solarized-theme omtose-phellack-theme ef-themes adaptive-wrap visual-fill-column which-key material-theme try use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
>>>>>>> aa675c8 (chore: update .emacs config with mac-os config)
