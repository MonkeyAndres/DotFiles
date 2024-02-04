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

;; (setq org-agenda-custom-commands
;;    '(("g" "Global view for today"
;;       ((agenda ""
;; 	       ((org-agenda-span 'day)
;; 		(org-agenda-overriding-header "Agenda for the day")))
;;        (todo "IN-PROGRESS|WRITING"
;; 	     ((org-agenda-overriding-header "Tasks already in progress")))
;;        (tags-todo "PRIORITY=\"A\""
;; 		  ((org-agenda-overriding-header "Top priority tasks")
;; 		   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
;;        (todo "BLOCKED"
;; 	     ((org-agenda-overriding-header "BLOCKED tasks")))
;;        (tags "+weaknesses+PRIORITY=\"A\""
;; 	     ((org-agenda-overriding-header "Pending weaknesses")))
;;        (todo "NEXT"
;; 	     ((org-agenda-overriding-header "Possible NEXT tasks")))
;;       nil))))

(setq org-agenda-custom-commands
   '(("g" "Global view for today"
      ((agenda ""
	       ((org-agenda-span 'day)
		(org-agenda-overriding-header "Agenda for the day")))
       (todo "IN-PROGRESS|WRITING"
	     ((org-agenda-overriding-header "Tasks already in progress")))
       (todo "BLOCKED"
	     ((org-agenda-overriding-header "BLOCKED tasks")))
       (tags "+weaknesses+PRIORITY=\"A\""
	     ((org-agenda-overriding-header "Pending weaknesses")))
       (todo "NEXT|TODO"
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
;;(setq org-sparse-tree-open-archived-trees

;; Increase CATEGORY width in Agenda view
(setq org-agenda-prefix-format
   '((agenda . " %i %-14:c%?-12t% s")
     (todo . " %i %-14:c")
     (tags . " %i %-14:c")
     (search . " %i %-14:c")))
