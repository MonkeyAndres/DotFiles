;; Basic spell checking with Hunspell
(with-eval-after-load "ispell"
    (setq ispell-program-name "hunspell")
    (setq ispell-dictionary "en_US,en_GB,es_ES")
    ;; ispell-set-spellchecker-params has to be called
    ;; before ispell-hunspell-add-multi-dic will work
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "en_US,en_GB,es_ES"))

(when (boundp 'ispell-hunspell-dictionary-alist)
  (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; Language tool (requires server running)
;; Enable by running =languagetool-server-mode=
(use-package languagetool
  :ensure t
  :defer t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :config
  (setq languagetool-server-url '("http://localhost")
        languagetool-server-port "8081"))
