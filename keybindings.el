;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-localleader-key ",")

(map! :leader
      :desc "M-x" "SPC" #'counsel-M-x
      :desc "Find file in project" ":" #'+ivy/projectile-find-file)

(map! :map evil-window-map
      ;; Unbind "p" so that it can be used as a prefix
      "p" nil
      :prefix ("p" . "purpose")
      :desc "purpose-load-layout" "l" #'purpose-load-layout
      :desc "purpose-save-layout" "s" #'purpose-save-layout)

(defvar doom-help-prefix-map (make-sparse-keymap))
(define-key! doom-help-prefix-map
  "c" #'doom/help-package-config
  "d" #'doom/goto-private-packages-file
  "h" #'doom/help-package-homepage
  "p" #'doom/help-packages)

(defvar doom-help-map (make-sparse-keymap))
(define-key! doom-help-map
  "b" #'doom/report-bug
  "c" #'doom/goto-private-config-file
  "d" #'doom/toggle-debug-mode
  "f" #'doom/help-faq
  "h" #'doom/help
  "l" #'doom/help-search-load-path
  "m" #'doom/help-modules
  "n" #'doom/help-news
  "s" #'doom/help-search-headings
  "t" #'doom/toggle-profiler
  "v" #'doom/help-autodefs
  "v" #'doom/version
  "x" #'doom/sandbox
  "C" #'doom/goto-private-init-file
  "L" #'doom/help-search-loaded-files
  "N" #'doom/help-news-search
  "S" #'doom/help-search
  "p" doom-help-prefix-map)

;; (define-key! help-map
;;   "D" doom-help-map)

(map! (:map doom-help-map
       :desc "prefix" "p" doom-help-prefix-map)
      (:map help-map
       :desc "Doom" "D" doom-help-map))

(defvar describe-map (make-sparse-keymap))
(define-key! describe-map
  "k" #'describe-key
  "g" #'describe-gnu-project
  "v" #'describe-variable
  "s" #'describe-symbol
  "c" #'describe-key-briefly
  "C" #'describe-coding-system
  "f" #'describe-function
  "F" #'describe-face
  "I" #'describe-input-method
  "L" #'describe-language-environment
  "m" #'describe-mode
  "M" #'doom/describe-active-minor-mode)

(map! (:map help-map
       "k" nil "v" nil "s" nil "c" nil "f" nil "I" nil "L" nil "m" nil "M" nil
       "g" nil "F" nil "C" nil
       "d" nil
       :desc "describe" "d" describe-map))

(map! (:map lisp-mode-map
       :localleader
       :prefix "g"
       :desc "goto-definition" "g" #'+lookup/definition))
