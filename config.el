;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Anish Moorthy"
      user-mail-address "anlsh@protonmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-Iosvkem)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Anish's Configuration ;;

;; Look idk what vc even does but it slows down tramp and causes errors in
;; magit forge https://github.com/magit/magit/issues/1274 so it's gone fs
(setq vc-handled-backends nil)
(setq auth-sources '("~/.authinfo.gpg"))
;; Relative line numbers, but I still never use them lol
(setq display-line-numbers-type 'relative)
;; Enable narrow-to-region by default
(put 'narrow-to-region 'disabled nil)
;; Enable the fill-column indicator everywhere
(global-display-fill-column-indicator-mode)
;; Maximize GUI on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Create a hook for lispy modes (emacs lisp, CL, clojure, etc)
;; lispy-mode-hook is an actual thing, so we just use "lispy-hook"
(defvar lispy-hook)
(add-hook 'lisp-mode-hook (lambda () (run-hooks 'lispy-hook)))
(add-hook 'emacs-lisp-mode-hook (lambda () (run-hooks 'lispy-hook)))
(add-hook 'sly-mrepl-mode-hook (lambda () (run-hooks 'lispy-hook)))
(add-hook 'lispy-hook (lambda () (setq fill-column 100)))
(add-hook 'lispy-hook #'hungry-delete-mode)
(add-hook 'lispy-hook #'aggressive-indent-mode)
(add-hook 'lispy-hook #'smartparens-strict-mode)
(add-hook 'lispy-hook #'evil-cleverparens-mode)

(defadvice projectile-project-root (around ignore-remote first activate)
  (unless (file-remote-p default-directory) ad-do-it))

(load "~/.doom.d/keybindings.el")

(use-package company
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 3)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (company-auto-complete-chars nil)

  :bind (:map company-active-map
         ("TAB" . #'company-complete-selection)
         ("<tab>" . #'company-complete-selection)
         ("SPC" . nil)
         ("RET" . nil)
         ("<return>" . nil))
  :config
  (global-company-mode)

  (use-package company-quickhelp
    :defer t
    :custom (company-quickhelp-delay 0))

  ;; I don't actually use coq these days, but wth
  (use-package company-coq
    :defer t
    :custom (coq-compile-before-require t)
    :config (load "~/.emacs.d/private/proof-general/generic/proof-site")))

(use-package counsel-projectile
  :config
  (cl-labels
      ;; Make counsel-projectile-switch-project open a dired buffer instead
      ;; of immediately asking you to open a file in the project
      ;; https://github.com/
      ;;     ericdanan/counsel-projectile/issues/58#issuecomment-387752675
      ((open-dired-in-counsel-projectile (project)
            "Open ‘dired’ at the root of the project."
            (let ((projectile-switch-project-action
                   (lambda ()
                     (projectile-dired))))
              (counsel-projectile-switch-project-by-name project))))

    (counsel-projectile-modify-action
     'counsel-projectile-switch-project-action
     `((add ("." ,#'open-dired-in-counsel-projectile
             "open ‘dired’ at the root of the project")
            1)))))

(use-package evil-cleverparens
  :after smartparens
  :hook (lispy-mode . evil-cleverparens-mode))

(use-package hungry-delete
  :custom (hungry-delete-join-reluctantly t))

;; Prefer clangd as C++ Language Server
(use-package lsp-clangd
  :custom
  (lsp-clients-clangd-args
   '("-j=4"
     "--background-index"
     "--clang-tidy"
     "--completion-style=detailed"
     "--header-insertion=never"
     "--header-insertion-decorators=0"))
  :config
  (set-lsp-priority! 'clangd 2))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-auto-revert-mode t)
  (git-rebase-confirm-cancel nil))

(use-package org
  :custom
  (org-directory "~/.org")
  (org-agenda-window-setup 'only-window))

(use-package projectile-rsync
  :custom
  (rsync-command-base "rsync -CavP"))

(use-package python
  :custom
  (python-indent-offset 4))

(use-package shackle
  :custom (shackle-rules
           '((shell-mode :same t :inhibit-window-quit t)))
  :config
  (shackle-mode))

(use-package sly
  :custom
  (inferior-lisp-program "sbcl")
  (sly-complete-symbol-function 'sly-flex-completions)
  (sly-compile-file-options '(:fasl-directory "/tmp/"))

  :config
  ;; I don't like popups, which Doom puts sly into by default
  ;; See ~/.emacs.d/modules/lang/common-lisp/config.el for the list of things to
  ;; override
  (set-popup-rules!
    (mapcar (lambda (regex) (list regex :ignore t))
            '("^\\*sly-mrepl" "^\\*sly-compilation" "^\\*sly-traces"
              "^\\*sly-description" "^\\*sly-\\(?:db\\|inspector\\)"))))

(use-package smartparens
   :hook (lispy-mode . turn-on-smartparens-strict-mode))

(use-package vterm
  :hook (vterm-mode . (lambda () (setq show-trailing-whitespace nil))))

(use-package ws-butler
  :config (ws-butler-global-mode))


(use-package! window-purpose
  :custom
  (purpose-layout-dirs '("~/.doom.d/layouts/"))
  ;; Although the package suggests using add-to-list, default value is nil
  (purpose-mode-user-purposes '((lisp-mode . cl-src)
                                (sldb-mode . cl-repl)
                                (slime-repl-mode . cl-repl)
                                (slime-inspector-mode . cl-general)))
  (purpose-user-name-purposes '(("*inferior-lisp*" . cl-repl)
                                ("*slime-repl sbcl*" . cl-repl)))
  (purpose-user-regexp-purposes '(( "\*sldb.*" . cl-repl)
                                  ("\*sly-db" . cl-repl)
                                  ("\*sly-mrepl" . cl-repl)))
  :config
  (purpose-compile-user-configuration))

(use-package yasnippet
  :custom (yas-snippet-dirs '("~/.doom.d/snippets/")))

(defun copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun load-local-file (filename)
  "Loads ~/.doom.d.local/filename if the file exists"
  (let ((full-name (concat (file-name-as-directory "~/.doom.d.local/")
                           filename)))
    (when (file-exists-p full-name)
      (load full-name))))

(load-local-file "init.el")
