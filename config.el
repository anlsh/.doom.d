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
(setq doom-theme 'doom-one)

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

(load "~/.doom.d/keybindings.el")
(setq display-line-numbers-type 'relative)

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
  (global-company-mode))

(use-package company-quickhelp
  :defer t
  :custom (company-quickhelp-delay 0))

;; I don't actually use coq these days, but wth
(use-package company-coq
  :defer t
  :custom (coq-compile-before-require t)
  :config (load "~/.emacs.d/private/proof-general/generic/proof-site"))

;;(use-package hungry-delete
;;  :after smartparens
;;  :config
;;  (global-hungry-delete-mode)
;;  (add-hook 'smartparens-enabled-hook (lambda () (hungry-delete-mode 1)))
;;  :diminish hungry-delete-mode)

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-auto-revert-mode t)
  (git-rebase-confirm-cancel nil))

(use-package org
  :init
  (setq  org-directory '("~/org/"))
  :custom
  (org-agenda-window-setup 'only-window))

(use-package python
  :custom
  (python-indent-offset 4))

(use-package shackle
  :custom (shackle-rules
           '((shell-mode :same t :inhibit-window-quit t)))
  :config
  (shackle-mode))

(use-package sly
  :custom (inferior-lisp-program "/usr/bin/sbcl"))


(use-package whitespace
  :hook (before-save . delete-trailing-whitespace)
  :config (global-whitespace-mode))


(use-package! window-purpose
  :custom
  (purpose-layout-dirs '("~/.spacemacs.d/layouts/"))
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
  :custom (yas-snippet-dirs '("~/.spacemacs.d/snippets/")))

(use-package erc
  ;; Sourced from
  ;; https://www.reddit.com/r/emacs/comments/8ml6na/tip_how_to_make_erc_fun_to_use/
  ;; Which is actually the excellent git repo
  ;; https://github.com/rememberYou/.emacs.d/blob/master/config.org#irc
  :preface
  (defun my/erc-notify (nickname message)
    "Displays a notification message for ERC."
    (let* ((channel (buffer-name))
           (nick (erc-hl-nicks-trim-irc-nick nickname))
           (title (if (string-match-p (concat "^" nickname) channel)
                      nick
                    (concat nick " (" channel ")")))
           (msg (s-trim (s-collapse-whitespace message))))
      (alert (concat nick ": " msg) :title title)))

  (defun my/erc-preprocess (string)
    "Avoids channel flooding."
    (setq str
          (string-trim
           (replace-regexp-in-string "\n+" " " str))))

  :hook ((ercn-notify . my/erc-notify)
         (erc-send-pre . my/erc-preprocess))

  :custom
  (erc-autojoin-timing 'ident)
  ;; TODO These lines create some sort of margin, no space on laptop screens :|
  ;; (erc-fill-function 'erc-fill-static)
  ;; (erc-fill-static-center 22)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-password nil)
  (erc-prompt-for-nickserv-password nil)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-nick "anlsh")
  (erc-server-list '(("irc.freenode.net"
                      :port "6697"
                      :ssl t
                      :nick "anlsh")))
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-enable)
  (erc-update-modules))
