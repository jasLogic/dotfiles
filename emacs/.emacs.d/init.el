;;; increase gc threshold
(setq gc-cons-threshold 10000000)
;;; garbage collect when losing focus or after being idle for 5s
(add-function :after after-focus-change-function (lambda () (unless (frame-focus-state) (garbage-collect))))
(run-with-idle-timer 5 t 'garbage-collect)

;;; focus help window when opened
(setq help-window-select t)

;;; uniquify buffer names like file system paths
;;; TODO:
(setq uniquify-buffer-name-style 'forward)

;;; don't show startup screen
(setq inhibit-startup-message t)

;;; disable scroll bar
(scroll-bar-mode -1)
;;; disable toolbar
(tool-bar-mode -1)
;;; disable menubar
(menu-bar-mode -1)

;; right alt as normal alt on darwin
(when (eq system-type 'darwin)
  (setq ns-right-alternate-modifier 'none))

(setq tab-always-indent 'complete)

(when (member "DejaVuSansMono Nerd Font Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVuSansMono Nerd Font Mono-12")
  (set-frame-font "DejaVuSansMono Nerd Font Mono-12" t t))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

;;; random stuff not associated to a package
(use-package emacs
  :straight (:type built-in)
  :custom
  (custom-file (make-temp-file "emacs-custom-file")) ; make customizations temporary
  (delete-by-moving-to-trash t)
  (display-line-numbers-width 3) ; min. 3 characters in line numbers column
  (fill-column 120)
  (history-length 25)
  (indent-tabs-mode nil) ; don't use tabs to indent
  (tab-width 4)
  (minibuffer-prompt-properties ; do not allow the cursor in the minibuffer prompt
   '(read-only t cursor-intangible t face minibuffer-prompt))
  ;;(native-comp-async-report-warnings-errors 'silent)
  (sentence-end-double-space nil)
  (use-short-answers t) ; only ask y or n, not yes or no
  (user-full-name "Jaslo Ziska")
  (user-mail-address "jaslo@jaslogic.tech")
  (warning-minimum-level :error)
  :config
  (add-hook #'before-save-hook #'delete-trailing-whitespace) ; delete trailing whitespace on save
  (add-hook #'minibuffer-setup-hook #'cursor-intangible-mode) ; keep cursor out of cursor-intangible areas
  (global-display-line-numbers-mode 1))

;;; built-in packages:
(use-package bibtex
  :straight (:type built-in)
  :custom (bibtex-align-at-equal-sign t))

(use-package calendar
  :straight (:type built-in)
  :config (calendar-set-date-style 'european))

(use-package dired
  :straight (:type built-in)
  :custom
  (dired-listing-switches "-l --almost-all --human-readable --si --group-directories-first")
  (dired-auto-revert-buffer t)
  (dired-kill-when-opening-new-dired-buffer t))

(use-package eldoc
  :straight (:type built-in)
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p 5))

(use-package flyspell
  :custom-face
  ;; make flyspell-incorrect face only underline in red (moe default is red background)
  (flyspell-incorrect ((t (:foreground unspecified :background unspecified :underline "#ef2929")))))

(use-package hl-line
  :straight (:type built-in)
  :init (global-hl-line-mode 1))

(use-package image
  :straight (:type built-in)
  :init (add-hook 'image-mode-hook (lambda ()
                                     (auto-revert-mode 1)
                                     (display-line-numbers-mode -1) ; no line numbers
                                     (blink-cursor-mode -1))) ; disable cursor blinking
  :custom (image-use-external-converter t))

(use-package project
  :straight (:type built-in)
  :custom (project-vc-ignores '("./build/" "./target/" ".cache/"))
  :config (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m)))

(use-package re-builder
  :straight (:type built-in)
  :commands re-builder
  :bind ("C-c r" . re-builder)
  :custom (reb-re-syntax 'rx))

(use-package recentf
  :straight (:type built-in)
  :after (no-littering)
  :init (recentf-mode))

(use-package savehist
  :straight (:type built-in)
  :after (no-littering)
  :custom (savehist-additional-variables '(command-history kill-ring))
  :init (savehist-mode))

(use-package simple
  :straight (:type built-in)
  :custom (line-move-visual nil) ; move on logical lines instead of visual lines (except for text-mode, see below)
  :config (column-number-mode))

;;; wrap lines nicely in text modes and move on visual lines
(use-package text-mode
  :straight (:type built-in)
  :init
  (add-hook 'text-mode-hook (lambda ()
                              (visual-line-mode 1)
                              (setq-local line-move-visual t))))

(use-package tramp
  :straight (:type built-in)
  :custom
  (tramp-default-method "ssh")
  :config (setq tramp-terminal-type "dump")) ; I don't know why but I need to set it to this value, otherwise it does not work...

(use-package vc-hooks
  :straight (:type built-in)
  :custom (vc-follow-symlinks t))

;;; TODO: trailing whitespace not working?
(use-package whitespace
  :straight (:type built-in)
  :hook prog-mode
  :custom
  (whitespace-line-column fill-column)
  (whitespace-style '(face trailing tabs lines-trail empty)))

(use-package windmove
  :straight (:type built-in)
  :commands (windmove-left windmove-down windmove-up windmove-right)
  :init (defvar-keymap windmove-hjkl-map)
  :bind (:map windmove-hjkl-map
              ("h" . windmove-left)
              ("j" . windmove-down)
              ("k" . windmove-up)
              ("l" . windmove-right)
              ("q" . delete-window)
              ("v" . split-window-below)
              ("s" . split-window-right)))

;;; external non-prog-mode packages:
(use-package all-the-icons)
(use-package all-the-icons-dired
  :hook dired-mode)

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-s" . consult-line)
         ("C-c g" . consult-ripgrep)
         ("C-c m" . consult-man)
         :map project-prefix-map
         ("b" . consult-project-buffer)
         ("C-b" . consult-project-buffer)
         :map goto-map
         ("g" . consult-goto-line)))

(use-package corfu
  :init (global-corfu-mode 1)
  :bind (:map corfu-map
              ("SPC" . corfu-insert-separator)
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 1)
  (corfu-auto-prefix 3)
  (corfu-cycle t)
  (corfu-echo-documentation t)
  :config
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active."
    (unless (bound-and-true-p vertico--input)
      (corfu-mode 1)
      (setq-local corfu-echo-documentation nil))) ; don't show doc while we are typing there
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

(use-package dwim-shell-command)

(use-package eglot
  :commands eglot
  :hook (LaTeX-mode . eglot-ensure)
  :bind (("C-c l n" . flymake-goto-next-error)
         ("C-c l p" . flymake-goto-prev-error)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format)
         ("C-c l a" . eglot-code-actions)))

;;; fix $PATH env variable on mac
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :config (exec-path-from-shell-initialize))

;;; highlight TODO, FIXME, ... (in programming modes)
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.6 :scale 1.0))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package magit
  :commands (magit-status magit-file-dispatch)
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch))
  :custom
  (magit-diff-refine-ignore-whitespace nil)
  (magit-diff-refine-hunk 'all))

(use-package magit-todos
  ;; fix meow not working inside magit (because of keymap property in magit-insert-section)
  :bind (:map magit-todos-section-map
         ("j" . nil)
         :map magit-todos-item-section-map
         ("j" . nil))
  :config (magit-todos-mode))

(use-package meow
  :custom
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-goto-line-function #'consult-goto-line)
  (meow-use-clipboard t)
  (meow-use-cursor-position-hack t)
  :config
  (when (not (assoc ?a meow-char-thing-table))
    (push '(?a . angle) meow-char-thing-table))
  (meow-thing-register 'angle '(pair ("<") (">")) '(pair ("<") (">")))
  (when (not (assoc ?$ meow-char-thing-table))
    (push '(?$ . dollar) meow-char-thing-table))
  (meow-thing-register 'dollar '(regexp "\\$" "\\$") '(regexp "\\$" "\\$"))
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("b" . consult-buffer)
   '("B" . list-buffers)
   (cons "p" project-prefix-map)
   '("v" . magit-status)
   '("V" . magit-file-dispatch)
   (cons "w" windmove-hjkl-map))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("=" . meow-indent)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '(":" . meow-goto-line)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . (lambda () (interactive) (move-end-of-line nil) (meow-insert)))
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . (lambda () (interactive) (if (region-active-p) (meow-kill) (meow-delete))))
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . (lambda () (interactive) (back-to-indentation) (meow-insert)))
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   '("p" . meow-yank)
   '("P" . consult-yank-from-kill-ring)
   '("q" . quit-window)
   '("Q" . kill-buffer)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-block)
   '("S" . meow-to-block)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))
  (meow-global-mode 1))

(use-package moe-theme
  :config (load-theme 'moe-dark t))

(use-package multi-vterm
  :after vterm
  :commands multi-vterm
  :bind ("C-x t" . multi-vterm))

(use-package no-littering
  :config
  ;; also place auto save file into var directory
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;;; https://emacs.stackexchange.com/questions/13314/install-pdf-tools-on-emacs-macosx
(use-package pdf-tools
  :mode "\\.pdf\\'"
  :magic-fallback "%PDF"
  :config
  (when (eq system-type 'darwin)
    (setq pdf-tools-handle-upgrades nil)
    (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (display-line-numbers-mode -1) ; no line numbers
                                  (blink-cursor-mode -1))) ; disable cursor blinking
  (pdf-tools-install :no-query)
  :bind (:map pdf-view-mode-map ("C-s" . isearch-forward))) ; use pdf-tool specific search

(use-package rainbow-delimiters
  :hook prog-mode)

(use-package vterm
  ;; disable line highlight: https://github.com/akermu/emacs-libvterm/issues/432#issuecomment-894230991
  :hook ((vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
         (vterm-copy-mode . (lambda () (hl-line-mode 'toggle))))
  :custom (vterm-buffer-name-string "vterm: %s")
  :config (set-face-attribute 'vterm-color-yellow nil :foreground "dark orange" :background "orange"))

(use-package which-key
  :hook (after-init . which-key-mode))

(use-package yasnippet
  :hook ((LaTeX-mode . yas-minor-mode)))

;;; minibuffer:
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package marginalia
  :init (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic orderless)))))

(use-package vertico
  :bind (:map vertico-map
              ("S-C-J" . vertico-next-group)
              ("S-C-K" . vertico-previous-group)
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("~" . (lambda ()
                       (interactive)
                       (beginning-of-line nil)
                       (kill-line)
                       (insert "~/")))
              ("DEL" . (lambda () ; delete the whole directory when completing a filename and the last character is '/'
                         (interactive)
                         (if (and minibuffer-completing-file-name (eq ?/ (char-before (point))))
                             (progn ; TODO: error when // or at ~/
                               (delete-backward-char 1)
                               (unless (zerop (skip-chars-backward "^/"))
                                 (kill-line)))
                           (delete-backward-char 1)))))
  :init (vertico-mode)
  :custom (vertico-cycle t))

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; org-mode config
(use-package org
  :straight (:type built-in)
  :config
  (plist-put org-format-latex-options :scale 2.0)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (emacs-lisp . t)
     (latex . t)
     (python . t)
     (screen . t)
     (shell . t)))
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images) ; redisplay images after running code
  :custom
  (org-confirm-babel-evaluate nil) ; don't ask before evaluating a code block
  (org-edit-src-content-indentation 0)
  (org-ellipsis " ▾")
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-image-actual-width 500)
  (org-startup-with-inline-images t)
  (org-startup-with-latex-preview t)
  ;; LaTeX
  (org-latex-packages-alist
   '(("AUTO" "babel" nil)
     ("per-mode=fraction" "siunitx" t) ; scientific units
     ("" "physics" t)))) ; abs, norm, braket, ...

(use-package org-roam
  :bind (("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :custom
  (org-roam-directory "~/org-roam")
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
      :unnarrowed t)
     ("q" "Quantum")
     ("qm" "Quantum Mechanics" plain "%?"
      :target (file+head "physics/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :Physics:QM:")
      :unnarrowed t)
     ("g" "Gardening" plain "%?"
      :target (file+head "gardening/%<%Y%m%d%H%M%S>-${sluf}.org" "#+title: ${title}\n#+filetags: :Gardening:")
      :unnarrowed t)))
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))
  :config (org-roam-db-autosync-mode))

(use-package htmlize)

;;; language specific
; TODO: maybe with (use-package auctex :mode ((...) . TeX-latex-mode)) ??
(use-package latex
  :straight auctex
  :mode (rx ".tex" eos)
  :custom
  (TeX-auto-save t)
  (TeX-engine 'xetex)
  (TeX-master nil)
  (TeX-parse-self t)
  (LaTeX-csquotes-open-quote "\\enquote{")
  (LaTeX-csquotes-close-quote "}")
  :config
  (setf (car (alist-get 'output-pdf TeX-view-program-selection)) "PDF Tools") ; use pdf-tools as pdf viewer
  (put 'LaTeX-mode 'flyspell-mode-predicate 'tex-mode-flyspell-verify) ; don't flyspell on LaTeX macros
  (add-hook 'TeX-language-ngerman-hook (lambda () (ispell-change-dictionary "de_DE")))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)) ; revert pdf buffer when finished compiling

(use-package cc-mode
  :straight (:type built-in)
  :custom
  (c-basic-offset 4)
  (c-backslash-column 79)
  (c-backslash-max-column (1- fill-column)))

(use-package cmake-mode)

(use-package c++-mode
  :straight (:type built-in)
  :mode (rx ".cu" (opt "h") eos)) ; for CUDA

(use-package dockerfile-mode)

(use-package dts-mode)

(use-package lua-mode
  :custom (lua-indent-level 4))

(use-package pyvenv)

(use-package rust-mode)

(use-package yaml-mode
  :mode (rx ".y" (opt "a") "ml" eos))
