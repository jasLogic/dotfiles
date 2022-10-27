;;; increase gc threshold
(setq gc-cons-threshold 10000000)
;;; garbage collect when losing focus or after being idle for 5s
(add-function :after after-focus-change-function (lambda () (unless (frame-focus-state) (garbage-collect))))
(run-with-idle-timer 5 t 'garbage-collect)

;;; make the custom file a temp file so that customizationsa are only temporary
(setq custom-file (make-temp-file "emacs-custom-file"))

;;; focus help window when opened
(setq help-window-select t)

;;; uniquify buffer names like file system paths
;;; TODO:
(setq uniquify-buffer-name-style 'forward)

;;; only ask y or n, not yes or no
(setq use-short-answers t)

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
  (fill-column 120)
  (indent-tabs-mode nil) ; don't use tabs to indent
  (tab-width 4)
  (minibuffer-prompt-properties ; do not allow the cursor in the minibuffer prompt
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :config
  (add-hook #'before-save-hook #'delete-trailing-whitespace) ; delete trailing whitespace on save
  (add-hook #'minibuffer-setup-hook #'cursor-intangible-mode) ; keep cursor out of cursor-intangible areas
  (global-display-line-numbers-mode 1))

;;; built-in packages:
(use-package dired
  :straight (:type built-in)
  :custom
  (dired-auto-revert-buffer t)
  (dired-kill-when-opening-new-dired-buffer t))

(use-package eldoc
  :straight (:type built-in)
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p 5))

(use-package hl-line
  :straight (:type built-in)
  :init (global-hl-line-mode 1))

(use-package re-builder
  :straight (:type built-in)
  :commands re-builder
  :bind ("C-c r" . re-builder)
  :custom (reb-re-syntax 'rx))

(use-package savehist
  :straight (:type built-in)
  :init (savehist-mode))

;;; move on logical lines instead of visual lines (except for text-mode, see below)
(use-package simple
  :straight (:type built-in)
  :custom (line-move-visual nil))

;;; wrap lines nicely in text modes and move on visual lines
(use-package text-mode
  :straight (:type built-in)
  :init
  (add-hook 'text-mode-hook (lambda ()
                              (visual-line-mode 1)
                              (setq-local line-move-visual t))))

(use-package tramp
  :straight (:type built-in)
  :custom (tramp-default-method "ssh"))

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

;;; external non-prog-mode packages:
(use-package all-the-icons)

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-s" . consult-line)
         ("C-c g" . consult-ripgrep)
         :map project-prefix-map
         ("b" . consult-project-buffer)
         :map goto-map
         ("g" . consult-goto-line)))

(use-package corfu
  :init (global-corfu-mode 1)
  :bind (:map corfu-map
              ("SPC" . corfu-insert-separator))
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

(use-package eglot
  :commands eglot
  :bind (("C-c l n" . flymake-goto-next-error)
         ("C-c l p" . flymake-goto-prev-error)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format)
         ("C-c l a" . eglot-code-actions)))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  :config (evil-set-initial-state 'vterm-mode 'emacs)
  :custom
  (evil-cross-lines t)
  (evil-move-beyond-eol t)
  (evil-respect-visual-line-mode t)
  (evil-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(corfu dired magit org))
  (evil-collection-init))

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
         ("C-c g" . magit-file-dispatch)))

(use-package moe-theme
  :config (load-theme 'moe-dark t))

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
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))) ; no line numbers
  (pdf-tools-install :no-query)
  :bind (:map pdf-view-mode-map ("C-s" . isearch-forward))) ; use pdf-tool specific search

;;; fix $PATH env variable on mac
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :config (exec-path-from-shell-initialize))

(use-package vterm
  :commands vterm
  :bind ("C-x t" . vterm)
  ;; disable line highlight: https://github.com/akermu/emacs-libvterm/issues/432#issuecomment-894230991
  :hook ((vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
         (vterm-copy-mode . (lambda () (hl-line-mode 'toggle))))
  :custom (vterm-buffer-name-string "vterm: %s")
  :config (set-face-attribute 'vterm-color-yellow nil :foreground "dark orange" :background "orange"))

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
              ("DEL" . (lambda () ; delete the whole word when completing a filename and the last character is '/'
                         (interactive)
                         (if (and minibuffer-completing-file-name (eq ?/ (char-before (point))))
                             (progn ; TODO: error when // or at ~/
                               (delete-backward-char 1)
                               (skip-chars-backward "^/")
                               (kill-line))
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
  (org-ellipsis " ▾")
  (org-startup-with-inline-images t)
  (org-startup-with-latex-preview t)
  ;; LaTeX
  (org-latex-caption-above nil) ; place caption below every element
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
