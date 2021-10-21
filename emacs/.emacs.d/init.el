;;; init.el --- init file
;;; Commentary:
;;; Code:

;;; increase gc threshold
(setq gc-cons-threshold 10000000)
;;; garbage collect when losing focus or after being idle for 5s
(add-hook 'focus-out-hook 'garbage-collect)
(run-with-idle-timer 5 t 'garbage-collect)

;;; make the custom file a temp file so that customizationsa are only temporary
(setq custom-file (make-temp-file "emacs-custom-file"))

;;; focus help window when opened
(setq help-window-select t)

;;; uniquify buffer names like file system paths
(setq uniquify-buffer-name-style 'forward)

;;; only ask y or n, not yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;;; fix $PATH env variable on mac
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :config (exec-path-from-shell-initialize))

;;; tramp
(setq-default tramp-default-method "ssh")
(setq-default tramp-terminal-type "dump")

;;; don't show startup screen
(setq inhibit-startup-message t)

;;; disable scroll bar
(scroll-bar-mode -1)
;;; disable toolbar
(tool-bar-mode -1)
;;; disable menubar
(menu-bar-mode -1)

;;; fullscreen with [F11]
(global-set-key [f11] 'toggle-frame-fullscreen)

;; right alt as normal alt on darwin
(when (eq system-type 'darwin)
  (setq ns-right-alternate-modifier 'none))

;;; wrap lines in programming modes
(global-visual-line-mode 1)

;;; fill-column
(setq-default fill-column 120)

;;; show line numbers
(global-display-line-numbers-mode 1)

;;; highlight current line
(global-hl-line-mode 1)

;;; tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; delete trailing whitespace when saving a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; highlight useless whitespace (in programming modes)
(require 'whitespace)
(setq whitespace-line-column fill-column)
(setq whitespace-style '(face trailing tabs lines-tail empty))
(add-hook 'prog-mode 'whitespace-mode)

;;; set font
;(add-to-list 'default-frame-alist '(font . "DejaVuSansMono Nerd Font Mono-12"))
(add-to-list 'default-frame-alist '(font . "DejaVuSansMono Nerd Font Mono-9"))

(use-package monokai-theme
  :config
  (load-theme 'monokai t))

(use-package all-the-icons)

;;; configure mode-line
;;; nyan-mode (very important)
(use-package nyan-mode
  :commands nyan-create
  :custom
  (nyan-animate-nyancat t)
  (nyan-wavy-trail t)
  (nyan-bar-length 64))

(defvar-local mode-line-directory nil
  "Cache the directory for the mode line as the call to determine the directory is pretty expensive,
especially when using tramp.")

(defun mode-line-directory ()
  "Get the directory path.
If the current buffer is not a file just print the `default-directory'.
If it is a file print the relative path inside the projectile project or its complete path."
  (if buffer-file-name
      (if mode-line-directory
          mode-line-directory
        (let ((root (projectile-project-root)))
          (if root
              (setq mode-line-directory
                    (concat
                     (projectile-project-name root)
                     "/"
                     (if (not (file-equal-p default-directory root))
                         (file-relative-name default-directory root))))
            (setq mode-line-directory default-directory))))))

(defun mode-line-buffer-name ()
  "Get the name of the buffer without the uniquifying for buffers with the same name."
    (if buffer-file-name
        (file-name-nondirectory buffer-file-name)
      (buffer-name)))

;;; inspired by: https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
(setq-default mode-line-format
              (list mode-line-front-space
                    ;; line number
                    "(%l:"
                    ;; column number (will be highlighted when greater than fill-column)
                    '(:eval (propertize "%3C" 'face
                                        (if (and (bound-and-true-p whitespace-mode) (>= (current-column) fill-column))
                                            'mode-line-fill-column-face)))
                    ") "
                    ;; RO if the buffer is read-only
                    '(:eval (if buffer-read-only
                                (propertize "RO " 'face 'mode-line-read-only-face)))
                    ;; @ if the file is remote
                    '(:eval (if (and buffer-file-name (file-remote-p buffer-file-name))
                                (propertize "@ " 'face 'mode-line-emphasis)))
                    ;; path to the buffer
                    '(:eval (mode-line-directory))
                    ;; buffer name
                    '(:eval (propertize (mode-line-buffer-name) 'face 'mode-line-buffer-id))
                    ;; add a asterik (*) if the buffer is modified and not saved
                    '(:eval (if (buffer-modified-p)
                                "* "
                              "  "))
                    ;; major mode
                    ;;'(:propertize "(%m) " face mode-line-emphasis)
                    '(:eval (all-the-icons-icon-for-buffer))
                    ;; version control info
                    '(vc-mode vc-mode)
                    " "
                    ;; miscellaneous
                    '(global-mode-string global-mode-string)
                    ;; percentage of position in buffer
                    " %p "
                    ;; nyan
                    '(:eval (list (nyan-create)))
                    mode-line-end-spaces))

;;; custom faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-fill-column-face)

(set-face-attribute 'mode-line nil
                    :foreground "gray60" :background "gray20"
                    :box '(:line-width 6 :color "gray20" :style nil))
(set-face-attribute 'mode-line-inactive nil
                    :foreground "gray60" :background "gray50"
                    :box '(:line-width 6 :color "gray50" :style nil))
(set-face-attribute 'mode-line-emphasis nil
                    :foreground "gray80"
                    :weight 'bold)
(set-face-attribute 'mode-line-buffer-id nil
                    :foreground "#eab700"
                    :weight 'bold)

(set-face-attribute 'mode-line-read-only-face nil
                    :foreground "red3"
                    :weight 'bold)
(set-face-attribute 'mode-line-fill-column-face nil
                    :foreground "magenta")

(use-package dashboard
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-projects-backend 'projectile)
  (dashboard-items '((recents . 5)
                     (projects . 5)
                     (agenda . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package no-littering
  :config
  ;; also place auto save file into var directory
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(setq backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;;; show key binding completions
(use-package which-key
  :hook (after-init . which-key-mode))

;;; ace-window
(use-package ace-window
  :bind ([remap other-window] . ace-window))

;;; ivy
(use-package ivy
  :config
  (setq ivy-initial-inputs-alist
        (cons '(execute-extended-command . "^") ivy-initial-inputs-alist))
  (ivy-mode 1))

;;; ivy-rich
(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  (ivy-rich-project-root-cache-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;;; counsel
(use-package counsel
  :bind (([remap find-file] . counsel-find-file)
         ([remap execute-extended-command] . counsel-M-x)
         ([remap describe-function] . counsel-describe-function)
         ([remap describe-variable] . counsel-describe-variable)
         :map counsel-find-file-map
         ("RET" . ivy-alt-done)))

;;; swiper (better search using ivy)
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)))

;;; highlight TODO, FIXME, ...
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;;; parenthesis
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook 'show-smartparens-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; from emacs redux: https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;; remap C-a to `smarter-move-beginning-of-line'
(when (boundp 'visual-line-mode)
  (define-key visual-line-mode-map [remap move-beginning-of-line] #'smarter-move-beginning-of-line))
(global-set-key [remap move-beginning-of-line] #'smarter-move-beginning-of-line)


(require 'dired)
;;; don't create a new buffer when pressing RET (or e or f)
(define-key dired-mode-map [remap dired-find-file] 'dired-find-alternate-file)
;;; automatically revert dired buffers
(setq dired-auto-revert-buffer t)

;;; pdf-tools
;;; https://emacs.stackexchange.com/questions/13314/install-pdf-tools-on-emacs-macosx
(use-package pdf-tools
  :mode "\\.pdf\\'"
  :config
  (when (eq system-type 'darwin)
    (setq pdf-tools-handle-upgrades nil)
    (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))) ; no line numbers
  (pdf-tools-install)
  :bind (:map pdf-view-mode-map ("C-s" . isearch-forward))) ; use pdf-tool specific search

;;; man
(require 'man)
(global-set-key (kbd "C-c m") 'man)
(setq Man-switches "-a")

;;; magit
(use-package magit
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch)))

;;; projectile
(use-package projectile
  :commands (projectile-project-root projectile-project-name)
  :init (projectile-mode 1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy "Use ivy for projectile completions")
  (projectile-enable-caching t "Enable caching for better performance")
  :config
  (setq projectile-globally-ignored-directories (cons "build" projectile-globally-ignored-directories)))

;;; lsp
(use-package lsp-mode
  :commands (lsp lsp-format-buffer)
  :hook
  (rustic-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :custom (lsp-keymap-prefix "C-c l"))

(use-package lsp-ui
  :commands (lsp-ui-mode lsp-ui-peek-find-definitions lsp-ui-peek-find-references lsp-ui-imenu)
  :bind (([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . #'lsp-ui-peek-find-references)
         ("M--" . #'lsp-ui-imenu))
  :custom
  (lsp-ui-doc-delay 2 "Wait 2s before showing doc")
  (lsp-ui-peek-always-show t "Always show peek window, also if only 1 reference")
  (lsp-ui-sideline-enable nil))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package company
  :hook (prog-mode . company-mode))

;;; flycheck
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;;; yasnippet (needed for lsp completions)
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

;; language specific

;;; c
(require 'cc-mode)
(setq c-basic-offset 4)
(setq c-backslash-column 79)
(setq c-backslash-max-column (1- fill-column))

;;; cmake
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;;; cuda
(use-package c++-mode
  :ensure nil
  :mode ("\\.cu\\'" "\\.cuh\\'"))

;;; geiser (for scheme)
(use-package geiser)

;;; go
;;; use spaces instead of tabs with go mode
(use-package go-mode
  :mode "\\.go\\'"
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil))))

;;; jupyter notebook
(use-package ein
  :custom (ein:output-area-inlined-images t))

;;; lua
(use-package lua-mode
  :mode "\\.lua\\'"
  :custom (lua-indent-level 4))

;;; rust
(use-package rustic
  :config (add-hook 'before-save-hook (lambda () (when (eq major-mode 'rustic-mode)
                                                   (lsp-format-buffer))))
  :custom
  (rustic-lsp-format t "Let lsp do the formatting")
  (lsp-rust-analyzer-cargo-watch-command "clippy" "Use clippy instead of cargo check"))

(use-package yaml-mode
  :mode "\\.yml\\'")

;;; org
(require 'org)

(setq org-ellipsis " ▾")

(require 'ox-latex)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (emacs-lisp . t)
   (python . t)
   (screen . t)
   (shell . t)))

(setq org-latex-packages-alist
      '(("AUTO" "babel" nil) ; automatically use correct language in export
        ("per-mode=fraction" "siunitx" t))) ; scientific units

(setq org-startup-with-inline-images t)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images) ; redisplay images after running
(setq org-confirm-babel-evaluate nil) ; don't ask before evaluating a code block
(setq org-latex-caption-above nil) ; place caption below every element
(setq org-image-actual-width 600) ; width of inline images in px

(provide 'init)
;;; init.el ends here
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
