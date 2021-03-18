;; initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; resizing the window pixelwise, not on steps
(setq frame-resize-pixelwise t)

;; install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-alays-ensure t)

;; Packages
(use-package swiper)
(use-package diminish)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
(use-package pdf-tools
  :config
  (pdf-tools-install :no-query)
  (if (featurep 'evil-collection-pdf)
      (evil-collection-define-key 'normal 'pdf-view-mode-map ;; TODO move to general.el
	(kbd "C-SPC a") 'pdf-annot-add-highlight-markup-annotation
	(kbd "C-SPC s") 'pdf-annot-add-underline-markup-annotation ))
  )

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))
(use-package counsel
  :bind (("M-x" . counsel-M-x) ;; TODO move to general.el
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; evil configurations
(use-package undo-fu)
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; org-noter, used to take notes in a better way
(use-package org-noter
  :config
  (if (featurep 'evil-collection-pdf)
      (evil-collection-define-key 'normal 'pdf-view-mode-map
	"i" 'org-noter-insert-note))) ;; this mapping was necessary due to clashes with evil mode.

;; Org-specific configuration options
(dolist (mode '(org-indent-mode
		visual-line-mode))
(add-hook 'org-mode-hook mode))

;; Other configuration options
(setq custom-file (concat user-emacs-directory "/custom.el"))

(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		help-mode-hook
		pdf-view-mode-hook
		helpful-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

 ;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-quit)
