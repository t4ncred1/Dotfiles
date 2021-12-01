;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-



;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tancredi Covioli"
      user-mail-address "tancredi.covioli@gmail.com")

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
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; Change the theme depending on the time
(let ((hour (nth 2 (decode-time))))
  (if (and (> hour 7) (< hour 17))
      (setq doom-theme 'doom-nord-light)
    (setq doom-theme 'doom-one)))


;; if you use `org' and don't want your org files in the default location below,q
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 't)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Add a mapping to insert a note when org noter is active and pdf-view is enabled
;; in a buffer.
(map! (:when (featurep! :lang org +noter)
       :map pdf-view-mode-map
       :after org-noter
       :desc "Insert a note in org noter"
       :nvi "i" #'org-noter-insert-note))

;; mappings for calc
(map! :leader
      (:prefix-map ("=" . "calc")
       "=" #'calc-dispatch
       "c" #'calc
       "q" #'quick-calc
       "g" #'calc-grab-region))

;; suggested config from :lang cc
(after! ccls
  (setq ccls-initialization-options '(:index (:comments 0) :completion (:detailedLabel t)))
  (set-lsp-priority! 'ccls 2)) ; optional as ccls is the default in Doom

;; haskell mode configuration
(after! haskell-mode
;; don't open new popup windows when showing haskell errors
 (setq haskell-interactive-popup-errors nil))

;;flycheck configuration
(after! flycheck
;; only check buffers with flycheck when the file is first opened or saved.
 (setq flycheck-check-syntax-automatically '(mode-enabled save)))

;; preview buffers
(setq +ivy-buffer-preview t)

;; if the battery is available, show it
(unless (string-match-p "^Battery status not available" (battery))
  (display-battery-mode 1))

;; display the time
(setq display-time-24hr-format 1)
(display-time-mode)
(setq display-time-interval 15)

;; customizations to be used on specific devices
(let ((distro (with-temp-buffer
  (insert-file-contents "/etc/issue")
  (nth 0 (split-string (buffer-string) nil t))
  )))
  ;; Only apply if the distribution is Debian
  (if (string-match-p "Debian" distro)
      (progn
       ;; change the default dictionary to english
       (setq debian-ispell-dictionary "english")
       (setq ispell-dictionary "english"))))
