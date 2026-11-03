;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Load the local configuration of this device.
;;
;; The local configuration should configure the following variables:
;; - user-full-name
;; - user-mail-address
;; - org-directory
;; - org-roam-directory
;; Defaults for the configuration to be loaded
(setq user-full-name "Tancredi Covioli"
      user-mail-address "tancredi.covioli@gmail.com"
      org-directory "~/org"
      org-roam-directory "~/org/roam"
      )
(setq t4n/bib-bib "~/documenti/university/magistrale/tesi/references.bib"
      t4n/bib-library "~/documenti/university/magistrale/tesi/libreria"
      t4n/bib-notes "~/documenti/university/magistrale/tesi/note")

(setq require-final-newline nil)
(setq mode-require-final-newline nil)

(load! "./local_config.el" nil t)

;; Set BASH_ENV to source non-interactive files
;; Useful to set up commands and non interactive shells
(setenv "BASH_ENV" (expand-file-name "./scripts/source-non-interactive" doom-user-dir))

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; Change the theme depending on the time
(let ((hour (nth 2 (decode-time))))
  (if (and (> hour 7) (< hour 17))
      (setq doom-theme 'doom-one-light)
    (setq doom-theme 'doom-one)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;
;; Add a mapping to insert a note when org noter is active and pdf-view is enabled
;; in a buffer.
(map! (:when (modulep! :lang org +noter)
        :map pdf-view-mode-map
        :after org-noter
        :desc "Insert a note in org noter"
        :nvi "i" #'org-noter-insert-note))

;;
;; Add a dired mapping to search only in the filenames
;;
(map! :map dired-mode-map
      :desc "iSearch forward for filename"
      :localleader "/" #'dired-isearch-filenames
      )

;;
;; Add a global mapping to open dired in the other window
;;
(map! :desc "Open dired (other window)"
      :leader "o _" #'dired-jump-other-window)

;;
;; Add a mapping to open dired at the root of the project
;;
(map! :desc "Open project dired"
      :leader "p -" #'projectile-dired)
(map! :desc "Open project dired (other window)"
      :leader "p _" #'projectile-dired-other-window)

;;
;; Add a mapping to open the VC at the root of the project
;;
(defun +t4n/projectile-switch-vc ()
  "Switch to a project workspace and open its Magit status buffer."
  (interactive)
  (let ((+workspaces-switch-project-function #'projectile-vc))
    (projectile-switch-project)))

(map! :leader
      :prefix "p"
      :desc "VC project" "v" #'+t4n/projectile-switch-vc)

(after! org
  (defun t4n/org-roam-list-files ()
    "Return a list of all notes in the org-roam directory."
    (directory-files-recursively org-roam-directory "\\.org$"))
  ;; Include all Org-roam files in the Org-agenda
  (setq org-agenda-files
        (append
         (directory-files-recursively (file-name-concat org-roam-directory "office" "projects") "\\.org$")
         (list (file-name-concat org-roam-directory "office" "inbox.org"))))
  ;; Include roam files to refile target
  (add-to-list 'org-refile-targets '(t4n/org-roam-list-files :maxlevel . 3))
  ;; Custom Project Dashboard Agenda View
  (setq org-agenda-custom-commands
        '(("p" "Project Dashboard"
           ((todo "TODO|IN_PROGRESS|WAIT|STRT"
                  ((org-agenda-overriding-header "=== Open Tasks & Features ===")))
            (tags "meeting"
                  ((org-agenda-overriding-header "=== Past Meetings ===")
                   (org-agenda-sorting-strategy '(timestamp-down))
                   (org-agenda-remove-tags t)
                   (org-use-tag-inheritance nil)))))))

  ;; org capture templates
  (setq org-capture-templates '(
                                ("t" "Personal todo" entry
                                 (file+headline +org-capture-todo-file "Inbox")
                                 "* TODO %?\n%i\n%a" :prepend t)
                                ("n" "Personal notes" entry
                                 (file+headline +org-capture-notes-file "Inbox")
                                 "* %u %?\n%i\n%a" :prepend t)
                                ("j" "Journal" entry
                                 (file+olp+datetree +org-capture-journal-file)
                                 "* %U %?\n%i%?\n%a" :prepend t))))

(after! org-roam
  ;; Clean up the Backlinks sidebar view (hide unlinked text noise)
  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section))
  
  ;; Adjust context snippet length in the backlinks buffer
  (setq org-roam-preview-function 
        (lambda () (org-roam-preview-default-function 15)))

  ;; Display node title, tags, and origin file path in the completion prompt
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:15}" 'face 'org-tag) " "
                (propertize "(${file:30})" 'face 'font-lock-comment-face)))

  ;; Automatically generate an Org ID for captured headlines upon finalizing (C-c C-c)
  ;;(add-hook 'org-capture-prepare-finalize-hook #'org-id-get-create)

  ;; Org Roam capture templates
  (setq org-roam-capture-templates
        `(
          ;; Generic templates
          ("g" "Generic")
          ("n" "Note" entry
           ,(string-join
             `("* ${title} :note:"
               ":PROPERTIES:"
               ,(format org-property-format ":ID:" "%(org-id-new)") 
               ":END:"
               "%?")
             "\n")
           :target (file+olp "generic/inbox.org" ("Content Notes")))

          ;; Office-related templates
          ("o" "Office" )

          ("oc" "Content Note (Refile)" entry
           ,(string-join
             `("* ${title} :note:"
               ":PROPERTIES:"
               ,(format org-property-format ":ID:" "%(org-id-new)") 
               ":END:"
               "%?")
             "\n")
           :target (file+olp "office/inbox.org" ("Content Notes")))
          ("oi" "Inbox Note (Refile)" entry
           ,(string-join
             `("* TODO ${title}"
               ":PROPERTIES:"
               ,(format org-property-format ":ID:" "%(org-id-new)") 
               ":END:"
               "%?")
             "\n")
           :target (file+olp "office/inbox.org" ("Inbox")))
          ("om" "Meeting Note (Refile)" entry
           ,(string-join
             `("* MEETING [%<%Y-%m-%d>] ${title} :meeting:"
               ":PROPERTIES:"
               ,(format org-property-format ":ID:" "%(org-id-new)") 
               ":END:"
               "%?")
             "\n")
           :target (file+olp "office/inbox.org" ("Meetings")))
          )))

;; reduce the size of the icons
(setq all-the-icons-scale-factor 1.1)

;; Disable persistent undo history
(remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode)

;; when poetry is enabled, set a shortcut
(after! python
  (map! :map python-mode-map
        :when (modulep! :lang python +poetry)
        :localleader
        :desc "poetry" "p" #'poetry))


;; Add hook to stop hangs from git-commit
(add-hook! 'git-commit-setup-hook
  (corfu-mode -1))

;; Add advice to stop dictionary completions
(after! cape
  (advice-add 'cape-dabbrev :override #'ignore)
  (advice-add 'cape-dict :override #'ignore))

;; Set up openwith
(use-package! openwith
  :config
  (openwith-mode t)
  (setq openwith-associations
        '(
          ;; Route PDFs and Images to your OS default viewer (xdg-open)
          ("\\.\\(pdf\\|png\\|jpg\\|jpeg\\|svg\\)\\'" "xdg-open" (file))
          )))

;;
;; Uni stuff, now obsolete
;;

                                        ;(setq bibtex-completion-bibliography t4n/bib-bib ; my bibliography pdf location
                                        ;      bibtex-completion-library-path t4n/bib-library ; my pdf lib location
                                        ;      bibtex-completion-notes-path t4n/bib-notes)
                                        ;
                                        ;(after! org-noter
                                        ;  (setq
                                        ;   org-noter-always-create-frame t
                                        ;   org-noter-notes-search-path (list t4n/bib-notes)
                                        ;   org-noter-hide-other t
                                        ;   org-noter-auto-save-last-location nil
                                        ;   org-noter-doc-split-fraction '(0.60 . 0.5)
                                        ;   )
                                        ;  (add-hook 'org-noter-doc-mode-hook 'pdf-view-fit-width-to-window)
                                        ;  )
                                        ;
;;; tools: biblio configuration
                                        ;(after! citar
                                        ;  (setq citar-bibliography (list t4n/bib-bib)
                                        ;        citar-library-paths (list t4n/bib-library)
                                        ;        citar-notes-paths (list t4n/bib-notes))
                                        ;  )
