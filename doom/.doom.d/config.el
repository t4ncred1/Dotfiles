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
;; Optionally, if used:
;; - t4n/bib-bib
;; - t4n/bib-library
;; - t4n/bib-notes
;; Defaults for the configuration to be loaded
(setq user-full-name "Tancredi Covioli"
      user-mail-address "tancredi.covioli@gmail.com"
      org-directory "~/org"
      org-roam-directory "~/org/roam"
)
(setq t4n/bib-bib "~/documenti/university/magistrale/tesi/references.bib"
      t4n/bib-library "~/documenti/university/magistrale/tesi/libreria"
      t4n/bib-notes "~/documenti/university/magistrale/tesi/note")

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
      (setq doom-theme 'doom-nord-light)
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

(map! :map dired-mode-map
      :desc "iSearch forward for filename"
      :localleader "/" #'dired-isearch-filenames
)

;;
;; Capture templates
;;
(after! org
 (setq org-capture-templates '(
  ("t" "Personal todo" entry
  (file+headline +org-capture-todo-file "Inbox")
  "* TODO %?\n%i\n%a" :prepend t)
  ("n" "Personal notes" entry
  (file+headline +org-capture-notes-file "Inbox")
  "* %u %?\n%i\n%a" :prepend t)
  ("j" "Journal" entry
   (file+olp+datetree +org-capture-journal-file)
   "* %U %?\n%i%?\n%a" :prepend t)

 ;; ("p" "Templates for projects")
 ;; ("pt" "Project-local todo" entry
 ;;  (file +org-capture-project-todo-file)
 ;;  "* TODO %?\n%i\n%a" :prepend t)
 ;; ("pn" "Project-local notes" entry
 ;;  (file +org-capture-project-notes-file)
 ;;  "* %U %?\n%i\n%a" :prepend nil)
  ("o" "Centralized templates for projects")
  ("ot" "Project todo" entry #'+org-capture-central-project-todo-file
         "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
  ("on" "Project notes" entry #'+org-capture-central-project-notes-file
         "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
  ("oj" "Project journal" entry #'+org-capture-central-project-notes-file
         "* %U\n %i\n %a" :heading "Journal" :prepend t)
       )))

;; reduce the size of the icons
(setq all-the-icons-scale-factor 1.1)

;; Disable persistent undo history
(remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode)

;; when poetry is enabled, set a shortcut
(map! :map python-mode-map
      :when (modulep! :lang python +poetry)
      :localleader
      :desc "poetry" "p" #'poetry)

;;
;; Uni stuff, now obsolete but still viable
;;


(setq bibtex-completion-bibliography t4n/bib-bib ; my bibliography pdf location
      bibtex-completion-library-path t4n/bib-library ; my pdf lib location
      bibtex-completion-notes-path t4n/bib-notes)

(after! org-noter
  (setq
   org-noter-always-create-frame t
   org-noter-notes-search-path (list t4n/bib-notes)
   org-noter-hide-other t
   org-noter-auto-save-last-location nil
   org-noter-doc-split-fraction '(0.60 . 0.5)
   )
  (add-hook 'org-noter-doc-mode-hook 'pdf-view-fit-width-to-window)
  )

;; tools: biblio configuration
(after! citar
  (setq citar-bibliography (list t4n/bib-bib)
        citar-library-paths (list t4n/bib-library)
        citar-notes-paths (list t4n/bib-notes))
  )

;; (after! org-roam
;;   (setq org-roam-v2-ack t
;;         +org-roam-open-buffer-on-find-file nil
;;         org-roam-completion-everywhere nil
;;         org-roam-directory t4n/bib-notes))
(after! org-roam
  (setq org-roam-capture-templates
        '(;; Default templates
          ("d" "default"
           plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ;; Office-related templates
          ("o" "Office keymaps" )
          ;; Template for meeting notes
          ("om" "Meeting notes" entry
           "* %<%Y-%m-%d> Meeting notes - ${title}\n%?"
           :if-new (file+head
                    "office/meetings/%<%Y%m%d%H%M%S>_meeting_${slug}.org"
                    "#+title: Meeting notes - ${title}\n#+date: %T"
                    )
           :unnarrowed t)
          ("on" "Concept Notes" entry
           "* ${title}\n%?"
           :if-new (file+head
                    "office/notes/%<%Y%m%d%H%M%S>_note_${slug}.org"
                    "#+title: Concept Note - ${title}\n#+date: %T"
                    )
           :unnarrowed t)
          )))

;; (use-package! org-roam-bibtex :after org-roam
;;   :config
;;   (setq orb-preformat-keywords
;;       '("citekey" "title" "url" "author-or-editor" "keywords" "file")
;;       orb-process-file-keyword t
;;       orb-attached-file-extensions '("pdf")
;;       orb-roam-ref-format 'org-cite)
;;   (setq org-roam-capture-templates
;;         '(;; bibliography note template
;;           ("r" "bibliography reference" plain "%?"
;;            :if-new (file+head "${citekey}.org"
;;                               "#+title: Notes on ${title}\n* ${title}\n :PROPERTIES:\n  :Custom_ID: ${citekey}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-get-attached-file \"${citekey}\")\n  :NOTER_PAGE: \n  :END:\n\n")
;;            :unnarrowed t)
;;           ))
;;   (require 'org-roam-bibtex)
;;   (setq
;;         citar-open-note-function 'orb-citar-edit-note
;;         orb-preformat-keywords '("citekey" "title" "url" "author-or-editor" "keywords" "file")
;;         orb-process-file-keyword t
;;         orb-attached-file-extensions '("pdf")))
