(require 'org)

(defvar t4n/literate-org-file  (concat user-emacs-directory "config.org"))
(defvar t4n/config-file (concat user-emacs-directory "config.el" ))

(if (file-exists-p t4n/literate-org-file)
    (progn				; tangle the literate file
      (find-file t4n/literate-org-file)
      (org-babel-tangle)))

(load-file t4n/config-file)
(byte-compile-file t4n/config-file) 	;load the config file
