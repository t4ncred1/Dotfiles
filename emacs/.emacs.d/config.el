;; [[file:~/.emacs.d/config.org::*Personal information][Personal information:1]]
(setq user-full-name "Tancredi Covioli"
      user-mail-address "tancredi.covioli@gmail.com")
;; Personal information:1 ends here

;; [[file:~/.emacs.d/config.org::*Change the bell][Change the bell:1]]
(setq visible-bell nil
      ring-bell-function 'double-flash-mode-line)
(defun double-flash-mode-line ()
  (let ((flash-sec (/ 1.0 20)))
    (invert-face 'mode-line)
    (run-with-timer flash-sec nil #'invert-face 'mode-line)
    (run-with-timer (* 2 flash-sec) nil #'invert-face 'mode-line)
    (run-with-timer (* 3 flash-sec) nil #'invert-face 'mode-line)))
;; Change the bell:1 ends here
