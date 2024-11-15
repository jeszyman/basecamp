(run-with-idle-timer
 1 nil
 (lambda ()
   (when (member "Hack" (font-family-list))
     (set-face-attribute 'default nil
                         :family "Hack"
                         :height 114
                         :weight 'light)
     (message "Font set to Hack"))))


(custom-set-faces
 '(default ((t (:family "Hack" :height 114 :weight light)))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (load-theme 'manoj-dark t)))

(load-theme 'manoj-dark t)
