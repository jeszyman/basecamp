
;; :PROPERTIES:
;; :header-args: :tangle ./lib/lisp_function.el :tangle-mode (identity #o555) :mkdirp yes :noweb yes :comments org
;; :ID:       2fc1a6f1-093c-4250-a905-1bf42b7b9eee
;; :END:


(defun run-gnome-ctrl ()
  (start-process "gnome-control-center" "*Messages*" "env" "XDG_CURRENT_DESKTOP=GNOME" "gnome-control-center"))
