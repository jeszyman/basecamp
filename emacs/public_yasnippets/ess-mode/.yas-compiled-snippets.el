;;; Compiled snippets and support files for `ess-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'ess-mode
		     '(("var" "$1 = args[$2]\n" "var" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/public_yasnippets/ess-mode/var" nil nil)
		       ("script" "#!/usr/bin/env Rscript\n\n# ---   Load Packages   --- #\n# ------------------------- #\n\npackages <- c(\"optparse\", \"tidyverse\")\nsapply(packages, require, character.only = TRUE, quietly = TRUE)\n\n# ---   Load Inputs   --- #\n# ----------------------- #\n\noption_list <- list(\n  make_option(c(\"--<OPTION NAME>\"), type = \"character\"),\n\nopts <- parse_args(OptionParser(option_list = option_list))\n\ninputs_list = function(opts){\n\n  return(list())\n}\n\n# ---   Main   --- #\n# ---------------- #\n\nmain = function(opts){\n\n  # Process inputs\n  inputs = inputs_list(opts)\n\n  # Return a list of outputs\n  return(list())\n}\n\n# ---   Functions   --- #\n# --------------------- #\n\n# ---   Run   --- #\n# --------------- #\n\nout = main(opts)\n\nfor (name in names(out)) {\n  item <- out[[name]]\n  file_path <- file.path(opts$out_dir, paste0(name, \".rds\"))\n  saveRDS(item, file_path)\n}\n" "script" nil nil
			((yas-indent-line 'fixed))
			"/home/jeszyman/.emacs.d/public_yasnippets/ess-mode/script" nil nil)
		       ("optparse" "option_list <- list(\n  make_option(c(\"--\"), type = \"character\", default = \"\")\n)\n\nopts <- parse_args(OptionParser(option_list = option_list))\n" "optparse" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/public_yasnippets/ess-mode/optparse" nil nil)
		       ("make.opt" "  make_option(c(\"--$1\"), type = \"character\", default = \"$2\"),\n" "make.opt" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/public_yasnippets/ess-mode/make.opt" nil nil)
		       ("long.comment2" "${1:Insert your comment here}\n" "long.comment2" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/public_yasnippets/ess-mode/long.comment2" nil nil)
		       ("h2" "# ========== ${1:$$(capitalize yas-text)} ========= #\n\n$0\n" "h2" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/public_yasnippets/ess-mode/h2" nil nil)
		       ("function" "${1:FUNCTIONNAME$$(replace-regexp-in-string \" \" \"_\" yas-text)} = function(${2:VARIABLE LIST}){\n  # ${3:DESCRIPTION}\n  $0\n}\n" "function" nil nil
			((yas-indent-line 'fixed))
			"/home/jeszyman/.emacs.d/public_yasnippets/ess-mode/function" nil nil)
		       ("80" "#########1#########2#########3#########4#########5#########6#########7#########8\n" "80" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/public_yasnippets/ess-mode/80" nil nil)))


;;; Do not edit! File generated at Mon Aug  5 10:58:40 2024
