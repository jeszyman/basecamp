;;; Compiled snippets and support files for `public_yasnippets'
;;; Snippet definitions:
;;;
(yas-define-snippets 'public_yasnippets
		     '(("yas.var" "\\${${1:<NUMBER>}: <${2:VALUE$$(upcase yas-text)}>} $0\n" "yas.var" 'auto nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/bash-mode/yas.var" nil nil)
		       ("usepackage" "(use-package ${1: <PACKAGE>}\n  :bind-keymap\n  (\"\" . cmd)\n  :config\n  :ensure t\n  (setq )\n  :hook\n)\n" "usepackage" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/bash-mode/usepackage" nil nil)
		       ("r.cmd" "args = commandArgs(trailingOnly = TRUE)\n$0=args[1]\n=args[2]\n" "r.cmd" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/bash-mode/r.cmd" nil nil)
		       ("nested" "$1\n\n$2\n\n$3\n" "nested" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/bash-mode/nested" nil nil)
		       ("function.py" "def ${1: <FUNCTION NAME>}(${2: <ARGUMENT 1>}):\n    $0\n" "function.py" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/bash-mode/function.py" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'public_yasnippets
		     '(("head4" ";;; ---   ${1:$$(capitalize yas-text)}   --- ;;;\n;;; ${1:$(make-string (+ (string-width yas-text) 12) ?\\-)} ;;;\n$0\n" "head4" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/emacs-lisp-mode/head4" nil nil)
		       ("head3" ";; ---   ${1:$$(capitalize yas-text)}   --- ;;\n;; ${1:$(make-string (+ (string-width yas-text) 12) ?\\-)} ;;\n$0\n" "head3" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/emacs-lisp-mode/head3" nil nil)
		       ("head2" "; ---   ${1:$$(capitalize yas-text)}   --- ;\n; ${1:$(make-string (+ (string-width yas-text) 12) ?\\-)} ;\n$0\n" "head2" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/emacs-lisp-mode/head2" nil nil)
		       ("head1" "${1:$(make-string (+ (string-width yas-text) 12) ?\\;)}\n;;;   ${1:$$(capitalize yas-text)}   ;;;\n${1:$(make-string (+ (string-width yas-text) 12) ?\\;)}\n" "head1" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/emacs-lisp-mode/head1" nil nil)
		       ("function" "(defun ${1: <FUNCTION NAME>} (${2: <VARIABLE 1 NAME>})\n  \"${3: <DOC STRING>}\n\n$2 ${4: <VARIABLE 1 DEFINITION>}\n\nExample usage:\n  ($1 ${5: <USAGE>})\"\n\n$0\n" "function" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/emacs-lisp-mode/function" nil nil)
		       ("80" ";;;;;;;;;1;;;;;;;;;2;;;;;;;;;3;;;;;;;;;4;;;;;;;;;5;;;;;;;;;6;;;;;;;;;7;;;;;;;;;8\n" "80" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/emacs-lisp-mode/80" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'public_yasnippets
		     '(("var" "$1 = args[$2]\n" "var" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/ess-mode/var" nil nil)
		       ("script" "#!/usr/bin/env Rscript\n\n# ---   Load Packages   --- #\n# ------------------------- #\n\npackages <- c(\"optparse\", \"tidyverse\")\nsapply(packages, require, character.only = TRUE, quietly = TRUE)\n\n# ---   Load Inputs   --- #\n# ----------------------- #\n\noption_list <- list(\n  make_option(c(\"--<OPTION NAME>\"), type = \"character\"),\n\nopts <- parse_args(OptionParser(option_list = option_list))\n\ninputs_list = function(opts){\n\n  return(list())\n}\n\n# ---   Main   --- #\n# ---------------- #\n\nmain = function(opts){\n\n  # Process inputs\n  inputs = inputs_list(opts)\n\n  # Return a list of outputs\n  return(list())\n}\n\n# ---   Functions   --- #\n# --------------------- #\n\n# ---   Run   --- #\n# --------------- #\n\nout = main(opts)\n\nfor (name in names(out)) {\n  item <- out[[name]]\n  file_path <- file.path(opts$out_dir, paste0(name, \".rds\"))\n  saveRDS(item, file_path)\n}\n" "script" nil nil
			((yas-indent-line 'fixed))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/ess-mode/script" nil nil)
		       ("optparse" "option_list <- list(\n  make_option(c(\"--\"), type = \"character\", default = \"\")\n)\n\nopts <- parse_args(OptionParser(option_list = option_list))\n" "optparse" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/ess-mode/optparse" nil nil)
		       ("make.opt" "  make_option(c(\"--$1\"), type = \"character\", default = \"$2\"),\n" "make.opt" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/ess-mode/make.opt" nil nil)
		       ("long.comment2" "${1:Insert your comment here}\n" "long.comment2" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/ess-mode/long.comment2" nil nil)
		       ("h2" "# ========== ${1:$$(capitalize yas-text)} ========= #\n\n$0\n" "h2" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/ess-mode/h2" nil nil)
		       ("function" "${1:FUNCTIONNAME$$(replace-regexp-in-string \" \" \"_\" yas-text)} = function(${2:VARIABLE LIST}){\n  # ${3:DESCRIPTION}\n  $0\n}\n" "function" nil nil
			((yas-indent-line 'fixed))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/ess-mode/function" nil nil)
		       ("80" "#########1#########2#########3#########4#########5#########6#########7#########8\n" "80" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/ess-mode/80" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'public_yasnippets
		     '(("yas.var" "\\${${1:<NUMBER>}: <${2:VALUE$$(upcase yas-text)}>} $0\n" "yas.var" 'auto nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/fundamental-mode/yas.var" nil nil)
		       ("usepackage" "(use-package ${1: <PACKAGE>}\n  :bind-keymap\n  (\"\" . cmd)\n  :config\n  :ensure t\n  (setq )\n  :hook\n)\n" "usepackage" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/fundamental-mode/usepackage" nil nil)
		       ("r.cmd" "args = commandArgs(trailingOnly = TRUE)\n$0=args[1]\n=args[2]\n" "r.cmd" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/fundamental-mode/r.cmd" nil nil)
		       ("nested" "$1\n\n$2\n\n$3\n" "nested" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/fundamental-mode/nested" nil nil)
		       ("function.py" "def ${1: <FUNCTION NAME>}(${2: <ARGUMENT 1>}):\n    $0\n" "function.py" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/fundamental-mode/function.py" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'public_yasnippets
		     '(("yas.org" "`(make-string (org-current-level) ?*)` $2 :yas:\n#+begin_src bash :comment no :tangle ~/repos/basecamp/emacs/public_yasnippets/${1:org-mode}/$2\n# -*- mode: snippet -*-\n# key: ${2: <KEY>}\n# expand-env: ((yas-indent-line 'none))\n# --\n$0\n#+end_src\n" "yas.org" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/yas.org" nil nil)
		       ("smk_int" "`(make-string (org-current-level) ?*)` [[file:./workflow/int_test.smk][Snakefile]]:smk:\n:PROPERTIES:\n:header-args:snakemake: :tangle ./workflow/int_test.smk\n:END:\n`(make-string (+ 1 (org-current-level)) ?*)` Preamble\n#+begin_src snakemake\n# Python setup\nimport numpy as np\nimport pandas as pd\nimport re\n#+end_src\n`(make-string (+ 1 (org-current-level)) ?*)` All rule\n`(make-string (+ 1 (org-current-level)) ?*)` Symlink fastqs\n`(make-string (+ 1 (org-current-level)) ?*)` Includes statements\n" "smk_int" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/smk_int" nil nil)
		       ("smk.rulegraph" "#+ATTR_ORG: :width 800px\n[[file:resources/$1_smk.png]]\n" "smk.rulegraph" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/smk.rulegraph" nil nil)
		       ("smk.rule.r" "#+begin_src snakemake\nrule ${1:$$(replace-regexp-in-string \" \" \"_\" yas-text)}:\n    input: ${2: <INPUT>},\n    log: f\"{log_dir}/$1.log\",\n    output: ${3: <OUTPUT>},\n    params: script = f\"{${3:$$(replace-regexp-in-string \" \" \"_\" yas-text)}}/$1.R\",\n    shell:\n        \"\"\"\n        Rscript {params.script} {input} {output} > {log} 2>&1\n        \"\"\"\n#+end_src\n\n#+begin_src R :tangle ./scripts/$1.R\n#+end_src\n" "smk.rule.r" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/smk.rule.r" nil nil)
		       ("smk.rule.py" "#+begin_src snakemake\nrule ${1:$$(replace-regexp-in-string \" \" \"_\" yas-text)}:\n    input: ${2: <INPUT>},\n    log: f\"{log_dir}/$1.log\",\n    output: ${3: <OUTPUT>},\n    params: script = f\"{${3:$$(replace-regexp-in-string \" \" \"_\" yas-text)}}/$1.py\",\n    shell:\n        \"\"\"\n        python {params.script} {input} {output} > {log} 2>&1\n        \"\"\"\n#+end_src\n\n#+begin_src python :tangle ./scripts/$1.py\n#+end_src\n" "smk.rule.py" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/smk.rule.py" nil nil)
		       ("smk.rule.bash" "#+begin_src snakemake\nrule ${1:$$(downcase(replace-regexp-in-string \" \" \"_\" yas-text))}:\n    input: ${2: <INPUT>},\n    log: f\"{log_dir}/$1.log\",\n    output: ${3: <OUTPUT>},\n    params: script = f\"{${3:$$(replace-regexp-in-string \" \" \"_\" yas-text)}}/$1.sh\",\n    shell:\n        \"\"\"\n        {params.script} {input} {output} > {log} 2>&1\n        \"\"\"\n#+end_src\n\n#+begin_src bash :tangle ./scripts/$1.sh\n#!/usr/bin/env bash\n#+end_src\n" "smk.rule.bash" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/smk.rule.bash" nil nil)
		       ("smk.rule" "smk.rule.${1:$$(yas-choose-value '(\"bash\" \"r\"))}\n\n# -*- mode: snippet -*-\n# key: smk.rule\n# expand-env: ((yas-indent-line 'none))\n# --\n#+begin_src snakemake\nrule ${1:$$(downcase(replace-regexp-in-string \" \" \"_\" yas-text))}:\n    input:\n    log: f\"{log_dir}/$1.log\",\n    output:\n    params: script = $1\n    shell:\n        \"\"\"\n        \"\"\"\n$0\n#+end_src\n" "smk.rule" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/smk.rule" nil nil)
		       ("smk.rscript" "    params:\n        script = f\"{$2_script_dir}/${1:$$(replace-regexp-in-string \" \" \"_\" yas-text)}.R\"\n    shell:\n        \"\"\"\n        Rscript {params.script} \\\n        {input} \\\n        {output} > {log} 2>&1\n        \"\"\"\n#+end_src\n- [[file:./scripts/$1.R][Rscript]]\n  #+begin_src R :tangle ./scripts/$1.R\nr.script$0\n#+end_src\n" "smk.rscript" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/smk.rscript" nil nil)
		       ("smk.org.rule" "`(make-string (org-current-level) ?*)` ${2: <RULE DESCRIPTIVE NAME>}    :smk_rule:\n- Snakemake\n  #+begin_src snakemake\nrule ${1:$$(replace-regexp-in-string \" \" \"_\" yas-text)}:\n    benchmark: bench_dir + \"/$1.benchmark\",\n    container:\n    input:\n    log: log_dir + \"/$1.log\",\n    output:\nsmk.$0\n" "smk.org.rule" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/smk.org.rule" nil nil)
		       ("smk.bashscript" "    params:\n        script = script_dir + \"/${1:$$(replace-regexp-in-string \" \" \"_\" yas-text)}.sh\"\n    shell:\n        \"\"\"\n        {params.script} \\\n        {input} \\\n        {output} &> {log}\n        \"\"\"\n#+end_src\n#+begin_src bash :tangle ./scripts/$1.sh\n$0\n#+end_src\n" "smk.bashscript" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/smk.bashscript" nil nil)
		       ("smk" "`(make-string (org-current-level) ?*)` [[file:./workflow/$2.smk][Snakefile]]:smk:\n:PROPERTIES:\n:header-args:snakemake: :tangle ./workflow/${2: SNAKEFILE NAME}.smk\n:END:\n`(make-string (+ 1 (org-current-level)) ?*)` Smk preamble\n#+begin_src snakemake\n\n#+end_src\n`(make-string (+ 1 (org-current-level)) ?*)` Smk rules\n`(make-string (+ 2 (org-current-level)) ?*)` All rule\n#+begin_src snakemake\nrule all:\n    input:\n\n#+end_src\n- add rules with smk.rule\n`(make-string (+ 1 (org-current-level)) ?*)` Dev\n:PROPERTIES:\n:header-args:snakemake: :tangle no\n:END:\n`(make-string (+ 2 (org-current-level)) ?*)` Ideas\n:PROPERTIES:\n:header-args:snakemake: :tangle no\n:END:\n\n# -*- mode: snippet -*-\n# key: smk\n# expand-env: ((yas-indent-line 'none))\n# --\n`(make-string (org-current-level) ?*)` $3 :smk:\n:PROPERTIES:\n:header-args:snakemake: :tangle ./workflow/${1: SNAKEFILE NAME}.smk\n:END:\n[[file:./workflow/$1.smk][Link to Snakefile]]\n[[46270062-e3f4-46c9-9d71-5868376e495b][yas]]\n`(make-string (+ 1 (org-current-level)) ?*)` Preamble\n#+begin_src snakemake\n# ${3: <ORG DESCRIPTIVE TITLE>}\n#+end_src\n`(make-string (+ 1 (org-current-level)) ?*)` Use smk.rule\n`(make-string (+ 1 (org-current-level)) ?*)` Dev\n:PROPERTIES:\n:header-args:snakemake: :tangle no\n:END:\n`(make-string (+ 1 (org-current-level)) ?*)` Ideas\n:PROPERTIES:\n:header-args:snakemake: :tangle no\n:END:\n" "smk" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/smk" nil nil)
		       ("prop" ":properties:\n$0\n:end:\n" "prop" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/prop" nil nil)
		       ("open_choose" "#+begin_src bash\n\n#+end_src\n" "open_choose" nil nil
			((yas-indent-line 'fixed))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/open_choose" nil nil)
		       ("launch.docker.interactive" "#+begin_src bash :tangle ./src/launch_$2.sh\n#!/bin/bash\n#########1#########2#########3#########4#########5#########6#########7#########8\n#\n### Launch interactive settion for ${1:ACCOUNT}/${2:CONTAINER} ###\n#\n\n# Setup\nset -euxo pipefail\n\n# Commands\n##\n## If already in a docker environment, then exit\nif [ -f /.dockerenv ]; then\n echo \"shell already in docker, exiting\"\n exit 1\nfi\n\n##\n## Run\ndocker run \\\n       --env HOME=\\${HOME} \\\n       --hostname \\${HOSTNAME} \\\n       --interactive \\\n       --tty \\\n       --volume /home/:/home/ \\\n       --volume /tmp/:/tmp/ \\\n       --volume /mnt/:/mnt/ \\\n       --user $(id -u \\${USER}) \\\n       $1/$2 \\\n       /bin/bash\n#+end_src\n" "launch.docker.interactive" nil nil
			((yas-indent-line 'fixed))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/launch.docker.interactive" nil nil)
		       ("head.notangle" ":PROPERTIES:\n:header-args: :tangle no\n:END:\n" "head.notangle" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/head.notangle" nil nil)
		       ("google" "[[https://www.google.com/search?q=${1:$(replace-regexp-in-string \" \" \"+\" yas-text)}][google:$1]]$0\n" "google" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/google" nil nil)
		       ("demo.org.header" "`(make-string (org-current-level) ?*)` THE HEADER\n:PROPERTIES:\n:END:\n`(make-string (+ 1 (org-current-level)) ?*)` BELOW THE HEADER\n" "demo.org.header" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/demo.org.header" nil nil)
		       ("code.smk" "#+begin_src snakemake\n$0\n#+end_src\n" "code.smk" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/code.smk" nil nil)
		       ("code.sh" "#+begin_src bash\n$0\n#+end_src\n" "code.sh" 'auto nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/code.sh" nil nil)
		       ("code.r" "#+begin_src R\n$0\n#+end_src\n" "code.r" 'auto nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/code.r" nil nil)
		       ("code.py" "#+begin_src python\n$0\n#+end_src\n" "code.py" 'auto nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/code.py" nil nil)
		       ("code.prop" ":PROPERTIES:\n:header-args:${1:`(yas-choose-value '(\"R\" \"bash\" \" \"))`}: :tangle $2\n:END:\n$0\n" "code.prop" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/code.prop" nil nil)
		       ("code.lisp" "#+begin_src emacs-lisp\n$0\n#+end_src\n" "code.lisp" 'auto nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/code.lisp" nil nil)
		       ("code.latex" "#+begin_src latex\n$0\n#+end_src\n" "code.latex" 'auto nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/code.latex" nil nil)
		       ("code.js" "#+begin_src javascript\n$0\n#+end_src\n" "code.js" 'auto nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/code.js" nil nil)
		       ("anki.cloze" "`(make-string (org-current-level) ?*)` ${2:(${(capitalize yas-text)})} :anki:\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: ${1: <DECK>}\n:END:\n`(make-string (+ 1 (org-current-level)) ?*)` Text\n$0\n`(make-string (+ 1 (org-current-level)) ?*)` Extra\n" "anki.cloze" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/anki.cloze" nil nil)
		       ("80" "#########1#########2#########3#########4#########5#########6#########7#########8\n" "80" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/80" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'public_yasnippets
		     '(("script" "#!/usr/bin/env python3\n\"\"\"\n$1\n\"\"\"\n\n# ---   Load Packages   --- #\n# ------------------------- #\n\nimport argparse\n\n# ---   Load Inputs   --- #\n# ----------------------- #\n\ndef load_inputs():\n    parser = argparse.ArgumentParser(description=__doc__)\n    parser.add_argument(\"--your_option\", type=str, default=\"\", help=\"Description of option\")\n\n    args = parser.parse_args()\n\n    return args\n\n# ---   Main   --- #\n# ---------------- #\n\ndef main():\n\n# ---   Functions   --- #\n# --------------------- #\n\n# ---   Main Guard   --- #\n# ---------------------- #\n\nif __name__ == \"__main__\":\n    main()\n" "script" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/python-mode/script" nil nil)
		       ("head2" "# ---   ${1:$$(capitalize yas-text)}   --- #\n# ${1:$(make-string (+ (string-width yas-text) 12) ?\\-)} #\n$0\n" "head2" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/python-mode/head2" nil nil)
		       ("head1" "${1:$(make-string (+ (string-width yas-text) 12) ?\\#)}\n###   ${1:$$(capitalize yas-text)}   ###\n${1:$(make-string (+ (string-width yas-text) 12) ?\\#)}\n" "head1" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/python-mode/head1" nil nil)
		       ("function" "def ${1:FUNCTIONNAME$$(replace-regexp-in-string \" \" \"_\" yas-text)}(${2:PARAM$$(replace-regexp-in-string \" \" \"_\" yas-text)}):\n    \"\"\"\n    ${3: <DOC STRING>}\n\n    \"\"\"\n    $0\n" "function" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/python-mode/function" nil nil)
		       ("80" "#########1#########2#########3#########4#########5#########6#########7#########8\n$0\n" "80" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/python-mode/80" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'public_yasnippets
		     '(("variables" "variables(){\n    ${1:$$(replace-regexp-in-string \" \" \"_\" yas-text)}=\"\\${1${2:$$(when (and yas-moving-away-p (not (string= \"\" yas-text)))(concat \":-\" yas-text))}}\"\n    ${3:$$(replace-regexp-in-string \" \" \"_\" yas-text)}=\"\\${2${4:$$(when (and yas-moving-away-p (not (string= \"\" yas-text)))(concat \":-\" yas-text))}}\"\n}\n$0\n" "variables" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/variables" nil nil)
		       ("var" "$1=\"$\\{$2}\"$0\n" "var" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/var" nil nil)
		       ("usage" "print_usage(){\n    cat <<- EOF\n\n Usage: ${1:${1:SCRIPT NAME}$$(replace-regexp-in-string \" \" \"_\" yas-text)} [OPTIONS] ${2:${2:REQUIRED OPERAND}$$(upcase yas-text)}  ${3:${3:[OPTIONAL OPERAND]}$$(upcase yas-text)}\n \n ${4:<DESCRIPTION>}\n \n Options:\n   -h, --help    Show this help message and exit\n\n Arguements:\n   ${2:$(ignore-errors (format (yas-field-value 2)))} ${5: <OPERAND DESCRIPTION>} \n\n Example:\n   ${1:$(ignore-errors (format (yas-field-value 1)))} ${6: <EXAMPLE>} \n\nEOF\n}\n\n# See print usage caller yas\n# return_usage_any\n" "usage" nil nil
			((yas-indent-line 'fixed))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/usage" nil nil)
		       ("script" "#!/usr/bin/env bash\n\nhead1\n\n# Block comment here\n\nset -o errexit   # abort on nonzero exitstatus\nset -o nounset   # abort on unbound variable\nset -o pipefail  # don't hide errors within pipes\n\nusage\n\nvariables\n\nmain(){\n    variables /$1\n}\n\nmain \"$@\"\n" "script" nil nil nil "/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/script" nil nil)
		       ("return_usage_any" "# Return usage if any arguments are provided\nif [[ $# -gt 0 ]]; then\n    print_usage\n    return 0\nfi\n$0\n" "return_usage_any" nil nil
			((yas-indent-line 'fixed))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/return_usage_any" nil nil)
		       ("must.src" "if [[ \"\\${BASH_SOURCE[0]}\" == \"\\${0}\" ]]; then\n  echo \"You must source this script\"\n  exit 1\nfi\n" "must.src" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/must.src" nil nil)
		       ("head1" "${1:$(make-string (+ (string-width yas-text) 12) ?\\#)}\n###   ${1:$$(capitalize yas-text)}   ###\n${1:$(make-string (+ (string-width yas-text) 12) ?\\#)}\n$0\n" "head1" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/head1" nil nil)
		       ("function" "${1:$$(replace-regexp-in-string \" \" \"_\" yas-text)}(){\n    local ${2: <VARIABLE NAME> }=\"\\${1}\"\n    $0\n}\n" "function" nil nil
			((yas-indent-line 'fixed))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/function" nil nil)
		       ("doc.fun" "  [[ \"\\$1\" =~ (-h|--help) || -z \"\\$1\" ]] && {\n    cat <<EOF\nUsage: ${1: <FUNCTION NAME>} ${2: <USAGE>}\n${3: <DESCRIPTION>}\nEOF\n    return\n  }\n" "doc.fun" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/doc.fun" nil nil)
		       ("array.from.find" "${1:ARRAY NAME}=()\nwhile IFS=  read -r -d $'\\0'; do\n    $1+=(\"$REPLY\")\ndone < <(find ${2:FIND DIR} -name ${3:FIND TERM} -print0)\nprintf '%s\\n' \"\\${$1[@]}\"\n$0\n" "array.from.find" nil nil nil "/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/array.from.find" nil nil)
		       ("80" "##########1##########2##########3##########4##########5##########6##########7##########8\n$0\n" "80" nil nil
			((yas-indent-line 'fixed))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/80" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'public_yasnippets
		     '(("print_array" "printf '%s\\n' \"${${1:ARRAY_NAME}[@]}\"\n$0\n" "print_array" nil nil nil "/home/jeszyman/.emacs.d/snippets/public_yasnippets/shell-mode/print_array" nil nil)))


;;; Do not edit! File generated at Fri Nov  1 14:54:21 2024
