;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
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
		       ("smk.rule" "smk.rule.${1:$$(yas-choose-value '(\"bash\" \"r\"))}\n# -*- mode: snippet -*-\n# key: smk.rule\n# expand-env: ((yas-indent-line 'none))\n# --\n#+begin_src snakemake\nrule ${1:$$(downcase(replace-regexp-in-string \" \" \"_\" yas-text))}:\n    input:\n    log: f\"{log_dir}/$1.log\",\n    output:\n    params: script = $1\n    shell:\n        \"\"\"\n        \"\"\"\n$0\n#+end_src\n" "smk.rule" nil nil
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
		       ("smk" "`(make-string (org-current-level) ?*)` [[file:./workflow/$2.smk][Snakefile]]:smk:\n:PROPERTIES:\n:header-args:snakemake: :tangle ./workflow/${2: SNAKEFILE NAME}.smk\n:END:\n`(make-string (+ 1 (org-current-level)) ?*)` Smk preamble\n#+begin_src snakemake\n\n#+end_src\n`(make-string (+ 1 (org-current-level)) ?*)` Smk rules\n`(make-string (+ 2 (org-current-level)) ?*)` All rule\n#+begin_src snakemake\nrule all:\n    input:\n\n#+end_src\n- add rules with smk.rule\n`(make-string (+ 1 (org-current-level)) ?*)` Dev\n:PROPERTIES:\n:header-args:snakemake: :tangle no\n:END:\n`(make-string (+ 2 (org-current-level)) ?*)` Ideas\n:PROPERTIES:\n:header-args:snakemake: :tangle no\n:END:\n# -*- mode: snippet -*-\n# key: smk\n# expand-env: ((yas-indent-line 'none))\n# --\n`(make-string (org-current-level) ?*)` $3 :smk:\n:PROPERTIES:\n:header-args:snakemake: :tangle ./workflow/${1: SNAKEFILE NAME}.smk\n:END:\n[[file:./workflow/$1.smk][Link to Snakefile]]\n[[46270062-e3f4-46c9-9d71-5868376e495b][yas]]\n`(make-string (+ 1 (org-current-level)) ?*)` Preamble\n#+begin_src snakemake\n# ${3: <ORG DESCRIPTIVE TITLE>}\n#+end_src\n`(make-string (+ 1 (org-current-level)) ?*)` Use smk.rule\n`(make-string (+ 1 (org-current-level)) ?*)` Dev\n:PROPERTIES:\n:header-args:snakemake: :tangle no\n:END:\n`(make-string (+ 1 (org-current-level)) ?*)` Ideas\n:PROPERTIES:\n:header-args:snakemake: :tangle no\n:END:\n" "smk" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/smk" nil nil)
		       ("prop" ":properties:\n$0\n:end:\n" "prop" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/prop" nil nil)
		       ("org.hl" "<\\{$1\\}>\n" "org.hl" 'auto nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/org.hl" nil nil)
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
		       ("export.latex" "#+begin_export latex\n$0\n#+end_export\n" "export.latex" 'auto nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/export.latex" nil nil)
		       ("demo.org.header" "`(make-string (org-current-level) ?*)` THE HEADER\n:PROPERTIES:\n:END:\n`(make-string (+ 1 (org-current-level)) ?*)` BELOW THE HEADER\n" "demo.org.header" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/demo.org.header" nil nil)
		       ("code.yaml" "#+begin_src yaml\n$0\n#+end_src\n" "code.yaml" 'auto nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/code.yaml" nil nil)
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
		       ("code.perl" "#+begin_src perl\n$0\n#+end_src\n" "code.perl" 'auto nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/org-mode/code.perl" nil nil)
		       ("code.lisp" "#+begin_src emacs-lisp\n$1\n#+end_src$0\n" "code.lisp" 'auto nil
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


;;; Do not edit! File generated at Tue May  6 10:40:19 2025
