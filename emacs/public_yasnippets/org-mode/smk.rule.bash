# -*- mode: snippet -*-
# key: smk.rule.bash
# expand-env: ((yas-indent-line 'none))
# --
#+begin_src snakemake
rule ${1:$$(downcase(replace-regexp-in-string " " "_" yas-text))}:
    input: ${2: <INPUT>},
    log: f"{log_dir}/$1.log",
    output: ${3: <OUTPUT>},
    params: script = f"{${3:$$(replace-regexp-in-string " " "_" yas-text)}}/$1.sh",
    shell:
        """
        {params.script} {input} {output} > {log} 2>&1
        """
#+end_src

#+begin_src bash :tangle ./scripts/$1.sh
#!/usr/bin/env bash
#+end_src
