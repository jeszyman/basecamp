# -*- mode: snippet -*-
# key: smk.rule.py
# expand-env: ((yas-indent-line 'none))
# --
#+begin_src snakemake
rule ${1:$$(replace-regexp-in-string " " "_" yas-text)}:
    input: ${2: <INPUT>},
    log: f"{log_dir}/$1.log",
    output: ${3: <OUTPUT>},
    params: script = f"{${3:$$(replace-regexp-in-string " " "_" yas-text)}}/$1.py",
    shell:
        """
        python {params.script} {input} {output} > {log} 2>&1
        """
#+end_src

#+begin_src python :tangle ./scripts/$1.py
#+end_src
