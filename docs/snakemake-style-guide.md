no r""" for shell  
message DOES NOT referenc especific inputs via wildcard  
. vs \_  
thread declarations in rule vs. param  

bespoke concurrency setup  

-   Scope and Philosophy  
    -   Intent: This guide defines conventions for Snakemake workflows, repositories, configuration, and rule structure. The primary goals are readability, reproducibility, and portability.
    -   Design Principles  
        -   Maintain a single source of truth.
        -   Encapsulate related functionality.
        -   Favor explicitness over cleverness, tolerating verbosity or repetition when necessary.
-   Repository Structure  
    -   Repository Types  
        -   Modular repositories: Collections of cohesive rules organized around a distinct data class and objective (RNA-seq, EM-seq, etc.).
        -   Project repositories: Wrapper Snakefiles that orchestrate multiple modular workflows, structured around specific project outcomes.
    -   Documentation  
        -   A README.md is required and must include:  
            -   A concise description of the workflow(s).
            -   A directed acyclic graph image of the current workflow(s).
            -   A changelog documenting workflow-level changes.
        -   Include a repository-internal minimum working example that functions in continuous integration testing.
    -   Version Management  
        -   Manage workflow versions with Git tags using semantic versioning.
        -   Tag format: wf/<workflow>/vX.Y.Z (e.g., wf/cfdna-cna/v2.1.0, wf/emseq/v1.7.1).
        -   Document workflow versions in both the README changelog and the org-file changelog.
        -   Org-file changelog  
            -   Each repository's main `.org` file contains a `**** Change Log` heading.
            -   Entries are reverse chronological list items with org timestamps: `- [YYYY-MM-DD Day] Description`.
            -   The top entry is always "Development since last tag" — a running accumulator for work not yet tagged to a version.
            -   When a version is tagged, the accumulator entries move under a `wf/<repo>/vX.Y.Z` bullet. The accumulator resets to empty.
            -   Entries describe functional changes (what was added, fixed, or changed), not commit-by-commit replay.
        -   Minimum Git workflow:  
            1.  Create the updated workflow.
            2.  When the workflow is ready for a version update, document changes in the README changelog and move org-file changelog accumulator entries under the new version bullet.
            3.  Update the README DAGs
            4.  Commit workflow and README updates: git add -A
            5.  Tag repository with workflow version: git tag -a wf/<WORKFLOW>/v<VERSION> -m "<MESSAGE>"
            6.  Push tag: git push origin master &#x2013;follow-tags
-   Workflow Architecture  
    -   Workflow Types  
        -   Project (Wrapper) Snakefile  
            -   Serves as the user-facing entry point.
            -   Defines global constants for use in modules.
            -   Loads configuration and overrides.
            -   Defines global directory constants derived from config.
            -   Loads unit tables and performs schema checks.
            -   Defines rule all as the single declaration of workflow outputs.
            -   Includes modular Snakefiles.
            -   The rule all should be a human-readable manifest of the project's exact deliverables; do not hide outputs in helper functions.
            -   Uses a two-layer configuration model:  
                -   Base (common) config committed to the repository.
                -   Optional override config provided at runtime via &#x2013;configfile or &#x2013;config.
        -   Module Snakefile  
            -   Encapsulates work rules for a cohesive data class (e.g., EM-seq, bulk RNA-seq).
            -   Enforces file organization below an abstracted data directory.
            -   Uses the older include directive, not the modules directive.
            -   Prefixes all rules with a module identifier.
            -   Does not define rule all.
            -   Does not contain constant definitions (e.g., D\_LOGS).
            -   Does not own global workflow logic.
            -   References variables provided by the wrapper.
            -   Intended for inclusion by the wrapper; not typically executed standalone.
    -   General Principles  
        -   Rules should read top-to-bottom.
        -   Avoid hiding workflow decisions inside helper functions when a rule pattern or explicit expansion would suffice.
-   Path and Variable Conventions  
    -   Paths should be declarative and reflect intended output organization.
    -   Build paths at the level of intended output organization. If an output directory structure is a meaningful stable construct, encode it at the level of each relevant rule.  
        -   Preferred: f"{D\_EMSEQ}/fastqs/trimmed/{{library\_id}}.trimmed\_R1.fastq.gz"
        -   Avoid: f"{D\_TRIMMED}/{{library\_id}}.trimmed\_R1.fastq.gz"
        -   Only perform this abstraction intentionally, such as when outputs write to a variable location for use in another workflow.
    -   Avoid encoding workflow logic or conditionals inside path expressions.
    -   When variables are fully predictable from wildcards, avoid lambda functions and reference directly: f"{config['data\_dir']}/bams/{{sample}}.sorted.bam"
    -   Hungarian notation prefixes for defined classes:  
        -   D\_ for data directories (absolute paths from config).
        -   R\_ for repository locations.
        -   CONDA\_ for conda environment YAML files.
    -   Build paths using Python f-strings with explicit structure (e.g., f"{D\_EMSEQ}/align/{{sample}}.bam").
    -   Escape wildcards in f-strings with double braces.
    -   Avoid os.path.join unless necessary for complex cross-platform compatibility; f-strings are more readable for this use case.
-   Configuration Files  
    -   YAML Configuration  
        -   Prefer structured (nested) YAML over flat keys.  
            -   Use nested mappings: config["ref"]["index"], not config["ref\_index"].
        -   Use booleans for conditional execution.  
            -   Express conditional branches in rule all or wrapper logic.
            -   Example:  
                -   In config: call\_variants: false
                -   In rule all: lambda wildcards: "analysis/09a\_variant\_annot/all.merged.filt.PASS.snpeff.vcf.gz.tbi" if config["call\_variants"] else []
        -   Project (wrapper) workflows use a two-layer YAML config pattern:  
            -   A common/base config checked into the repo that defines defaults.
            -   An override config passed at runtime via &#x2013;configfile or &#x2013;config that changes behavior without editing code.
    -   Tabular Configuration  
        -   Use tabular configuration (TSV/CSV) for sample- or unit-level metadata.
        -   Load once in the Snakefile preamble and index by identifier.
-   Sample and Experiment Grouping  
    -   Experiment-Level Configuration  
        -   When aggregating library-level outputs into experiment-level analyses:  
            -   Define experiment maps in YAML as nested dictionaries.
            -   Each top-level key represents an experiment.
            -   Values define shared parameters and the list of libraries.
        -   The Snakefile binds the YAML subtree once and treats it as canonical.
        -   Example YAML structure:
        -   In the Snakefile, bind the YAML subtree once and consume it consistently:
        -   Expand library-level inputs for a given experiment using the YAML-provided library list:
        -   Treat experiments as first-class workflow units by expanding experiment outputs in rule all:
        -   This approach keeps experiment definitions human-auditable and version-controlled as data while preserving rule-level usability as a Python dict-of-dicts.
        -   Dual-path workflows: When an experiment can use multiple methods (e.g., STAR vs Salmon alignment), include `align_method` as a list in the experiment map. Use it as a filename designator and expand across it in rule all:
        -   Method-conditional rules (e.g., BAM QC only for STAR path) gate on list membership:
-   Rule Construction  
    -   General Principles  
        -   Rules describe what is produced, not how it is computed.
        -   Abstract values and paths to descriptive common variables (see Path and Variable Conventions).
        -   If understanding a rule requires mentally executing Python code, the logic is likely misplaced.
    -   Naming Conventions  
        -   Prefix modular workflow rules with the workflow ID (e.g., rule align in workflow emseq.smk becomes rule emseq\_align).
    -   Configuration Access  
        -   Modular workflow rules should not directly reference config.
        -   Avoid: r1=f"{D\_EMSEQ}/fastqs/{config['raw\_subdir']}/{library\_id}\_R1.fastq.gz"
    -   Separation of Concerns  
        -   In mature workflows, Snakefile rules and referenced scripts fulfill separate concerns:  
            -   Workflow logic lives in Snakefiles; implementation logic lives in external scripts.
            -   Snakefiles express dependencies, data flow, and resources.
            -   Scripts perform computations.
        -   Make resource usage visible in the workflow and pass it as a variable to scripts.
    -   Standard Directive Order  
        -   Use the following order unless contraindicated:  
            1.  message
            2.  conda
            3.  input
            4.  log
            5.  benchmark
            6.  params
            7.  threads
            8.  output
    -   Threads  
        -   Always parameterize threads as a directive; never hard-code thread counts inside shell or scripts.
        -   Do not include threads in params.
    -   Conda Environments  
        -   Each rule specifies its conda environment.
    -   Logging and Output  
        -   A common D\_LOG convention exists.
        -   Every rule defines a log file.
        -   Redirect tool stdout and stderr explicitly so logs land in D\_LOG.
        -   Emit useful execution messages for traceability.
        -   Example: echo "[fastp] $(date) lib={wildcards.library\_id}"
    -   Shell Blocks and Scripts  
        -   Echo an execution-specific message:  
            -   Example: echo "[mosdepth] $(date) lib={wildcards.library\_id} ref={wildcards.emseq\_ref\_name} aln={wildcards.align\_method} threads={threads}"
        -   Prefer external scripts over inline shell; scripts should be runnable outside Snakemake with explicit arguments.
        -   Keep log and workflow logic in the Snakefile, external to the script.
        -   Redirect stdout and stderr explicitly (e.g., via exec > "{log}" 2>&1).
        -   Avoid embedding complex logic in run or shell blocks.
    -   Resource Declaration  
        -   Declare threads, memory, and other resources in rules, not hidden in scripts.
    -   Path References  
        -   Prefer paths relative to a master data directory or repository.
        -   Avoid hard-coded absolute paths.
-   Known Issues  
    -   Snakemake + mamba prefix collision: `mamba env create` fails with "Non-conda folder exists at prefix" when Snakemake manages per-rule conda environments. Workaround: use `--conda-frontend conda` flag. Observed 2026-03 with Snakemake 7.x + mamba.
-   See Also  
    -   [Python style guide](basecamp.md)
    -   <https://snakemake.readthedocs.io/en/stable/>

