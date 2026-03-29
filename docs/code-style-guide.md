- [Commenting Verbosity](#org3fc3f39)
        - [How to use this table](#orgf3b2aa4)
        - [When NOT to comment](#org0816d1b)
      - [Bash](#org0fdffbb)
        - [Indentation](#orgefd4de4)
        - [Comments](#org4cbbf34)
        - [Comment template](#orgdef1d77)
        - [Scripts](#org6430c14)
        - [Script template](#org84d6bd9)
        - [Functions](#org852c4d7)
        - [References](#orgabbc0bc)
      - [Python](#orga580dfd)
        - [Indentation](#org4d9b991)
        - [Comments](#org7b853d1)
        - [Comment template](#orgb2560d8)
        - [Scripts](#org5366dd7)
        - [Script template](#org106988e)
        - [Functions](#orgefc1ed4)
        - [References](#org7c6f553)
      - [R](#orgcc2c913)
        - [Indentation](#orgcd7c5e6)
        - [Comment format](#org8f4d366)
        - [Cookbook / notebook conventions](#orgb4c0e46)
        - [Script structure (standalone R scripts)](#org92d465b)
        - [References](#orgd03b05f)
      - [Emacs Lisp](#org19fbd94)
        - [Comment format](#org30aa51d)
        - [Verbosity by context](#orgd819c93)
        - [Template](#orga6b642d)
      - [Snakemake](#orgdfcaf42)
        - [Comments](#org3af8a8d)
        - [Path and variable conventions](#orgba25b68)
        - [Rule construction](#org4c122c2)
        - [Shell blocks and scripts](#orgc1775e8)
        - [Configuration](#org25e5cf9)
        - [Rule template](#org8360642)
        - [References](#orgcc1d9a5)
      - [YAML / Config](#orga16d473)

Consolidated code style guide covering commenting verbosity, comment formatting, and language-specific conventions. Exported to `docs/code-style-guide.md` for Claude and other tool consumption.  


<a id="org3fc3f39"></a>

# Commenting Verbosity

Match commenting density to context. When the context is ambiguous, ask the user.  

| Context                                               | Verbosity      | What to comment                                                                             |
|----------------------------------------------------- |-------------- |------------------------------------------------------------------------------------------- |
| Cookbook / notebook (R/Python analysis in org-babel)  | Heavy          | Narrate analytical reasoning, explain why each step, section headers between logical phases |
| Standalone scripts (bash, Python)                     | Moderate–heavy | Top block comment, section banners, function docstrings, non-obvious logic                  |
| Snakemake rules (.smk)                                | Moderate       | Rule-level purpose, key params, workflow rationale                                          |
| Snakemake `script:` R/Python files                    | Moderate       | Explain the analytical step; pipeline context lives in the rule                             |
| Infrastructure elisp (defuns called from many places) | Moderate       | Function docstring, non-obvious logic                                                       |
| Config elisp (tangled from emacs.org)                 | Light          | One-liner above function; org source is canonical documentation                             |
| Ansible playbooks/tasks (YAML)                        | Light          | Task `name:` field is the comment; `#` only for non-obvious `when:` or workarounds          |
| Config YAML (conda envs, i3, etc.)                    | Minimal        | Comment only version pins or workarounds                                                    |
| Org-babel blocks in documentation/tutorial headings   | Heavy          | Reader is learning; treat like a cookbook                                                   |
| Quick org-babel snippets (exploratory, throwaway)     | Minimal        | Only if logic is non-obvious                                                                |
| Test code                                             | Light          | Test name describes intent; comment only surprising setup                                   |


<a id="orgf3b2aa4"></a>

## How to use this table

1.  Identify which row matches the code you are about to write
2.  Apply the verbosity level from that row
3.  Use the language-specific comment format conventions below
4.  If no row matches or the context is ambiguous, ask


<a id="org0816d1b"></a>

## When NOT to comment

-   Do not restate what the code does — explain *why*
-   Do not comment obvious stdlib calls or standard idioms
-   Do not add `TODO` comments without a corresponding task/ticket
-   Do not add comments that will immediately go stale (e.g. "this list has 5 items")


<a id="org0fdffbb"></a>

# Bash

Consolidated from [Bash and shell](basecamp.md) (under Programming languages).  


<a id="orgefd4de4"></a>

## Indentation

-   2 spaces, no tabs


<a id="org4cbbf34"></a>

## Comments

-   Wrap comments at 80 columns when practical; allow exceptions for unbreakable tokens (paths, commands, URLs) and for usage synopsis lines.
-   Comment types  
    -   Sectioning comments: major section headers use a separator banner with an UPPERCASE title; minor section subheaders use a separator banner with a Mixed-case title
    -   Block comments: multi-line explanations sectioned off with a banner (same width/style as other separators); not indented, not subheaders; subheaders are always single-line
    -   Standalone comments: single-line comments that precede the code they describe; used for short context or rationale
    -   Inline comments: on the same line as code, placed after two spaces; kept short, explain rationale rather than restating the code. Do not place inline comments after a line-continuation backslash (`\`) or after a here-doc delimiter (`<<EOF`), as this breaks parsing.


<a id="orgdef1d77"></a>

## Comment template

```bash
# =============================================================================
# SECTION: CONFIGURATION
# =============================================================================

# Default threads. Keep low for interactive nodes.
DEFAULT_THREADS=8

# -----------------------------------------------------------------------------
# Subsection: Input parsing
# -----------------------------------------------------------------------------

# parse_args parses CLI arguments and sets global config variables.
parse_args() {
  out_dir="${2%/}"  # normalize output directory (avoid trailing slash)

# -----------------------------------------------------------------------------
# Block comment: input validation rationale
#
# Validate inputs early so downstream functions can assume invariants.
# This keeps work functions simple and avoids duplicated checks.
# Fail fast with a targeted message if a required input is missing.
# -----------------------------------------------------------------------------

  if [[ ! -f "$1" ]]; then
    echo "Error: Input file not found: $1" >&2
    exit 1
  fi
}

# =============================================================================
# SECTION: WORK FUNCTIONS
# =============================================================================

run_tool() {
  echo "[INFO] Running tool" >&2  # log to stderr to keep stdout clean
}

# -----------------------------------------------------------------------------
# Subsection: Inline comment parse hazards
# -----------------------------------------------------------------------------

# BAD: inline comment after a continuation backslash breaks line continuation
echo "a" \  # breaks parsing because '\' is no longer the last character on the line
  "b"

# GOOD: comment above; backslash is the last character on the line
# Split command across lines for readability.
echo "a" \
  "b"

# BAD: inline comment after here-doc delimiter breaks the delimiter
cat <<EOF  # delimiter must be the last token on the line
hello
EOF

# GOOD: comment above; delimiter line contains only the delimiter token
# Print a small block of text.
cat <<EOF
hello
EOF
```


<a id="org6430c14"></a>

## Scripts

-   Overall structure: functions first, `main "$@"` last. `main()` is the entry point.
-   Shebang: `#!/usr/bin/env bash`
-   Documentation: top block comment describing script purpose
-   `print_usage()`: follows Unix man-page conventions (Usage line, Options, Arguments, Examples). Required arguments use `<UPPERCASE>` notation; optional in brackets.
-   `parse_args()`  
    -   The only function that may read positional args (`$1`, `$2`, `$@`, `$#`)
    -   Validates inputs, applies defaults, computes derived values
    -   Sets script configuration variables (globals) for downstream functions
    -   `-h|--help` prints usage and exits 0; misuse exits 2; runtime errors exit 1
-   `main()`: entry point; calls `parse_args "$@"`, then work functions in order
-   Work functions: consume named variables only, no CLI parsing, no new globals, assume preconditions satisfied
-   Final line: `main "$@"`
-   Error policy: stdout for machine-consumable output; stderr for all diagnostics


<a id="org84d6bd9"></a>

## Script template

```bash
#!/usr/bin/env bash

# -----------------------------------------------------------------------------
# Script name
# Purpose, inputs, outputs, assumptions, etc.
# -----------------------------------------------------------------------------

print_usage() {
  cat <<EOF
Usage: ${0##*/} [OPTIONS] <REQUIRED_ARG> [OPTIONAL_ARG]

Short description of what the command does.

Options:
  -h, --help        Show this help message and exit
  -o, --output DIR  Write output to DIR

Arguments:
  REQUIRED_ARG      Description
  OPTIONAL_ARG      Description

Examples:
  ${0##*/} input.txt
  ${0##*/} -o out input.txt
EOF
}

parse_args() {
  # 1) Check help first,
  #    exit 0 to stdout
  # 2) Check command line misuse (command invoked incorrectly),
  #    exit 2 to stderr
  # 3) Make input argument declarations
  # 4) Perform variable-specific runtime validations as needed,
  #    These exit 1 to stderr with variable-specific message
  # 5) Compute derived variables as needed
}

function_1() {}
function_2() {}

main() {
  parse_args "$@"
  function_1
  function_2
}

main "$@"
```


<a id="org852c4d7"></a>

## Functions

-   `lower_snake_case` names, opening brace on same line: `name() {`
-   In-script functions (parse-once model): `parse_args()` populates globals; other functions must not read positional args
-   Standalone reusable functions (sourced): accept inputs as positional parameters, immediately assign to `local` variables, declare all temporaries `local`


<a id="orgabbc0bc"></a>

## References

-   [Google Shell Style Guide](https://google.github.io/styleguide/shellguide.html)


<a id="orga580dfd"></a>

# Python

Consolidated from [Bash and shell → Python](basecamp.md) (under Programming languages).  


<a id="org4d9b991"></a>

## Indentation

-   4 spaces, no tabs


<a id="org7b853d1"></a>

## Comments

-   Wrap at 80 columns (Black default 88); exceptions for unbreakable tokens
-   Comment types  
    -   Sectioning comments: same banner style as bash (`# =====`, `# -----`)
    -   Docstrings: `"""..."""` as first statement in module/class/function
    -   Block comments: multi-line with banner, not indented, not subheaders
    -   Standalone comments: single line preceding code
    -   Inline comments: after two spaces, rationale not restatement


<a id="orgb2560d8"></a>

## Comment template

```python
# =============================================================================
# SECTION: CONFIGURATION
# =============================================================================


DEFAULT_THREADS = 8  # keep low for interactive nodes

# -----------------------------------------------------------------------------
# Subsection: Input parsing
# -----------------------------------------------------------------------------

def parse_args(argv: list[str]) -> "Args":

# -----------------------------------------------------------------------------
# Block comment
#
# Parse arguments once, validate inputs, and compute derived values here.
# Downstream functions should consume named attributes only.
# -----------------------------------------------------------------------------
```


<a id="org5366dd7"></a>

## Scripts

-   Overall structure: definitions first, `main()` last, `if __name__ == "__main__":` entry point
-   Shebang: `#!/usr/bin/env python3` only for directly executed files
-   Top module docstring describing purpose, inputs, outputs, assumptions
-   `argparse` for CLI parsing in `parse_args()`
-   `parse_args()`: only function that reads raw CLI args; returns structured object (dataclass or Namespace); `-h/--help` exits 0, CLI misuse exits 2, runtime errors exit 1
-   `main()`: entry point, short linear orchestration, calls `parse_args(argv)` then work functions
-   Work functions: consume named values only, prefer returning over mutating globals
-   Entry point: `if __name__ == "__main__": raise SystemExit(main())`
-   Error policy: stdout for machine-consumable output; stderr for all diagnostics


<a id="org106988e"></a>

## Script template

```python
#!/usr/bin/env python3
"""
Docstring
"""

import argparse
from pathlib import Path

# Centralize defaults as named constants so:
# - help text stays consistent
# - you do not repeat long strings
# - defaults are easy to change later
DEFAULT_INPUTS_DIR = "chaudhuri-lab-bucket1/jeszyman/projects/breast/inputs"
DEFAULT_OUTPUT_TSV = Path("./data/emseq-fastq-inputs.tsv")

def parse_args():
    parser = argparse.ArgumentParser(
        prog=Path(sys.argv[0]).name,
        description="Extract R1 FASTQ metadata from a GCS bucket.",
    )
    parser.add_argument(
        "--inputs-dir",
        default=DEFAULT_INPUTS_DIR,
        metavar="GCS_PATH",
        help=f"GCS path after gs:// (default: {DEFAULT_INPUTS_DIR})",
    )
    parser.add_argument(
        "--output-tsv",
        type=Path,
        default=DEFAULT_OUTPUT_TSV,
        metavar="TSV",
        help=f"Output TSV path (default: {DEFAULT_OUTPUT_TSV})",
    )
    args = parser.parse_args()
    return args

def main(argv: list[str] | None = None) -> int:
    """
    Orchestrate the script.

    Good practice:
    - parse args once
    - call work functions with named values
    - return an exit code (0 success; nonzero failure)
    """
    args = parse_args(argv)

    # Placeholder for the real work. Keep heavy logic out of parse_args.
    # Example:
    # run_work(inputs_dir=args.inputs_dir, output_tsv=args.output_tsv)
    return 0


if __name__ == "__main__":
    # Entry point:
    # - ensures the script does not run when imported as a module
    # - propagates main()'s integer return value as the process exit code
    raise SystemExit(main())

```


<a id="orgefc1ed4"></a>

## Functions

-   `lower_snake_case` names, type hints for public functions
-   Single-purpose, explicit parameters over globals
-   Reusable (library-style): parameterize inputs, return outputs, never print; raise specific exceptions, convert to exit codes only in `main()`


<a id="org7c6f553"></a>

## References

-   [Google Python Style Guide](https://google.github.io/styleguide/pyguide.html)


<a id="orgcc2c913"></a>

# R


<a id="orgcd7c5e6"></a>

## Indentation

-   2 spaces, no tabs


<a id="org8f4d366"></a>

## Comment format

-   **Sectioning comments**: use banner style consistent with bash/Python (`# =====` for major, `# ----` for minor)
-   **Roxygen-style docstrings** for functions: `#' @param`, `#' @return`, `#' @examples`
-   **Standalone comments**: single `#` line preceding code
-   **Inline comments**: after two spaces


<a id="orgb4c0e46"></a>

## Cookbook / notebook conventions

-   Heavy commenting is expected — narrate the analytical reasoning
-   Section headers between logical phases (data loading, filtering, transformation, visualization, output)
-   Explain *why* a filter threshold or parameter was chosen, not just what the code does
-   When a step produces an intermediate result, briefly describe what the result looks like (rows, columns, expected values)


<a id="org92d465b"></a>

## Script structure (standalone R scripts)

-   Follow tidyverse style (dplyr, ggplot2, readxl)
-   `variable_names_with_underscore`
-   Spaces before and after `=`, `+`, `-`, `<-`, etc.; after comma
-   For Snakemake `script:` files: use `snakemake@input`, `snakemake@output`, `snakemake@log`


<a id="orgd03b05f"></a>

## References

-   [Tidyverse style guide](https://style.tidyverse.org/)
-   [Google R style guide](https://google.github.io/styleguide/Rguide.html)
-   [Bioconductor coding style](https://contributions.bioconductor.org/r-code.html)


<a id="org19fbd94"></a>

# Emacs Lisp


<a id="org30aa51d"></a>

## Comment format

-   `;` — inline (end of line), rare
-   `;;` — standalone comment preceding code (most common)
-   `;;;` — section header
-   `;;;;` — top-level file section header


<a id="orgd819c93"></a>

## Verbosity by context

-   **Config functions** (tangled from emacs.org/private.org): one-liner `;;` above the function. The org prose is the documentation; the tangled file just needs enough to grep.
-   **Infrastructure defuns** (called from many places, hooks, capture templates): full docstring as first form in the function body, plus `;;` for non-obvious logic.
-   **Use-package blocks**: generally no comments needed; the package name and `:config` are self-documenting.


<a id="orga6b642d"></a>

## Template

```
;;;; Navigation utilities

;;; Filter helm results to exclude structural headings
(defun jg/helm-filter-example (candidate)
  "Return non-nil if CANDIDATE should be shown in helm."
  (not (string-match-p ":nohelm:" candidate)))  ; tag-based exclusion
```


<a id="orgdfcaf42"></a>

# Snakemake

Style conventions for Snakemake workflow files. For workflow architecture (wrapper vs module types, experiment grouping), see the full [Snakemake Style Guide and Good Practices](basecamp.md).  


<a id="org3af8a8d"></a>

## Comments

-   `#` comments follow Python conventions
-   Rule-level: brief docstring comment immediately after `rule name:` explaining purpose and key I/O
-   Workflow-level: section banners between logical groups of rules
-   Do not comment individual `input:` / `output:` / `params:` lines unless non-obvious


<a id="orgba25b68"></a>

## Path and variable conventions

-   Hungarian notation prefixes: `D_` for data directories (absolute paths from config), `R_` for repository locations, `CONDA_` for conda environment YAML files
-   Build paths using Python f-strings with explicit structure: `f"{D_EMSEQ}/align/{{sample}}.bam"`
-   Escape wildcards in f-strings with double braces
-   Paths should be declarative and reflect intended output organization
-   Avoid `os.path.join` unless necessary for cross-platform compatibility
-   Avoid encoding workflow logic or conditionals inside path expressions


<a id="org4c122c2"></a>

## Rule construction

-   Rules describe what is produced, not how it is computed
-   Prefix modular workflow rules with the workflow ID (e.g., `rule emseq_align`)
-   Standard directive order:  
    1.  message
    2.  conda
    3.  input
    4.  log
    5.  benchmark
    6.  params
    7.  threads
    8.  output
-   Always parameterize threads as a directive; never hard-code thread counts in shell or scripts
-   Each rule specifies its conda environment
-   Every rule defines a log file; redirect stdout/stderr explicitly so logs land in `D_LOG`


<a id="orgc1775e8"></a>

## Shell blocks and scripts

-   Prefer external scripts over inline shell; scripts should be runnable outside Snakemake with explicit arguments
-   Keep log and workflow logic in the Snakefile, external to the script
-   Echo an execution-specific message: `echo "[tool] $(date) lib={wildcards.library_id} threads={threads}"`
-   Redirect stdout/stderr explicitly (e.g., `exec > "{log}" 2>&1`)
-   Avoid embedding complex logic in `run` or `shell` blocks


<a id="org25e5cf9"></a>

## Configuration

-   Prefer structured (nested) YAML over flat keys: `config["ref"]["index"]` not `config["ref_index"]`
-   Two-layer config: base (committed to repo) + override (runtime via `--configfile`)
-   Use booleans for conditional execution; express conditional branches in `rule all`
-   Use tabular configuration (TSV/CSV) for sample/unit metadata; load once in the preamble


<a id="org8360642"></a>

## Rule template

```snakemake
# =============================================================================
# SECTION: ALIGNMENT
# =============================================================================

# Align reads to reference genome using bwa-mem2.
# Input: trimmed FASTQs; Output: sorted BAM + index.
rule align_reads:
    input:
        r1 = rules.trim.output.r1,
        r2 = rules.trim.output.r2,
    output:
        bam = D_ALIGNED / "{sample}.bam",
    threads: 8
    conda: ENV_BIOTOOLS
    shell:
        "bwa-mem2 mem -t {threads} {config[ref_genome]} "
        "{input.r1} {input.r2} | samtools sort -o {output.bam}"
```


<a id="orgcc1d9a5"></a>

## References

-   [Snakemake documentation](https://snakemake.readthedocs.io/en/stable/)
-   [Python style guide](#orga580dfd) (comments in .smk files follow Python conventions)


<a id="orga16d473"></a>

# YAML / Config

-   Minimal commenting — the structure is self-documenting
-   Comment version pins: `# pinned for compat with X`
-   Comment workarounds: `# workaround for bug in Y`
-   Ansible tasks: the `name:` field IS the comment; add `#` only for non-obvious `when:` conditions
