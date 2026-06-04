
# Table of Contents

1.  [Commenting Verbosity](#orgcce1bf7)
    1.  [How to use this table](#org9973d5c)
    2.  [When NOT to comment](#org8ffd27e)
2.  [Bash](#org0d33c51)
    1.  [Indentation](#org3ba0d17)
    2.  [Comments](#org924065a)
    3.  [Comment template](#orgcfb1c29)
    4.  [Scripts](#org946eecb)
    5.  [Script template](#org6a1284e)
    6.  [Functions](#org7a5581b)
    7.  [References](#org6492853)
3.  [Python Style Guide](#org7c6da57)
    1.  [Indentation](#org6dfdf42)
    2.  [Comments](#orgc9c0342)
    3.  [Comment template](#org16214db)
    4.  [Scripts](#org9cce286)
    5.  [Script template](#orgb0de687)
    6.  [Functions](#org1640505)
    7.  [References](#orgf7f45c7)
4.  [R](#org6a667a8)
    1.  [Indentation](#org1fbea59)
    2.  [Comments](#orgae535d0)
    3.  [Comment template](#org1dac12f)
    4.  [Cookbook / notebook conventions](#org2979d55)
    5.  [Cookbook section header template](#org9ade73b)
    6.  [Scripts](#org9561225)
    7.  [Script template](#org3205e2e)
    8.  [Functions](#orgef85fc7)
    9.  [Function template](#org64204f5)
    10. [References](#orgb5ef2e1)
5.  [Emacs Lisp](#org7ee706e)
    1.  [Comment format](#orgbf46073)
    2.  [Verbosity by context](#org145806b)
    3.  [Template](#org854aeb9)
6.  [Snakemake](#org120878c)
    1.  [Comments](#orgca23fc3)
    2.  [Path and variable conventions](#org89c530a)
    3.  [Rule construction](#org6e308fd)
    4.  [Shell blocks and scripts](#org3bfb0af)
    5.  [Configuration](#org046aaf8)
    6.  [Rule template](#orga2f2f71)
    7.  [References](#orgc2d7d82)
7.  [YAML / Config](#org62e6ea5)

Consolidated code style guide covering commenting verbosity, comment formatting, and language-specific conventions. Exported to `docs/code-style-guide.md` for Claude and other tool consumption.  

-   [Org-edit style guide](../org/work.md) — org structural conventions


<a id="orgcce1bf7"></a>

# Commenting Verbosity

Match commenting density to context. When the context is ambiguous, ask the user.  

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Context</th>
<th scope="col" class="org-left">Verbosity</th>
<th scope="col" class="org-left">What to comment</th>
</tr>
</thead>
<tbody>
<tr>
<td class="org-left">Cookbook / notebook (R/Python analysis in org-babel)</td>
<td class="org-left">Heavy</td>
<td class="org-left">Narrate analytical reasoning, explain why each step, section headers between logical phases</td>
</tr>

<tr>
<td class="org-left">Standalone scripts (bash, Python)</td>
<td class="org-left">Moderate–heavy</td>
<td class="org-left">Top block comment, section banners, function docstrings, non-obvious logic</td>
</tr>

<tr>
<td class="org-left">Snakemake rules (.smk)</td>
<td class="org-left">Moderate</td>
<td class="org-left">Rule-level purpose, key params, workflow rationale</td>
</tr>

<tr>
<td class="org-left">Snakemake <code>script:</code> R/Python files</td>
<td class="org-left">Moderate</td>
<td class="org-left">Explain the analytical step; pipeline context lives in the rule</td>
</tr>

<tr>
<td class="org-left">Infrastructure elisp (defuns called from many places)</td>
<td class="org-left">Moderate</td>
<td class="org-left">Function docstring, non-obvious logic</td>
</tr>

<tr>
<td class="org-left">Config elisp (tangled from emacs.org)</td>
<td class="org-left">Light</td>
<td class="org-left">One-liner above function; org source is canonical documentation</td>
</tr>

<tr>
<td class="org-left">Ansible playbooks/tasks (YAML)</td>
<td class="org-left">Light</td>
<td class="org-left">Task <code>name:</code> field is the comment; <code>#</code> only for non-obvious <code>when:</code> or workarounds</td>
</tr>

<tr>
<td class="org-left">Config YAML (conda envs, i3, etc.)</td>
<td class="org-left">Minimal</td>
<td class="org-left">Comment only version pins or workarounds</td>
</tr>

<tr>
<td class="org-left">Org-babel blocks in documentation/tutorial headings</td>
<td class="org-left">Heavy</td>
<td class="org-left">Reader is learning; treat like a cookbook</td>
</tr>

<tr>
<td class="org-left">Quick org-babel snippets (exploratory, throwaway)</td>
<td class="org-left">Minimal</td>
<td class="org-left">Only if logic is non-obvious</td>
</tr>

<tr>
<td class="org-left">Test code</td>
<td class="org-left">Light</td>
<td class="org-left">Test name describes intent; comment only surprising setup</td>
</tr>
</tbody>
</table>


<a id="org9973d5c"></a>

## How to use this table

1.  Identify which row matches the code you are about to write
2.  Apply the verbosity level from that row
3.  Use the language-specific comment format conventions below
4.  If no row matches or the context is ambiguous, ask


<a id="org8ffd27e"></a>

## When NOT to comment

-   Do not restate what the code does — explain *why*
-   Do not comment obvious stdlib calls or standard idioms
-   Do not add `TODO` comments without a corresponding task/ticket
-   Do not add comments that will immediately go stale (e.g. "this list has 5 items")


<a id="org0d33c51"></a>

# Bash

Consolidated from [Bash and shell](basecamp.md) (under Programming languages).  


<a id="org3ba0d17"></a>

## Indentation

-   2 spaces, no tabs


<a id="org924065a"></a>

## Comments

-   Wrap comments at 80 columns when practical; allow exceptions for unbreakable tokens (paths, commands, URLs) and for usage synopsis lines.
-   Comment types  
    -   Sectioning comments: major section headers use a separator banner with an UPPERCASE title; minor section subheaders use a separator banner with a Mixed-case title
    -   Block comments: multi-line explanations sectioned off with a banner (same width/style as other separators); not indented, not subheaders; subheaders are always single-line
    -   Standalone comments: single-line comments that precede the code they describe; used for short context or rationale
    -   Inline comments: on the same line as code, placed after two spaces; kept short, explain rationale rather than restating the code. Do not place inline comments after a line-continuation backslash (`\`) or after a here-doc delimiter (`<<EOF`), as this breaks parsing.


<a id="orgcfb1c29"></a>

## Comment template

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


<a id="org946eecb"></a>

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


<a id="org6a1284e"></a>

## Script template

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


<a id="org7a5581b"></a>

## Functions

-   `lower_snake_case` names, opening brace on same line: `name() {`
-   In-script functions (parse-once model): `parse_args()` populates globals; other functions must not read positional args
-   Standalone reusable functions (sourced): accept inputs as positional parameters, immediately assign to `local` variables, declare all temporaries `local`


<a id="org6492853"></a>

## References

-   [Google Shell Style Guide](https://google.github.io/styleguide/shellguide.html)


<a id="org7c6da57"></a>

# Python Style Guide


<a id="org6dfdf42"></a>

## Indentation

-   4 spaces, no tabs


<a id="orgc9c0342"></a>

## Comments

-   Code: format with Black (88-column target). Black is the source of truth for code-line wrapping; Black-unsplittable tokens (file paths, URLs, single long string/f-string literals) may exceed 88 — do not hand-wrap them.
-   Comments and docstring prose: wrap at 80 columns; allow exceptions for unbreakable tokens (paths, URLs). Black does not rewrap comment text, so this is a manual convention.
-   Section banners (the Comment-template `# ===` / `# ---` style) are OPTIONAL and scaled to the file: short single-purpose scripts may omit them; multi-section scripts should use them to mark major phases (e.g. CONFIGURATION, PARSE/CLI, WORK, MAIN). Label wording is free-form.
-   Comment types  
    -   Sectioning comments: a three-line banner — a rule line, a `# TITLE` line, then a rule line. Major sections use `#` + space + 77 `=` (79 cols) with an UPPERCASE title; subsections use `#` + space + 77 `-` with a Mixed-case title. See the Comment template.
    -   Docstrings: `"""..."""` as first statement in module/class/function. The first line of docstring prose is a one-line summary of purpose; it may sit on the same line as the opening `"""` (PEP 257 style) or on the line below a `"""` on its own line (as in the Script template). Either is acceptable; pick one per file.
    -   Block comments: multi-line with banner, not indented, not subheaders
    -   Standalone comments: single line preceding code
    -   Inline comments: after two spaces, rationale not restatement


<a id="org16214db"></a>

## Comment template

    # =============================================================================
    # SECTION: CONFIGURATION
    # =============================================================================
    
    
    DEFAULT_THREADS = 8  # keep low for interactive nodes
    
    # -----------------------------------------------------------------------------
    # Subsection: Input parsing
    # -----------------------------------------------------------------------------
    
    def parse_args():
        ...  # full body in the Script template below
    
    # -----------------------------------------------------------------------------
    # Block comment
    #
    # Parse arguments once, validate inputs, and compute derived values here.
    # Downstream functions should consume named attributes only.
    # -----------------------------------------------------------------------------


<a id="org9cce286"></a>

## Scripts

-   Overall structure (top to bottom): shebang, module docstring, imports, module-level constants, `parse_args()`, work functions, `main()`, then the `if __name__ == "__main__"` guard. A constant consumed by exactly one function may instead be defined immediately above that consumer; the sibling-import shim (below) follows the stdlib/third-party import block.
-   Shebang: `#!/usr/bin/env python3` only for directly executed files
-   Top module docstring. REQUIRED: a one-line purpose summary and a Usage line showing argument order. RECOMMENDED, and required when the I/O schema is non-obvious: Inputs and Outputs sections; Assumptions when correctness depends on caller-guaranteed invariants.
-   Imports grouped: stdlib, then third-party, then local — each group separated by a blank line. Within a group, list plain `import X` statements first (alphabetized), then `from Y import Z` statements (alphabetized by module), per PEP 8 / Google §3.13.
-   Sibling-module imports (scripts run as a file, not an installed package): after the other import groups, add `sys.path.insert(0, str(Path(__file__).resolve().parent))` on the line immediately preceding the sibling import, and annotate that import `# noqa: E402`. Keep the shim and its import together as the final import block; an explanatory comment naming the sibling is optional.
-   Module-level constants are UPPER\_SNAKE\_CASE — including computed constants (numpy lookup tables / power vectors, e.g. POWERS) and fixed column-name lists (e.g. METRIC\_COLUMNS), not just literal strings/Paths. Place them after imports, or immediately above the single function that uses them — whichever keeps the definition near its use. Give computed arrays an explicit dtype and a one-line comment explaining non-obvious math.
-   `argparse` for CLI parsing in `parse_args()`
-   `parse_args()`: only function that reads raw CLI args; ends with `return parser.parse_args()` and yields the argparse Namespace by default. Wrap the Namespace in a frozen dataclass ONLY when you need typed/validated/derived fields or immutability, and say why in a comment.
-   Help text: for scripts run by hand, give each `add_argument` a `help=` string and the parser a `description=` so `-h` is self-documenting; optionals taking a value MAY add `metavar=`. Strong recommendation, not a conformance gate — internal/pipeline-only scripts may omit it.
-   `main()`: top-level driver, short linear orchestration, calls `parse_args()` then work functions
-   Work functions: consume named values only, prefer returning over mutating globals
-   File handles: open files in a `with` statement so they close on every exit path, including exceptions. For a fixed small count, chain them in one `with`. For a variable number, use `contextlib.ExitStack` rather than storing raw handles and closing them in a loop. A factory that returns a handle is fine if the caller wraps the returned handle in `with`.
-   Blank lines: two between top-level defs/classes (and before the `if __name__` guard); one between methods inside a class (PEP 8 / Google Python Style Guide).
-   Entry point: `if __name__ == "__main__": raise SystemExit(main())`. `main()` returns the process exit code: `0` success, `1` for a runtime/checked-input failure; argparse itself exits `0` on `-h/--help` and `2` on CLI misuse. Prefer `parser.error()` (exit 2) for problems detectable from the arguments alone; a `return 1` from `main()` for a checked-but-failing input (e.g. missing directory) is also acceptable.
-   Error policy: stdout is reserved for machine-consumable output; all diagnostics, progress, and summaries go to stderr. Emit diagnostics with `print(..., file=sys.stderr)`; the `logging` module is not used in these scripts.
-   Long-running scripts emit a final stderr summary (counts/timing) and may stream incremental progress (per-record or every N records). Pass `flush=True` on progress prints when output must stay visible under nohup / GNU parallel (optional — short scripts do not need it).


<a id="orgb0de687"></a>

## Script template

    #!/usr/bin/env python3
    """
    Docstring
    """
    
    import argparse
    import sys
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
    
    
    def main() -> int:
        """
        Orchestrate the script.
    
        Good practice:
        - parse args once
        - call work functions with named values
        - return an exit code (0 success; nonzero failure)
        """
        args = parse_args()
    
        # Placeholder for the real work. Keep heavy logic out of parse_args.
        # Example:
        # run_work(inputs_dir=args.inputs_dir, output_tsv=args.output_tsv)
        return 0  # placeholder success; real failures surface via raised exceptions / explicit return 1
    
    
    if __name__ == "__main__":
        # Entry point:
        # - ensures the script does not run when imported as a module
        # - propagates main()'s integer return value as the process exit code
        raise SystemExit(main())


<a id="org1640505"></a>

## Functions

-   `lower_snake_case` names
-   Type hints: `main()` returns `-> int`; `parse_args()` needs no return or parameter hints (Namespace implied); work/helper functions should carry hints where types are non-obvious. Full annotation is encouraged, never required; trivial one-line wrappers may be left unhinted.
-   Generator functions (any `yield`): hint the return type with `Iterator[T]` (from `collections.abc`), e.g. `def iter_chromosomes(path: str) -> Iterator[tuple[str, bytes]]:`. The type-hint rule applies to generators — annotate the yielded element type.
-   Single-purpose, explicit parameters over globals
-   Reusable (library-style) functions — any function imported by another module — parameterize inputs, return outputs, never print, and raise specific exceptions; only `main()` converts exceptions to exit codes and emits diagnostics. Script-local work functions called only from `main()` may write progress/diagnostics to stderr (see Error policy).


<a id="orgf7f45c7"></a>

## References

-   [Google Python Style Guide](https://google.github.io/styleguide/pyguide.html)


<a id="org6a667a8"></a>

# R


<a id="org1fbea59"></a>

## Indentation

-   2 spaces, no tabs


<a id="orgae535d0"></a>

## Comments

-   Wrap comments at 80 columns when practical; allow exceptions for unbreakable tokens (paths, URLs)
-   Comment types  
    -   Sectioning comments: same banner style as bash/Python (`# =====` for major, `# ----` for minor)
    -   Roxygen docstrings: `#'` lines as the first form before a function (`#' @param`, `#' @return`, `#' @examples`)
    -   Block comments: multi-line explanations sectioned off with a banner; not indented
    -   Standalone comments: single `#` line preceding code
    -   Inline comments: on the same line as code, placed after two spaces; rationale not restatement


<a id="org1dac12f"></a>

## Comment template

    # =============================================================================
    # SECTION: CONFIGURATION
    # =============================================================================
    
    # Default chromosome set; restrict to autosomes for fragmentomics.
    default_chroms <- paste0("chr", 1:22)
    
    # -----------------------------------------------------------------------------
    # Subsection: Argument parsing
    # -----------------------------------------------------------------------------
    
    #' Parse command-line options via optparse.
    #'
    #' @return Named list of parsed options.
    #' @examples
    #' opts <- parse_options()
    parse_options <- function() {
      option_list <- list(
        make_option(c("-i", "--input"), type = "character", help = "Input file"),
        make_option(c("-o", "--output"), type = "character", help = "Output file")
      )
      parse_args(OptionParser(option_list = option_list))
    }
    
    # -----------------------------------------------------------------------------
    # Block comment: input validation rationale
    #
    # Validate inputs early so downstream functions can assume invariants.
    # This keeps work functions simple and avoids duplicated checks.
    # Fail fast with a targeted message if a required input is missing.
    # -----------------------------------------------------------------------------
    
    if (!file.exists(opts$input)) {
      stop("Input file not found: ", opts$input)
    }
    
    n_threads <- 8  # keep low for interactive nodes


<a id="org2979d55"></a>

## Cookbook / notebook conventions

-   Heavy commenting is expected — narrate the analytical reasoning
-   Section headers between logical phases (data loading, filtering, transformation, visualization, output)
-   Explain *why* a filter threshold or parameter was chosen, not just what the code does
-   When a step produces an intermediate result, briefly describe what the result looks like (rows, columns, expected values)


<a id="org9ade73b"></a>

## Cookbook section header template

    # =============================================================================
    # Data loading
    # =============================================================================
    
    metadata <- read_xlsx("metadata.xlsx")
    
    # =============================================================================
    # Filtering
    # =============================================================================
    
    # Drop samples missing both treatment and outcome (n=3 expected).
    filtered <- metadata %>%
      filter(!is.na(treatment), !is.na(outcome))
    
    # =============================================================================
    # Visualization
    # =============================================================================
    
    ggplot(filtered, aes(x = treatment, y = outcome)) +
      geom_boxplot()


<a id="org9561225"></a>

## Scripts

-   Follow tidyverse style (dplyr, ggplot2, readxl); `<-` for assignment
-   `variable_names_with_underscore`
-   Spaces before and after `=`, `+`, `-`, `<-`, etc.; after comma
-   Overall structure: packages → option parsing → work functions → `main()` → entry guard
-   Shebang: `#!/usr/bin/env Rscript`
-   Top block comment describing purpose, inputs, outputs, assumptions
-   Use `optparse` for CLI parsing in a wrapper function (e.g., `parse_options()`) to avoid shadowing optparse's own `parse_args`
-   `main()`: orchestrate; call `parse_options()` then work functions; return an integer-like exit code (`invisible(0)` on success)
-   Work functions: consume named values, return objects rather than mutate globals; avoid `print()` / `cat()` side effects in reusable code
-   Entry guard: wrap the `main()` call in `if (!interactive()) { ... }` so the file can be sourced for interactive testing without re-running
-   Error policy: `stop()` for fatal errors, `warning()` for non-fatal; let `main()` convert exceptions to exit codes
-   For Snakemake `script:` files: use `snakemake@input`, `snakemake@output`, `snakemake@log`; no `parse_options`, no entry guard
-   Save R-native outputs as `.rds` via `saveRDS()`; use `readr::write_tsv()` for portable cross-tool output


<a id="org3205e2e"></a>

## Script template

    #!/usr/bin/env Rscript
    
    # -----------------------------------------------------------------------------
    # Script name
    # Purpose, inputs, outputs, assumptions, etc.
    # -----------------------------------------------------------------------------
    
    # =============================================================================
    # SECTION: PACKAGES
    # =============================================================================
    
    packages <- c("optparse", "tidyverse")
    suppressPackageStartupMessages(
      invisible(lapply(packages, require, character.only = TRUE))
    )
    
    # =============================================================================
    # SECTION: ARGUMENT PARSING
    # =============================================================================
    
    parse_options <- function() {
      option_list <- list(
        make_option(c("-i", "--input"), type = "character",
                    help = "Input file path"),
        make_option(c("-o", "--output"), type = "character",
                    help = "Output file path")
      )
      parse_args(OptionParser(option_list = option_list))
    }
    
    # =============================================================================
    # SECTION: WORK FUNCTIONS
    # =============================================================================
    
    #' Read input data and return a tibble.
    #'
    #' @param path Path to input file.
    #' @return Tibble of input data.
    read_input <- function(path) {
      read_tsv(path, show_col_types = FALSE)
    }
    
    # =============================================================================
    # SECTION: MAIN
    # =============================================================================
    
    main <- function() {
      opts <- parse_options()
      data <- read_input(opts$input)
      saveRDS(data, opts$output)
      invisible(0)
    }
    
    # Run main() only when executed via Rscript, not when sourced interactively.
    if (!interactive()) {
      main()
    }


<a id="orgef85fc7"></a>

## Functions

-   `lower_snake_case` names; opening brace on same line: `name <- function(args) {`
-   Single-purpose, explicit parameters over globals
-   Roxygen docstring above each non-trivial function (`#' @param`, `#' @return`, `#' @examples`)
-   Reusable functions: parameterize inputs, return outputs; avoid `print()` / `cat()` side effects
-   Use `stop()` for fatal errors, `warning()` for non-fatal
-   Validate inputs early; return early on failure paths


<a id="org64204f5"></a>

## Function template

    #' Compute mean fragment length per sample.
    #'
    #' @param frag_df Tibble with columns =sample= and =length=.
    #' @return Tibble with one row per sample and column =mean_length=.
    #' @examples
    #' compute_mean_length(frag_df)
    compute_mean_length <- function(frag_df) {
      if (!all(c("sample", "length") %in% names(frag_df))) {
        stop("frag_df must contain columns 'sample' and 'length'")
      }
      frag_df %>%
        group_by(sample) %>%
        summarize(mean_length = mean(length), .groups = "drop")
    }


<a id="orgb5ef2e1"></a>

## References

-   [Tidyverse style guide](https://style.tidyverse.org/)
-   [Google R style guide](https://google.github.io/styleguide/Rguide.html)
-   [Bioconductor coding style](https://contributions.bioconductor.org/r-code.html)


<a id="org7ee706e"></a>

# Emacs Lisp


<a id="orgbf46073"></a>

## Comment format

-   `;` — inline (end of line), rare
-   `;;` — standalone comment preceding code (most common)
-   `;;;` — section header
-   `;;;;` — top-level file section header


<a id="org145806b"></a>

## Verbosity by context

-   **Config functions** (tangled from emacs.org/private.org): one-liner `;;` above the function. The org prose is the documentation; the tangled file just needs enough to grep.
-   **Infrastructure defuns** (called from many places, hooks, capture templates): full docstring as first form in the function body, plus `;;` for non-obvious logic.
-   **Use-package blocks**: generally no comments needed; the package name and `:config` are self-documenting.


<a id="org854aeb9"></a>

## Template

    ;;;; Navigation utilities
    
    ;;; Filter helm results to exclude structural headings
    (defun jg/helm-filter-example (candidate)
      "Return non-nil if CANDIDATE should be shown in helm."
      (not (string-match-p ":nohelm:" candidate)))  ; tag-based exclusion


<a id="org120878c"></a>

# Snakemake

Style conventions for Snakemake workflow files. For workflow architecture (wrapper vs module types, experiment grouping), see the full [Snakemake Style Guide and Good Practices](basecamp.md).  


<a id="orgca23fc3"></a>

## Comments

-   `#` comments follow Python conventions
-   Rule-level: brief docstring comment immediately after `rule name:` explaining purpose and key I/O
-   Workflow-level: section banners between logical groups of rules
-   Do not comment individual `input:` / `output:` / `params:` lines unless non-obvious


<a id="org89c530a"></a>

## Path and variable conventions

-   Hungarian notation prefixes: `D_` for data directories (absolute paths from config), `R_` for repository locations, `CONDA_` for conda environment YAML files
-   Build paths using Python f-strings with explicit structure: `f"{D_EMSEQ}/align/{{sample}}.bam"`
-   Escape wildcards in f-strings with double braces
-   Paths should be declarative and reflect intended output organization
-   Avoid `os.path.join` unless necessary for cross-platform compatibility
-   Avoid encoding workflow logic or conditionals inside path expressions


<a id="org6e308fd"></a>

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


<a id="org3bfb0af"></a>

## Shell blocks and scripts

-   Prefer external scripts over inline shell; scripts should be runnable outside Snakemake with explicit arguments
-   Keep log and workflow logic in the Snakefile, external to the script
-   Echo an execution-specific message: `echo "[tool] $(date) lib={wildcards.library_id} threads={threads}"`
-   Redirect stdout/stderr explicitly (e.g., `exec > "{log}" 2>&1`)
-   Avoid embedding complex logic in `run` or `shell` blocks


<a id="org046aaf8"></a>

## Configuration

-   Prefer structured (nested) YAML over flat keys: `config["ref"]["index"]` not `config["ref_index"]`
-   Two-layer config: base (committed to repo) + override (runtime via `--configfile`)
-   Use booleans for conditional execution; express conditional branches in `rule all`
-   Use tabular configuration (TSV/CSV) for sample/unit metadata; load once in the preamble


<a id="orga2f2f71"></a>

## Rule template

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


<a id="orgc2d7d82"></a>

## References

-   [Snakemake documentation](https://snakemake.readthedocs.io/en/stable/)
-   [Python style guide](#org7c6da57) (comments in .smk files follow Python conventions)


<a id="org62e6ea5"></a>

# YAML / Config

-   Minimal commenting — the structure is self-documenting
-   Comment version pins: `# pinned for compat with X`
-   Comment workarounds: `# workaround for bug in Y`
-   Ansible tasks: the `name:` field IS the comment; add `#` only for non-obvious `when:` conditions

