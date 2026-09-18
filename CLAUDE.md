# Claude Code Preferences for the basecamp repository

## Repository model
- `basecamp.org` (~20k lines) is the literate source. Nearly everything tracked here is
  tangled FROM it: `ansible/`, `config/`, `lib/`, `tools/`, `scripts/`, `src/`, `workflow/`,
  `docker/`, `resources/`, `emacs/public_yasnippets/`, `.github/workflows/`. Edit the org
  block, never the tangled output — direct edits to generated files are overwritten on the
  next tangle.
- A bare `org-babel-tangle` regenerates ALL targets at once and surfaces org-vs-repo drift.
  To change one target, edit the org source and hand-sync just that file (or narrow-tangle
  its subtree).
- Conda env for this repo: `basecamp` (general tools / snakemake); env yaml in the repo root.
- `CLAUDE.md` and `AGENTS.md` are the same file (`AGENTS.md` is a symlink) — edit `CLAUDE.md`.

## Code style
- Canonical per-language style: the **Code Style Guide** in `basecamp.org`
  (`:claudekb:`-exported for headless reads to `~/.claude/kb/0ccb6668-d5dd-4519-8e32-d8082d74e336.md`).
  Covers Bash/Python/R/Emacs-Lisp/YAML/LaTeX/Markdown. Read it before writing code here.
- Snakemake style is separate: bioinformatics-module handbook,
  `~/.claude/kb/146f3266-f634-4728-8172-9ae8b459d0c7.md`.

## Ansible conventions
- Public common playbook ID: `42a1c7e4-9b67-41b6-8336-83e061da7154`
- **Common tools**: simple apt-only packages go in `** Configuration files` > `apt_packages` list (tangled to `config/config.yaml`), alphabetically sorted
- **Capabilities**: packages needing config or service enablement get their own `****` heading under the public common playbook, with a descriptive function name (not package name). One yaml block per heading: apt install → configure → service enable, with inline YAML comments.
- No bare apt heading — a package only gets its own heading if it needs more than `apt install`
- Same structure/style applies to private playbooks in the org repo
- **bashrc.d drop-ins**: deploy via `ansible.builtin.copy` with inline `content:`, not symlinks to repo files. Symlinks require the source repo cloned on every target; `copy` makes the playbook self-contained and portable to any machine ansible reaches.
- **blockinfile marker renames**: `blockinfile` keys its region on the marker string, so renaming a marker orphans the old block in the deployed file on the next run. Rename the live file's `# BEGIN/# END` markers to match in the same change.
