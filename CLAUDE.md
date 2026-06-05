# Claude Code Preferences for the basecamp repository

## Ansible conventions
- Public common playbook ID: `42a1c7e4-9b67-41b6-8336-83e061da7154`
- **Common tools**: simple apt-only packages go in `** Configuration files` > `apt_packages` list (tangled to `config/config.yaml`), alphabetically sorted
- **Capabilities**: packages needing config or service enablement get their own `****` heading under the public common playbook, with a descriptive function name (not package name). One yaml block per heading: apt install → configure → service enable, with inline YAML comments.
- No bare apt heading — a package only gets its own heading if it needs more than `apt install`
- Same structure/style applies to private playbooks in the org repo
- **bashrc.d drop-ins**: deploy via `ansible.builtin.copy` with inline `content:`, not symlinks to repo files. Symlinks require the source repo cloned on every target; `copy` makes the playbook self-contained and portable to any machine ansible reaches.
- **Editing tangled playbooks**: `basecamp.org` tangles to many targets; a bare `org-babel-tangle` regenerates ALL of them and surfaces org-vs-repo drift. To change one playbook/config, edit the org source and hand-sync just that tangle target (or narrow-tangle the subtree) — never bare-tangle the whole file.
- **blockinfile marker renames**: `blockinfile` keys its region on the marker string, so renaming a marker orphans the old block in the deployed file on the next run. Rename the live file's `# BEGIN/# END` markers to match in the same change.
