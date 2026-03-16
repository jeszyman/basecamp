# Claude Code Preferences for the basecamp repository

## Ansible conventions
- Public common playbook ID: `42a1c7e4-9b67-41b6-8336-83e061da7154`
- **Common tools**: simple apt-only packages go in `** Configuration files` > `apt_packages` list (tangled to `config/config.yaml`), alphabetically sorted
- **Capabilities**: packages needing config or service enablement get their own `****` heading under the public common playbook, with a descriptive function name (not package name). One yaml block per heading: apt install → configure → service enable, with inline YAML comments.
- No bare apt heading — a package only gets its own heading if it needs more than `apt install`
- Same structure/style applies to private playbooks in the org repo
