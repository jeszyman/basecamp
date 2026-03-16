---
name: git-hygiene
description: >
  Interactive git hygiene scanner. Runs the git-hygiene detection script and agent,
  then presents findings with approve/skip gates for auto-fixes. Invoke with
  "git hygiene", "check repos", "scan repos", or similar phrases.
allowed-tools:
  - Agent
  - Bash
  - Read
  - AskUserQuestion
  - TaskCreate
  - TaskUpdate
---

**Installation:** Copy this file to `~/.claude/skills/git-hygiene/SKILL.md`

## Purpose

Interactive wrapper around the git-hygiene agent. Runs detection, presents findings
with user gates for auto-fixes, and summarizes results.

## Progress tracking

At the start, create tasks via **TaskCreate**:

1. "Run detection" (activeForm: "Running git hygiene detection")
2. "Review findings" (activeForm: "Reviewing findings")
3. "Apply fixes" (activeForm: "Applying auto-fixes")
4. "Judgment checks" (activeForm: "Running judgment-layer checks")
5. "Summary" (activeForm: "Generating summary")

Use **TaskUpdate** to set each task to `in_progress` when starting and `completed` when done.

## Flow

### Step 1: Determine scope

Use **AskUserQuestion** to ask the user what to scan:

```
question: "What should I scan?"
header: "Git Hygiene"
options:
  - label: "Current repo"
    description: "Scan only the current repository"
  - label: "All repos"
    description: "Scan all repositories in ~/repos/"
  - label: "Custom path"
    description: "I'll specify a path"
```

If "Current repo": use `--single .`
If "All repos": use `~/repos/`
If "Custom path": ask user for path, then use it.

### Step 2: Run detection script

```bash
bash ~/repos/basecamp/scripts/git-hygiene.sh <PATH> [--single]
```

Parse the TSV output. Mark task 1 complete.

### Step 3: Present findings

Group findings by repo and severity. Present a summary via **AskUserQuestion**:

If there are auto-fixable issues (gitignore-missing, gitignore-stale, precommit-missing):

```
question: "Found N auto-fixable issues across M repos. How should I proceed?"
header: "Auto-fix gate"
options:
  - label: "Apply all"
    description: "Apply all safe auto-fixes (gitignore sync, pre-commit hook install)"
  - label: "Review each"
    description: "Show each fix and let me approve individually"
  - label: "Skip fixes"
    description: "Skip auto-fixes, show report only"
```

If "Apply all": dispatch the git-hygiene agent to fix all issues.
If "Review each": present each fix individually with approve/skip options.
If "Skip fixes": proceed to report.

Mark task 2 complete.

### Step 4: Apply fixes (if approved)

If "Apply all" was selected, invoke the agent:

```
Agent: git-hygiene
Prompt: "Apply auto-fixes for the following findings: <TSV_FINDINGS>"
```

If "Review each" was selected, for each auto-fixable finding:

```
question: "Fix: <CHECK_ID> in <REPO> — <DETAIL>"
header: "Fix review"
options:
  - label: "Apply"
    description: "Apply this fix"
  - label: "Skip"
    description: "Skip this fix"
```

Mark task 3 complete.

### Step 5: Judgment-layer checks

Run the agent for judgment-layer checks (visibility, sensitive files, large file triage):

```
Agent: git-hygiene
Prompt: "Run judgment-layer checks for repos at <PATH>. Skip auto-fixes — they have been handled."
```

Present any critical findings (sensitive files in public repos) via AskUserQuestion.

Mark task 4 complete.

### Step 6: Memory update gate

If the agent produced memory notes, present them:

```
question: "The agent wants to write these notes to project memory. Allow?"
header: "Memory update"
options:
  - label: "Write notes"
    description: "Save hygiene scan notes to project memory"
  - label: "Skip"
    description: "Don't update memory"
```

### Step 7: Summary

Present final summary:

```
=== Git Hygiene Summary ===
Repos scanned: N
Errors found: N
Warnings found: N
Auto-fixes applied: N
Items requiring attention: N
```

Mark task 5 complete.

Present a closing gate:

```
question: "Git hygiene scan complete. Anything else?"
header: "Done"
options:
  - label: "Re-scan"
    description: "Run the scan again"
  - label: "Done"
    description: "All done"
```
