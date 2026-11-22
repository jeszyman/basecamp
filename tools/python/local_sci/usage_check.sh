#!/usr/bin/env bash
# Compares the token use of one local-sci planning call (call A: no instruction
# files, no MCP servers, no tools, local-sci system prompt) with the same request
# sent with default settings from the home folder (call B: the context a fresh
# interactive session starts with). Runs on jeff-beast. Prints per-call token
# totals and the five-hour usage percentage before and after each call.
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
PROMPT="$HERE/plan_prompt.md"
DATA="/mnt/gcs/lilli/ewing/analysis_bundle/permutation_bootstrap_jeff"
MODEL="${MODEL:-claude-sonnet-5-5}"
QUOTA="$HOME/.claude/quota-status/account.json"
OUT="${OUT:-/mnt/data/local-sci/usage-check/$(date +%Y%m%d-%H%M%S)}"
mkdir -p "$OUT"

QUESTION="In ultra-low-pass cfDNA whole-genome sequencing, does keeping only fragments within a length window give a net gain in signal-to-noise for copy-number tumor detection, given that the window both raises tumor fraction and discards reads? If so, which window, and how sure can we be? Cancers are Ewing sarcoma; the label column 'selected' takes the values cancer, healthy, and other; 'tf' is the ichorCNA tumor fraction."

schema() {
  for f in "$DATA"/*.tsv; do
    printf 'file: %s\nrows: %s\ncolumns: %s\n\n' "$(basename "$f")" \
      "$(($(wc -l < "$f") - 1))" "$(head -1 "$f" | tr '\t' ',' | cut -c1-400)"
  done
}

MSG="Question: $QUESTION

Data folder: $DATA

$(schema)"
printf '%s\n' "$MSG" > "$OUT/message.txt"

quota() { python3 -c "import json;d=json.load(open('$QUOTA'));print(d['five_hour']['pct'])" 2>/dev/null || echo NA; }

summarize() {
  python3 - "$1" <<'EOF'
import json, sys
r = json.load(open(sys.argv[1]))
u = r.get("usage", {})
keys = ["input_tokens", "cache_creation_input_tokens", "cache_read_input_tokens", "output_tokens"]
vals = {k: int(u.get(k, 0) or 0) for k in keys}
print(" ".join(f"{k}={v}" for k, v in vals.items()), "total=%d" % sum(vals.values()), "turns=%s" % r.get("num_turns"))
EOF
}

EMPTY="$(mktemp -d)"
q0=$(quota)
( cd "$EMPTY" && claude -p --model "$MODEL" --setting-sources "" --strict-mcp-config --tools "" \
    --system-prompt-file "$PROMPT" --output-format json --no-session-persistence "$MSG" ) > "$OUT/A.json"
q1=$(quota)
( cd "$HOME" && claude -p --model "$MODEL" --output-format json --no-session-persistence \
    "$(cat "$PROMPT")

$MSG" ) > "$OUT/B.json"
q2=$(quota)
rmdir "$EMPTY"

echo "A (local-sci call):      $(summarize "$OUT/A.json")  five-hour % $q0 -> $q1"
echo "B (default settings):    $(summarize "$OUT/B.json")  five-hour % $q1 -> $q2"
echo "results: $OUT"
