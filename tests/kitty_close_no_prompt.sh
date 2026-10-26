#!/usr/bin/env bash
set -u
title="kitty-close-test-$$"
kitty --title "$title" "$HOME/.local/bin/yazi" /tmp &
kitty_pid=$!
sleep 3
i3-msg "[title=\"$title\"] kill" >/dev/null
sleep 2
# kitty runs the prompt as a `kitten ask` child of its own process.
if pgrep -P "$kitty_pid" -f "kitten ask" >/dev/null; then
    echo "FAIL: kitty asked to confirm closing the window"
    kill "$kitty_pid"
    exit 1
fi
if kill -0 "$kitty_pid" 2>/dev/null; then
    echo "FAIL: kitty window still open"
    kill "$kitty_pid"
    exit 1
fi
echo "PASS: window closed without a prompt"
