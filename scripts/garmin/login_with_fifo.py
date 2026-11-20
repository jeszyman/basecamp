#!/usr/bin/env python3
"""Garmin login that reads MFA code from a FIFO. Allows non-interactive driving."""
import os, sys
from pathlib import Path
from garminconnect import Garmin

email = os.environ["GARMIN_EMAIL"]
password = os.environ["GARMIN_PASSWORD"]
TOKENSTORE = Path.home() / ".garminconnect"
FIFO = Path("/tmp/garmin_mfa.fifo")

def read_mfa():
    print(f"[login] waiting for MFA code on {FIFO} ...", flush=True)
    with open(FIFO, "r") as f:
        code = f.read().strip()
    print(f"[login] got code, submitting...", flush=True)
    return code

print(f"[login] starting SSO for {email}", flush=True)
g = Garmin(email, password, prompt_mfa=read_mfa)
g.login()
TOKENSTORE.mkdir(parents=True, exist_ok=True)
g.garth.dump(str(TOKENSTORE))
print(f"[login] OK. tokens cached to {TOKENSTORE}", flush=True)
