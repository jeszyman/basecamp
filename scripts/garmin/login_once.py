#!/usr/bin/env python3
"""One-shot interactive Garmin login. Caches OAuth tokens to ~/.garminconnect/.

Run once in a real terminal so you can type the MFA code.
After this completes, future scripts can use cached tokens.
"""
import os
from pathlib import Path
from garminconnect import Garmin

email = os.environ["GARMIN_EMAIL"]
password = os.environ["GARMIN_PASSWORD"]
TOKENSTORE = Path.home() / ".garminconnect"

print(f"Logging in as {email}. You will be prompted for MFA.")
g = Garmin(email, password, prompt_mfa=lambda: input("MFA code: ").strip())
g.login()
TOKENSTORE.mkdir(parents=True, exist_ok=True)
g.garth.dump(str(TOKENSTORE))
print(f"OK. Tokens cached to {TOKENSTORE}.")
