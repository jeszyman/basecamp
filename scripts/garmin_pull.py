"""Garmin Connect pull. Single-process two-phase MFA via file drop.

Flow when no cached token:
  1. Submit credentials with return_on_mfa=True. Garmin emails 6-digit code.
  2. Script polls ~/.garmin_mfa_code every 5s for up to 10 min.
  3. Caller writes code to that file: `echo 123456 > ~/.garmin_mfa_code`
  4. Script reads + deletes the file, calls resume_login on the same client
     (preserves in-memory session state — pickle does not work for this).
  5. On success, dumps token to ~/.garminconnect/ for subsequent non-interactive runs.

Flow when cached token exists:
  Logs in via token, runs pull, exits.

Run in background so the caller can write the code file while the script polls.
"""
import os
import sys
import time
from datetime import date, timedelta
from pathlib import Path

from garminconnect import Garmin

TOKENSTORE = Path.home() / ".garminconnect"
CODE_FILE = Path.home() / ".garmin_mfa_code"
WAIT_SECS = 600
POLL_SECS = 5


def make_client():
    return Garmin(
        email=os.environ["GARMIN_EMAIL"],
        password=os.environ["GARMIN_PASSWORD"],
        is_cn=False,
        return_on_mfa=True,
    )


def try_cached_login(client) -> bool:
    if not TOKENSTORE.exists():
        return False
    try:
        client.login(str(TOKENSTORE))
        return True
    except Exception as e:
        print(f"cached login did not succeed ({type(e).__name__}: {e})", flush=True)
        return False


def wait_for_code() -> str:
    if CODE_FILE.exists():
        CODE_FILE.unlink()
    print(f"WAITING FOR MFA CODE — write to {CODE_FILE} (timeout {WAIT_SECS}s)", flush=True)
    waited = 0
    while waited < WAIT_SECS:
        if CODE_FILE.exists():
            code = CODE_FILE.read_text().strip()
            CODE_FILE.unlink()
            if code:
                print(f"got MFA code (len={len(code)})", flush=True)
                return code
        time.sleep(POLL_SECS)
        waited += POLL_SECS
    raise TimeoutError(f"no MFA code written to {CODE_FILE} within {WAIT_SECS}s")


def do_mfa_login(client):
    print("submitting credentials, expecting MFA challenge", flush=True)
    try:
        result = client.login()
    except Exception as e:
        msg = str(e).lower()
        if "429" in msg or "rate" in msg:
            print("\n*** RATE LIMITED (429) — bailing without further attempts ***", flush=True)
            sys.exit(2)
        raise

    if not (isinstance(result, tuple) and len(result) == 2):
        print(f"unexpected login result: {result!r}", flush=True)
        return
    status, client_state = result
    if status != "needs_mfa":
        print(f"login returned status {status!r}, no MFA needed", flush=True)
        return

    print("MFA challenge sent — check email for 6-digit code", flush=True)
    code = wait_for_code()
    print("submitting code via resume_login", flush=True)
    client.resume_login(client_state, code)
    TOKENSTORE.mkdir(exist_ok=True)
    client.client.dump(str(TOKENSTORE))
    print(f"login successful, token cached to {TOKENSTORE}", flush=True)


def run_pull(client):
    end = date.today()
    start = end - timedelta(days=7)
    print("\n=== probe pull (last 7 days) ===", flush=True)
    print("user:", client.get_full_name(), flush=True)
    print("today RHR:", client.get_rhr_day(end.isoformat()), flush=True)
    acts = client.get_activities_by_date(start.isoformat(), end.isoformat())
    print(f"activities last 7d: {len(acts)}", flush=True)
    for a in acts[:5]:
        kind = a.get("activityType", {}).get("typeKey")
        print(f"  - {a.get('startTimeLocal')} {kind} {a.get('activityName')}", flush=True)


def main():
    client = make_client()
    if try_cached_login(client):
        print("logged in via cached token", flush=True)
    else:
        do_mfa_login(client)
    run_pull(client)


if __name__ == "__main__":
    main()
