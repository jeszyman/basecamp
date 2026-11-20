#!/usr/bin/env python3
"""Pull recent Garmin Connect data to inform HR zone settings.

One login (token-cached), pulls:
- User profile / training stats (max HR, LTHR, VO2max)
- Last 90 days of running activities + HR zone distribution
"""
import json
import os
import sys
from datetime import date, timedelta
from pathlib import Path

from garminconnect import Garmin

OUT = Path(__file__).resolve().parents[2] / "data" / "garmin" / "2026-05-10"
OUT.mkdir(parents=True, exist_ok=True)
TOKENSTORE = Path.home() / ".garminconnect"

email = os.environ["GARMIN_EMAIL"]
password = os.environ["GARMIN_PASSWORD"]

print(f"Logging in as {email} ...", flush=True)
g = Garmin(email, password)
if (TOKENSTORE / "oauth1_token.json").exists():
    print("Using cached tokens.", flush=True)
    g.login(str(TOKENSTORE))
else:
    print("Fresh login (will cache tokens).", flush=True)
    g.login()
    TOKENSTORE.mkdir(parents=True, exist_ok=True)
    g.garth.dump(str(TOKENSTORE))

# 1. User summary / training stats
print("Pulling user summary, max HR, training status...", flush=True)
profile = g.get_user_profile()
summary = g.get_user_summary(date.today().isoformat())
training_status = g.get_training_status(date.today().isoformat())
max_metrics = g.get_max_metrics(date.today().isoformat())
hr_zones = g.get_heart_rates(date.today().isoformat())

(OUT / "profile.json").write_text(json.dumps(profile, indent=2, default=str))
(OUT / "user_summary.json").write_text(json.dumps(summary, indent=2, default=str))
(OUT / "training_status.json").write_text(json.dumps(training_status, indent=2, default=str))
(OUT / "max_metrics.json").write_text(json.dumps(max_metrics, indent=2, default=str))
(OUT / "hr_zones_today.json").write_text(json.dumps(hr_zones, indent=2, default=str))

# 2. Recent running activities (90 days)
end = date.today()
start = end - timedelta(days=90)
print(f"Pulling running activities {start} to {end}...", flush=True)
activities = g.get_activities_by_date(start.isoformat(), end.isoformat(), "running")
(OUT / "activities_90d.json").write_text(json.dumps(activities, indent=2, default=str))
print(f"  {len(activities)} running activities found.", flush=True)

# 3. Per-activity HR zone time-in-zone (last 30 days only, to limit calls)
recent_cutoff = (end - timedelta(days=30)).isoformat()
recent = [a for a in activities if a.get("startTimeLocal", "")[:10] >= recent_cutoff]
print(f"Pulling HR zone breakdown for {len(recent)} recent activities...", flush=True)
zone_data = []
for a in recent:
    aid = a["activityId"]
    try:
        hrz = g.get_activity_hr_in_timezones(aid)
        zone_data.append({"activityId": aid, "startTimeLocal": a.get("startTimeLocal"),
                          "duration_s": a.get("duration"), "avgHR": a.get("averageHR"),
                          "maxHR": a.get("maxHR"), "distance_m": a.get("distance"),
                          "hr_zones": hrz})
    except Exception as e:
        print(f"  skip {aid}: {e}", flush=True)
(OUT / "hr_zones_recent_activities.json").write_text(json.dumps(zone_data, indent=2, default=str))

print("DONE. Data in", OUT, flush=True)
