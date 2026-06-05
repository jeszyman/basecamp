"""Matched-workout comparison: same-route runs pre vs post inflection.

Filters activities by name (e.g., "Olivette - Zone 2 Run") in an 8-week window
and prints HR/pace/distance/duration table split by 2026-05-08 cutoff.
"""
import os
from datetime import date, timedelta
from pathlib import Path
from statistics import mean

from garminconnect import Garmin

CUTOFF = date(2026, 5, 8)
WINDOW_DAYS = 56
ROUTE_PATTERNS = ["Olivette - Zone 2 Run", "Treadmill Negative Split"]


def login():
    c = Garmin(email=os.environ["GARMIN_EMAIL"], password=os.environ["GARMIN_PASSWORD"])
    c.login(str(Path.home() / ".garminconnect"))
    return c


def main():
    c = login()
    end = date.today()
    start = end - timedelta(days=WINDOW_DAYS - 1)
    acts = c.get_activities_by_date(start.isoformat(), end.isoformat())

    by_route = {}
    for a in acts:
        name = a.get("activityName", "")
        for pat in ROUTE_PATTERNS:
            if pat in name:
                by_route.setdefault(pat, []).append(a)
                break

    for pat, rows in by_route.items():
        rows.sort(key=lambda x: x.get("startTimeLocal", ""))
        print(f"\n=== {pat} ({len(rows)} runs) ===")
        print(f"{'date':10s}  {'bin':5s}  {'dist_km':>7}  {'dur_min':>7}  {'pace_min/km':>11}  {'avg_hr':>6}  {'max_hr':>6}  {'load':>5}")
        pre_hr, post_hr = [], []
        pre_pace, post_pace = [], []
        for a in rows:
            dt = a.get("startTimeLocal", "")[:10]
            d = date.fromisoformat(dt)
            bin_ = "pre" if d < CUTOFF else "post"
            dist_m = a.get("distance") or 0
            dur_s = a.get("duration") or 0
            dist_km = dist_m / 1000
            dur_min = dur_s / 60
            pace = (dur_min / dist_km) if dist_km else None  # min/km
            avg_hr = a.get("averageHR")
            max_hr = a.get("maxHR")
            load = a.get("activityTrainingLoad")
            pace_str = f"{pace:.2f}" if pace else "—"
            print(f"{dt:10s}  {bin_:5s}  {dist_km:7.2f}  {dur_min:7.1f}  {pace_str:>11}  {str(avg_hr):>6}  {str(max_hr):>6}  {str(round(load,1) if load else '—'):>5}")
            if bin_ == "pre" and avg_hr:
                pre_hr.append(avg_hr)
                if pace: pre_pace.append(pace)
            elif bin_ == "post" and avg_hr:
                post_hr.append(avg_hr)
                if pace: post_pace.append(pace)
        if pre_hr and post_hr:
            print(f"  avg HR    pre {mean(pre_hr):5.1f} (n={len(pre_hr)})   post {mean(post_hr):5.1f} (n={len(post_hr)})   Δ {mean(post_hr)-mean(pre_hr):+.1f}")
        if pre_pace and post_pace:
            print(f"  avg pace  pre {mean(pre_pace):5.2f} (n={len(pre_pace)})   post {mean(post_pace):5.2f} (n={len(post_pace)})   Δ {mean(post_pace)-mean(pre_pace):+.2f} min/km")


if __name__ == "__main__":
    main()
