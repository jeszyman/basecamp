"""8-week Garmin pull with pre/post semaglutide split.

Uses cached token at ~/.garminconnect/. Writes raw + summary to
~/repos/basecamp/data/garmin/<date>/ for inspection.
"""
import json
import os
from datetime import date, datetime, timedelta
from pathlib import Path
from statistics import mean, median, stdev

from garminconnect import Garmin

SEMAGLUTIDE_START = date(2026, 5, 8)  # ~3 weeks before today (2026-05-29)
WINDOW_DAYS = 56  # 8 weeks
OUT_DIR = Path.home() / "repos/basecamp/data/garmin" / date.today().isoformat()


def login():
    c = Garmin(email=os.environ["GARMIN_EMAIL"], password=os.environ["GARMIN_PASSWORD"])
    c.login(str(Path.home() / ".garminconnect"))
    return c


def safe(fn, *a, default=None, **kw):
    try:
        return fn(*a, **kw)
    except Exception as e:
        return {"_error": f"{type(e).__name__}: {e}", "_default": default}


def daily_pull(c, day):
    iso = day.isoformat()
    return {
        "date": iso,
        "rhr": safe(c.get_rhr_day, iso),
        "hrv": safe(c.get_hrv_data, iso),
        "sleep": safe(c.get_sleep_data, iso),
        "body_battery": safe(c.get_body_battery, iso, iso),
        "stress": safe(c.get_stress_data, iso),
        "training_status": safe(c.get_training_status, iso),
        "training_readiness": safe(c.get_training_readiness, iso),
    }


def extract_rhr(d):
    try:
        return d["rhr"]["allMetrics"]["metricsMap"]["WELLNESS_RESTING_HEART_RATE"][0]["value"]
    except Exception:
        return None


def extract_hrv_overnight(d):
    try:
        v = d["hrv"].get("hrvSummary", {}).get("lastNightAvg")
        return v
    except Exception:
        return None


def extract_sleep_hours(d):
    try:
        secs = d["sleep"].get("dailySleepDTO", {}).get("sleepTimeSeconds")
        return secs / 3600 if secs else None
    except Exception:
        return None


def extract_body_battery_min(d):
    try:
        bb = d["body_battery"]
        if isinstance(bb, list) and bb:
            vals = bb[0].get("bodyBatteryValuesArray", [])
            mins = [v[1] for v in vals if isinstance(v, list) and len(v) >= 2 and v[1] is not None]
            return min(mins) if mins else None
    except Exception:
        return None


def extract_stress_avg(d):
    try:
        return d["stress"].get("avgStressLevel")
    except Exception:
        return None


def summarize(label, days):
    rhrs = [extract_rhr(d) for d in days]
    rhrs = [v for v in rhrs if v is not None]
    hrvs = [extract_hrv_overnight(d) for d in days]
    hrvs = [v for v in hrvs if v is not None]
    sleeps = [extract_sleep_hours(d) for d in days]
    sleeps = [v for v in sleeps if v is not None]
    bbs = [extract_body_battery_min(d) for d in days]
    bbs = [v for v in bbs if v is not None]
    stresses = [extract_stress_avg(d) for d in days]
    stresses = [v for v in stresses if v is not None]

    def stats(v):
        if not v:
            return None
        return {
            "n": len(v),
            "mean": round(mean(v), 2),
            "median": round(median(v), 2),
            "sd": round(stdev(v), 2) if len(v) > 1 else None,
            "min": round(min(v), 2),
            "max": round(max(v), 2),
        }

    return {
        "label": label,
        "n_days": len(days),
        "rhr": stats(rhrs),
        "hrv_overnight": stats(hrvs),
        "sleep_hours": stats(sleeps),
        "body_battery_min": stats(bbs),
        "stress_avg": stats(stresses),
    }


def activity_summary(c, start, end):
    acts = c.get_activities_by_date(start.isoformat(), end.isoformat())
    rows = []
    for a in acts:
        dt = a.get("startTimeLocal", "")[:10]
        rows.append({
            "date": dt,
            "type": a.get("activityType", {}).get("typeKey"),
            "name": a.get("activityName"),
            "duration_min": round(a.get("duration", 0) / 60, 1),
            "distance_km": round(a.get("distance", 0) / 1000, 2) if a.get("distance") else None,
            "avg_hr": a.get("averageHR"),
            "max_hr": a.get("maxHR"),
            "calories": a.get("calories"),
            "avg_speed_mps": a.get("averageSpeed"),
            "training_load": a.get("activityTrainingLoad"),
            "aerobic_te": a.get("aerobicTrainingEffect"),
            "anaerobic_te": a.get("anaerobicTrainingEffect"),
        })
    return rows


def main():
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    c = login()
    today = date.today()
    start = today - timedelta(days=WINDOW_DAYS - 1)

    print(f"pulling daily metrics {start} → {today} ({WINDOW_DAYS} days)")
    days = []
    for i in range(WINDOW_DAYS):
        day = start + timedelta(days=i)
        days.append(daily_pull(c, day))
        if (i + 1) % 14 == 0:
            print(f"  done through {day}")

    with open(OUT_DIR / "daily_raw.json", "w") as f:
        json.dump(days, f, default=str)

    pre = [d for d in days if date.fromisoformat(d["date"]) < SEMAGLUTIDE_START]
    post = [d for d in days if date.fromisoformat(d["date"]) >= SEMAGLUTIDE_START]

    summary = {
        "today": today.isoformat(),
        "semaglutide_start": SEMAGLUTIDE_START.isoformat(),
        "window_start": start.isoformat(),
        "pre": summarize("pre-semaglutide", pre),
        "post": summarize("post-semaglutide", post),
    }

    print("\npulling activities")
    summary["activities"] = activity_summary(c, start, today)
    print(f"  {len(summary['activities'])} activities")

    with open(OUT_DIR / "summary.json", "w") as f:
        json.dump(summary, f, indent=2, default=str)

    print(f"\nwrote {OUT_DIR}/daily_raw.json")
    print(f"wrote {OUT_DIR}/summary.json")
    print(f"\n=== PRE vs POST summary ===")
    for metric in ["rhr", "hrv_overnight", "sleep_hours", "body_battery_min", "stress_avg"]:
        pre_s = summary["pre"][metric]
        post_s = summary["post"][metric]
        if pre_s and post_s:
            pm, pom = pre_s["mean"], post_s["mean"]
            delta = round(pom - pm, 2)
            print(f"{metric:20s}  pre {pm:>7} (n={pre_s['n']})   post {pom:>7} (n={post_s['n']})   Δ {delta:+}")


if __name__ == "__main__":
    main()
