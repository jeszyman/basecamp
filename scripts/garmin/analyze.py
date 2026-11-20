#!/usr/bin/env python3
"""Analyze pulled Garmin data to inform HR zone settings."""
import json
from pathlib import Path
from collections import Counter
from datetime import datetime

D = Path(__file__).resolve().parents[2] / "data" / "garmin" / "2026-05-10"

profile = json.loads((D / "profile.json").read_text())
training = json.loads((D / "training_status.json").read_text())
hr_today = json.loads((D / "hr_zones_today.json").read_text())
acts = json.loads((D / "activities_90d.json").read_text())
zones = json.loads((D / "hr_zones_recent_activities.json").read_text())

print("=" * 70)
print("USER PROFILE / METRICS")
print("=" * 70)

# Pull key fields
def show(d, keys, prefix=""):
    for k in keys:
        v = d.get(k) if isinstance(d, dict) else None
        if v is not None:
            print(f"  {prefix}{k}: {v}")

# Profile
print("\n[profile.json]")
show(profile, ["userName", "displayName", "fullName", "weight", "height", "vo2MaxRunning",
               "vo2MaxCycling", "lactateThresholdHeartRate", "lactateThresholdSpeed",
               "userMetric", "trainingPaces"])

# heart rate zones from today's daily summary
print("\n[hr_zones_today.json] keys:", list(hr_today.keys()) if isinstance(hr_today, dict) else "list")
if isinstance(hr_today, dict):
    show(hr_today, ["maxHeartRate", "restingHeartRate", "minHeartRate", "lastSevenDaysAvgRestingHeartRate"])

# Training status
print("\n[training_status.json] top-level keys:", list(training.keys())[:10] if isinstance(training, dict) else "list")
if isinstance(training, dict):
    for k in ["mostRecentVO2Max", "mostRecentTrainingStatus", "mostRecentTrainingLoadBalance",
             "heatAltitudeAcclimationDTO", "metricsTrainingStatus"]:
        v = training.get(k)
        if v: print(f"  {k}: <present>")

# Activities summary
print("\n" + "=" * 70)
print("RECENT RUNNING ACTIVITIES (90 days)")
print("=" * 70)
print(f"  Total runs: {len(acts)}")

# Aggregate stats
total_dist_km = sum((a.get("distance") or 0) for a in acts) / 1000
total_dur_h = sum((a.get("duration") or 0) for a in acts) / 3600
print(f"  Total distance: {total_dist_km:.1f} km")
print(f"  Total duration: {total_dur_h:.1f} h")

# Pace and HR per activity
print(f"\n  {'Date':<12} {'Dist(km)':>9} {'AvgHR':>6} {'MaxHR':>6} {'Pace(min/km)':>13} {'Type':>20}")
for a in sorted(acts, key=lambda x: x.get("startTimeLocal", ""), reverse=True)[:25]:
    d = a.get("startTimeLocal", "")[:10]
    dist = (a.get("distance") or 0) / 1000
    avg = a.get("averageHR") or 0
    mx = a.get("maxHR") or 0
    dur = a.get("duration") or 0
    pace = (dur / 60) / dist if dist > 0 else 0
    typ = (a.get("activityType") or {}).get("typeKey", "")
    print(f"  {d:<12} {dist:>9.2f} {avg:>6.0f} {mx:>6.0f} {pace:>13.2f} {typ:>20}")

# Max HR observed across all activities
all_max_hr = [a.get("maxHR") for a in acts if a.get("maxHR")]
if all_max_hr:
    print(f"\n  Max HR observed (any run, 90d): {max(all_max_hr)} bpm")
    print(f"  Top 5 max HRs: {sorted(all_max_hr, reverse=True)[:5]}")

# HR zone time distribution from recent activities
print("\n" + "=" * 70)
print("HR ZONE TIME DISTRIBUTION (last 30 days)")
print("=" * 70)
print(f"  Activities with zone data: {len(zones)}")

if zones:
    # Inspect structure
    first = zones[0]["hr_zones"]
    print(f"  Zone data shape (first activity): {type(first).__name__}, len={len(first) if hasattr(first, '__len__') else '?'}")
    if isinstance(first, list) and first:
        print(f"  Sample entry: {first[0]}")

    # Aggregate time per zone across activities
    total_by_zone = Counter()
    zone_bounds = {}
    for z in zones:
        for entry in z["hr_zones"]:
            zn = entry.get("zoneNumber")
            secs = entry.get("secsInZone", 0) or 0
            total_by_zone[zn] += secs
            zone_bounds[zn] = (entry.get("zoneLowBoundary"), entry.get("zoneHighBoundary"))

    total = sum(total_by_zone.values())
    print(f"\n  Total time in zones: {total/3600:.1f} h\n")
    print(f"  {'Zone':<6} {'bpm range':<12} {'Hours':>7} {'%total':>8}")
    for zn in sorted(total_by_zone.keys()):
        lo, hi = zone_bounds.get(zn, (None, None))
        rng = f"{lo}-{hi}" if lo is not None and hi is not None else "?"
        h = total_by_zone[zn] / 3600
        pct = 100 * total_by_zone[zn] / total if total else 0
        print(f"  Z{zn:<5} {rng:<12} {h:>7.2f} {pct:>7.1f}%")
