"""Figure and report for one local-sci run."""
import os
import sys

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

sys.path.insert(0, os.path.expanduser("~/repos/science/python"))
from figure_schema import save_plot  # noqa: E402

plt.rcParams.update(
    {
        "font.family": "Arial",
        "font.size": 12,
        "axes.spines.top": False,
        "axes.spines.right": False,
        "pdf.fonttype": 42,
        "svg.fonttype": "none",
    }
)


def _fmt(value, spec=".3f"):
    return "" if value is None else format(value, spec)


def plot(rows, null, path):
    shown = [r for r in rows if r["median"] is not None]
    names = [r["setting"]["name"] for r in shown]
    x = list(range(len(shown)))
    medians = [r["median"] for r in shown]
    lower = [r["median"] - r["ci_low"] for r in shown]
    upper = [r["ci_high"] - r["median"] for r in shown]
    width = max(4.0, 0.55 * len(shown) + 1.5)
    fig, ax = plt.subplots(figsize=(width, 4.0))
    ax.axhline(null, linestyle="--", color="0.35", linewidth=1)
    ax.errorbar(x, medians, yerr=[lower, upper], fmt="o", color="#2166AC",
                markerfacecolor="#2166AC", markeredgecolor="black", capsize=4, linewidth=1.5)
    ax.set_xticks(x)
    ax.set_xticklabels(names, rotation=45 if len(names) > 6 else 0, ha="right" if len(names) > 6 else "center")
    ax.set_xlim(-0.6, len(shown) - 0.4)
    ax.set_xlabel("Setting")
    ax.set_ylabel("Median, 95% interval")
    fig.tight_layout()
    base = path[:-4] if path.endswith(".png") else path
    waive = {"axis_empty": "fewer than 5 settings; the margin keeps the end points off the axis edge"} if len(shown) < 5 else None
    save_plot(fig, base, caption="Median per setting with its bootstrap 95% interval; dashed line, no-change value.",
              waive=waive)
    plt.close(fig)


CLAIM_WORDS = ("significant", "confirmed", "better than", "improves", "outperform", "proves")


def _row_line(r):
    interval = "" if r["ci_low"] is None else f"{r['ci_low']:.3f} to {r['ci_high']:.3f}"
    return interval


def write_study_report(run_dir, question, measure_md, history, conf_rows, control, narrative, figure, holdout_n):
    """Report of one experimenter run. Findings come from the confirmation flags;
    the experimenter's account is printed only if it claims nothing the flags do not support."""
    confirmed = [r for r in conf_rows if r["flagged"]]
    lines = ["# Question", "", question.strip(), "", measure_md.strip(), "", "## Findings", ""]
    if confirmed:
        for r in confirmed:
            lines.append(
                f"- {r['setting']['name']} confirmed on the held-out half ({holdout_n} case samples): median "
                f"{r['median']:.3f}, 95% interval {_row_line(r)}, Holm p {r['p_holm']:.4f}."
            )
    else:
        lines.append(f"- No setting was confirmed on the held-out half ({holdout_n} case samples).")
    lines += ["", "## Confirmation on the held-out half", "",
              "| Pick | n | Median | 95% interval | Holm p | Confirmed |", "|---|---|---|---|---|---|"]
    for r in conf_rows:
        flag = "" if r["flagged"] is None else ("yes" if r["flagged"] else "no")
        lines.append(f"| {r['setting']['name']} | {r['n']} | {_fmt(r['median'])} | {_row_line(r)} | {_fmt(r['p_holm'], '.4f')} | {flag} |")
    if control["rate"] is not None:
        lines += ["", f"Negative control: with each pick's values shifted so their median equals the no-change value "
                      f"and resampled {control['n_rep']} times, {control['n_flagged']} of {control['n_tested']} were flagged "
                      f"({control['rate']:.1%})."]
    lines += ["", "## What the experimenter tried", ""]
    text = narrative.strip()
    if any(w in text.lower() for w in CLAIM_WORDS) and not confirmed:
        text = "(The experimenter's account was withheld because it stated a result the confirmation does not support.)"
    lines += [text, "", "## Discovery results (exploratory, no flags)", "",
              "| Round | Setting | n | Median | 95% interval | Reason |", "|---|---|---|---|---|---|"]
    for h in history:
        r = h["row"]
        lines.append(f"| {h['round']} | {h['setting']['name']} | {r['n']} | {_fmt(r['median'])} | {_row_line(r)} | {h['reason']} |")
    lines += ["", f"![No-change setting and the picks on the held-out half]({figure})", ""]
    path = os.path.join(run_dir, "report.md")
    with open(path, "w") as fh:
        fh.write("\n".join(lines))
    return path


def write_report(run_dir, measure_md, rows, control, figure):
    lines = [measure_md.rstrip("\n"), "", "## Results", ""]
    lines.append("| Setting | n | Dropped | Median | 95% interval | Holm p | Flagged |")
    lines.append("|---|---|---|---|---|---|---|")
    for r in rows:
        interval = "" if r["ci_low"] is None else f"{r['ci_low']:.3f} to {r['ci_high']:.3f}"
        flagged = "" if r["flagged"] is None else ("yes" if r["flagged"] else "no")
        lines.append(
            f"| {r['setting']['name']} | {r['n']} | {r['n_dropped']} | {_fmt(r['median'])} | "
            f"{interval} | {_fmt(r['p_holm'], '.4f')} | {flagged} |"
        )
    lines += [
        "",
        "The first row is the no-change setting. A setting is flagged when its Holm-adjusted p is below 0.05; "
        "a setting with fewer than 5 usable samples has no interval or flag.",
        "",
    ]
    if control["rate"] is None:
        lines.append("Negative control: no setting had enough samples to test.")
    else:
        lines.append(
            f"Negative control: with each setting's values shifted so their median equals the no-change value "
            f"and resampled {control['n_rep']} times, {control['n_flagged']} of {control['n_tested']} were flagged "
            f"({control['rate']:.1%}); a test that holds its 5% error rate flags about 5% or fewer."
        )
    lines += ["", f"![Median and interval per setting]({figure})", ""]
    path = os.path.join(run_dir, "report.md")
    with open(path, "w") as fh:
        fh.write("\n".join(lines))
    return path
