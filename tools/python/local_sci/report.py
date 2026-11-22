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
    save_plot(fig, base, caption="Median per setting with its bootstrap 95% interval; dashed line, no-change value.")
    plt.close(fig)


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
