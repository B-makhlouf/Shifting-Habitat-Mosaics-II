"""
IP Sensitivity Analysis
=======================
Two figures exploring intrinsic potential (IP) across stream orders
and its relationship with watershed slope (WtrshdSlp) for the
Yukon (YK) and Kuskokwim (KK) watersheds.

Figure 1 – Ridgeline/density plots of IP by stream order (2×2:
             rows = Juvi_IP / Spawner_IP, cols = YK / KK)
Figure 2 – Scatter plots of IP vs. WtrshdSlp (2×2 same layout)

Only reaches with IP > 0 are included (non-habitat reaches excluded).
"""

import geopandas as gpd
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import matplotlib.patches as mpatches
import numpy as np
from scipy.stats import gaussian_kde
from pathlib import Path

# ── Paths ──────────────────────────────────────────────────────────────────────
DATA_DIR = Path(
    r"C:\Users\makhl\Research Repos\Shifting-Habitat-Mosaics-II"
    r"\Data\Spatial Data\AnalysisShapefiles"
)
OUT_DIR = Path(
    r"C:\Users\makhl\Research Repos\Shifting-Habitat-Mosaics-II"
    r"\Figures"
)
OUT_DIR.mkdir(parents=True, exist_ok=True)

# ── Load data ──────────────────────────────────────────────────────────────────
yk_raw = gpd.read_file(DATA_DIR / "YkIPall.shp")
kk_raw = gpd.read_file(DATA_DIR / "KkIPall.shp")

# Normalise stream order to integer for consistent sorting/labelling
yk_raw["Str_Order"] = yk_raw["Str_Order"].astype(float).astype(int)
kk_raw["Str_Order"] = kk_raw["Str_Order"].astype(float).astype(int)

# Keep only reaches with positive IP values
yk = yk_raw[yk_raw["Juvi_IP"] > 0].copy()
kk = kk_raw[kk_raw["Juvi_IP"] > 0].copy()

# ── Colour scheme ──────────────────────────────────────────────────────────────
YK_CMAP = plt.cm.YlOrRd      # warm palette for Yukon
KK_CMAP = plt.cm.YlGnBu      # cool palette for Kuskokwim

YK_ACCENT = "#C0392B"         # deep red  — used in scatter
KK_ACCENT = "#1A6B8A"         # teal blue — used in scatter

FONT = "DejaVu Sans"
plt.rcParams.update({
    "font.family": FONT,
    "axes.spines.top": False,
    "axes.spines.right": False,
})

IP_COLS = [("Juvi_IP", "Juvenile IP"), ("Spawner_IP", "Spawner IP")]
DATASETS = [
    ("YK", yk, YK_CMAP, YK_ACCENT),
    ("KK", kk, KK_CMAP, KK_ACCENT),
]


# ══════════════════════════════════════════════════════════════════════════════
#  FIGURE 1 — Ridgeline / density plots of IP by stream order
# ══════════════════════════════════════════════════════════════════════════════

def ridgeline_panel(ax, df, ip_col, cmap, label_order=True):
    """
    Draw overlapping KDE density curves, one per stream order, offset
    vertically so higher-order streams sit higher on the y-axis.
    """
    orders = sorted(df["Str_Order"].unique())
    n = len(orders)
    x_min, x_max = 0.55, 1.02
    x_grid = np.linspace(x_min, x_max, 400)

    # Vertical spacing: each curve is offset by `step`
    step = 1.2
    # Colour gradient across stream orders
    colours = [cmap(0.25 + 0.65 * i / max(n - 1, 1)) for i in range(n)]

    yticks, yticklabels = [], []

    for i, order in enumerate(orders):
        vals = df.loc[df["Str_Order"] == order, ip_col].dropna().values
        if len(vals) < 5:
            continue

        kde = gaussian_kde(vals, bw_method="silverman")
        density = kde(x_grid)
        # Scale density so the tallest peak reaches ~0.9 * step
        peak = density.max()
        if peak > 0:
            density = density / peak * (0.9 * step)

        base_y = i * step
        ax.fill_between(
            x_grid,
            base_y,
            base_y + density,
            alpha=0.75,
            color=colours[i],
            zorder=n - i,
        )
        ax.plot(
            x_grid,
            base_y + density,
            color=colours[i],
            linewidth=1.2,
            zorder=n - i,
        )
        # Horizontal baseline
        ax.axhline(base_y, color="white", linewidth=0.6, zorder=n - i + 1)

        yticks.append(base_y)
        yticklabels.append(f"Order {order}")

    ax.set_yticks(yticks)
    ax.set_yticklabels(yticklabels, fontsize=8.5)
    ax.set_xlim(x_min, x_max)
    ax.set_ylim(-0.3, (n - 1) * step + step)
    ax.tick_params(axis="x", labelsize=8.5)
    ax.set_xlabel("IP value", fontsize=9)
    ax.spines["left"].set_visible(False)
    ax.spines["bottom"].set_color("#555555")
    ax.tick_params(left=False)


fig1, axes1 = plt.subplots(
    2, 2, figsize=(13, 11),
    gridspec_kw={"hspace": 0.45, "wspace": 0.35}
)
fig1.suptitle(
    "Distribution of Intrinsic Potential by Stream Order",
    fontsize=14, fontweight="bold", y=0.98
)

for row, (ip_col, ip_label) in enumerate(IP_COLS):
    for col, (ws_name, df, cmap, accent) in enumerate(DATASETS):
        ax = axes1[row, col]
        ridgeline_panel(ax, df, ip_col, cmap)
        ax.set_title(
            f"{ws_name} — {ip_label}",
            fontsize=10, fontweight="semibold", pad=8
        )
        if col == 0:
            ax.set_ylabel("Stream order", fontsize=9, labelpad=4)

fig1.savefig(
    OUT_DIR / "IP_StreamOrder_Ridgeline.png",
    dpi=200, bbox_inches="tight", facecolor="white"
)
print("Saved: IP_StreamOrder_Ridgeline.png")


# ══════════════════════════════════════════════════════════════════════════════
#  FIGURE 2 — Scatter: IP vs. WtrshdSlp
# ══════════════════════════════════════════════════════════════════════════════

def scatter_panel(ax, df, ip_col, cmap, accent, ws_name):
    """
    Scatter of IP (y) vs. WtrshdSlp (x), points coloured by stream order.
    Includes a colourbar for stream order.
    """
    orders = sorted(df["Str_Order"].unique())
    n = len(orders)
    norm = mcolors.Normalize(vmin=min(orders), vmax=max(orders))
    sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
    sm.set_array([])

    # Plot per stream order so legend is clean
    for order in orders:
        sub = df[df["Str_Order"] == order]
        colour = cmap(norm(order))
        ax.scatter(
            sub["WtrshdSlp"],
            sub[ip_col],
            c=[colour],
            s=6,
            alpha=0.55,
            linewidths=0,
            label=f"Order {order}",
            zorder=2,
        )

    ax.set_xlabel("Watershed slope (%)", fontsize=9)
    ax.set_ylabel("IP value", fontsize=9)
    ax.tick_params(labelsize=8.5)

    # Colourbar
    cbar = plt.colorbar(sm, ax=ax, pad=0.02, shrink=0.75)
    cbar.set_label("Stream order", fontsize=8)
    cbar.set_ticks(orders)
    cbar.ax.tick_params(labelsize=7.5)

    # Annotation: n
    n_pts = len(df)
    ax.text(
        0.97, 0.03,
        f"n = {n_pts:,}",
        transform=ax.transAxes,
        fontsize=8, ha="right", va="bottom", color="#555555"
    )


fig2, axes2 = plt.subplots(
    2, 2, figsize=(13, 10),
    gridspec_kw={"hspace": 0.45, "wspace": 0.45}
)
fig2.suptitle(
    "Intrinsic Potential vs. Watershed Slope",
    fontsize=14, fontweight="bold", y=0.98
)

for row, (ip_col, ip_label) in enumerate(IP_COLS):
    for col, (ws_name, df, cmap, accent) in enumerate(DATASETS):
        ax = axes2[row, col]
        scatter_panel(ax, df, ip_col, cmap, accent, ws_name)
        ax.set_title(
            f"{ws_name} — {ip_label}",
            fontsize=10, fontweight="semibold", pad=8
        )

fig2.savefig(
    OUT_DIR / "IP_WtrshdSlp_Scatter.png",
    dpi=200, bbox_inches="tight", facecolor="white"
)
print("Saved: IP_WtrshdSlp_Scatter.png")

plt.show()
print("\nDone.")
