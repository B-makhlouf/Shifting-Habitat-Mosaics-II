#!/usr/bin/env python3
"""
Portfolio-effect analysis: interannual CV of salmon production at nested
spatial scales defined by stream (Strahler) order.

UNIT OF ANALYSIS
----------------
A "unit" at order k is a maximal order-k tributary: the order-k reach at its
MOUTH (i.e. whose immediate downstream reach is of HIGHER order, or the basin
outlet) PLUS every reach upstream of it. This matches the grouping logic in
Code/CollectUpstream/Collect_GROUPStrOrd.R and avoids treating the many
contiguous same-order reaches of one tributary stem as separate nested units.

For each unit we build an annual time series = sum of assignment_individuals
over all member reaches, per year, then compute the raw across-year
CV = sd / mean (sample sd, ddof=1). Units are grouped by stream order to test
whether mean CV declines as order (catchment scale) increases -- the portfolio
prediction.

Production metric: assignment_individuals (absolute estimated fish).
Years: Kusko 2017-2022 (6), Yukon 2015/16/18/21 (4).
"""
import os, glob, sys
import numpy as np
import pandas as pd

REPO = "/sessions/gifted-dazzling-wozniak/mnt/Shifting-Habitat-Mosaics-II"
OUT_DIR = os.path.join(REPO, "Outputs", "PortfolioEffect")
FIG_DIR = os.path.join(REPO, "Figures", "PortfolioEffect")
os.makedirs(OUT_DIR, exist_ok=True)
os.makedirs(FIG_DIR, exist_ok=True)

BASINS = {
    "Kusko": dict(
        topo=os.path.join(REPO, "Data/UpstreamReaches/kusko_upstream_topology.csv"),
        prod_glob=os.path.join(REPO, "Outputs/ProductionData/Kusko/2*_Kusko_Assignment_Results.csv"),
    ),
    "Yukon": dict(
        topo=os.path.join(REPO, "Data/UpstreamReaches/yukon_upstream_topology.csv"),
        prod_glob=os.path.join(REPO, "Outputs/ProductionData/Yukon_full/2*_Yukon_Full_Assignment_Results.csv"),
    ),
}


def load_production(prod_glob):
    """Return wide df: index=reachid, columns=years, values=assignment_individuals."""
    files = sorted(glob.glob(prod_glob))
    series = {}
    for f in files:
        yr = int(os.path.basename(f).split("_")[0])
        d = pd.read_csv(f, usecols=["reachid", "assignment_individuals"])
        d["reachid"] = d["reachid"].round().astype(int)
        series[yr] = d.set_index("reachid")["assignment_individuals"]
    wide = pd.DataFrame(series).sort_index(axis=1)
    return wide  # rows=reachid, cols=year


def upstream_inclusive_sums(topo, prod_wide):
    """
    For every reach R, cumulative upstream-inclusive production per year:
        cum[R] = prod[R] + sum(cum[c] for c flowing directly into R)
    Computed by processing reaches in ascending n_upstream order (children
    always have strictly fewer upstream reaches than their parent).
    Returns DataFrame index=reachid, cols=year.
    """
    years = list(prod_wide.columns)
    # children: down_reachid -> list of reachid
    children = {}
    for r, d in zip(topo["reachid"].values, topo["down_reachid"].values):
        children.setdefault(d, []).append(r)

    prod = {r: prod_wide.loc[r].values.astype(float) if r in prod_wide.index
            else np.zeros(len(years)) for r in topo["reachid"].values}
    cum = {}
    order = topo.sort_values("n_upstream")["reachid"].values  # ascending
    for r in order:
        acc = prod[r].copy()
        for c in children.get(r, []):
            acc = acc + cum[c]
        cum[r] = acc
    cum_df = pd.DataFrame.from_dict(cum, orient="index", columns=years)
    cum_df.index.name = "reachid"
    return cum_df.sort_index()


def cv(row):
    m = np.mean(row)
    if m <= 0:
        return np.nan
    return np.std(row, ddof=1) / m


def run_basin(name, cfg):
    topo = pd.read_csv(cfg["topo"])
    topo["reachid"] = topo["reachid"].round().astype(int)
    topo["down_reachid"] = topo["down_reachid"].round().astype(int)
    prod = load_production(cfg["prod_glob"])
    years = list(prod.columns)

    cum = upstream_inclusive_sums(topo, prod)

    # downstream order lookup -> identify tributary mouths
    order_by_reach = dict(zip(topo["reachid"], topo["strahler"]))
    down_by_reach = dict(zip(topo["reachid"], topo["down_reachid"]))
    is_outlet = dict(zip(topo["reachid"], topo["is_outlet"]))
    nup = dict(zip(topo["reachid"], topo["n_upstream"]))

    def is_mouth(r):
        if is_outlet.get(r, 0) == 1:
            return True
        dn = down_by_reach[r]
        if dn == -1:
            return True
        return order_by_reach.get(dn, 0) > order_by_reach[r]

    rows = []
    for r in topo["reachid"].values:
        if not is_mouth(r):
            continue
        ts = cum.loc[r, years].values.astype(float)
        rows.append(dict(
            reachid=r,
            stream_order=order_by_reach[r],
            n_upstream_reaches=int(nup[r]),
            n_unit_reaches=int(nup[r]) + 1,
            is_outlet=int(is_outlet.get(r, 0)),
            mean_production=np.mean(ts),
            sd_production=np.std(ts, ddof=1),
            cv=cv(ts),
            n_years=len(years),
            **{f"prod_{y}": v for y, v in zip(years, ts)},
        ))
    units = pd.DataFrame(rows).sort_values(["stream_order", "reachid"])
    units.to_csv(os.path.join(OUT_DIR, f"{name}_unit_CVs.csv"), index=False)

    # by-order summary (units with defined CV, i.e. mean production > 0)
    valid = units.dropna(subset=["cv"])
    summ = (valid.groupby("stream_order")
            .agg(n_units=("cv", "size"),
                 mean_CV=("cv", "mean"),
                 median_CV=("cv", "median"),
                 sd_CV=("cv", "std"),
                 mean_unit_reaches=("n_unit_reaches", "mean"),
                 mean_unit_production=("mean_production", "mean"))
            .reset_index())
    # tally of zero-production units dropped per order
    dropped = (units[units["cv"].isna()].groupby("stream_order").size()
               .rename("n_units_zero_prod").reset_index())
    summ = summ.merge(dropped, on="stream_order", how="left").fillna({"n_units_zero_prod": 0})
    summ["n_units_zero_prod"] = summ["n_units_zero_prod"].astype(int)
    summ.to_csv(os.path.join(OUT_DIR, f"{name}_CV_by_order.csv"), index=False)

    outlet_cv = units.loc[units["is_outlet"] == 1, "cv"]
    outlet_cv = float(outlet_cv.iloc[0]) if len(outlet_cv) else np.nan

    print(f"\n===== {name} =====")
    print(f"years: {years}")
    print(f"total tributary units (mouths): {len(units)}  | with production: {len(valid)}")
    print(f"whole-basin (outlet) CV: {outlet_cv:.4f}")
    print(summ.to_string(index=False,
          formatters={"mean_CV": "{:.4f}".format, "median_CV": "{:.4f}".format,
                      "sd_CV": "{:.4f}".format,
                      "mean_unit_reaches": "{:.0f}".format,
                      "mean_unit_production": "{:,.0f}".format}))
    return units, summ, outlet_cv, years


def main():
    results = {}
    for name, cfg in BASINS.items():
        results[name] = run_basin(name, cfg)

    # ---- figure: CV vs stream order, both basins ----
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    fig, axes = plt.subplots(1, 2, figsize=(13, 5.2), sharey=False)
    for ax, name in zip(axes, ["Kusko", "Yukon"]):
        units, summ, outlet_cv, years = results[name]
        valid = units.dropna(subset=["cv"])
        orders = sorted(valid["stream_order"].unique())
        data = [valid.loc[valid["stream_order"] == o, "cv"].values for o in orders]
        # jittered points
        for o, vals in zip(orders, data):
            x = np.random.normal(o, 0.06, size=len(vals))
            ax.scatter(x, vals, s=18, alpha=0.45, color="#2c7fb8",
                       edgecolor="none", zorder=2)
        # mean CV line
        mean_cv = [np.mean(v) for v in data]
        ax.plot(orders, mean_cv, "-o", color="#d95f0e", lw=2, zorder=3,
                label="mean unit CV")
        ax.set_title(f"{name}  (n={len(years)} yrs: {years[0]}–{years[-1]})")
        ax.set_xlabel("Stream order (Strahler)")
        ax.set_ylabel("Interannual CV of production")
        ax.set_xticks(orders)
        ax.grid(alpha=0.25)
        ax.legend(loc="upper right", fontsize=9)
    fig.suptitle("Portfolio effect: interannual CV of production by spatial scale\n"
                 "(unit = order-k tributary + full upstream catchment)",
                 fontsize=12)
    fig.tight_layout(rect=[0, 0, 1, 0.93])
    out_png = os.path.join(FIG_DIR, "CV_by_streamorder.png")
    fig.savefig(out_png, dpi=200)
    print(f"\nfigure -> {out_png}")


if __name__ == "__main__":
    main()
