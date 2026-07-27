# Parameter Explorer

A local dashboard for exploring how the six main publication figures change as you
vary the analysis parameters in `Code/Analysis/params.R`.

You drag sliders in the browser; the tool rewrites `params.R`, re-runs only the R
scripts those parameters affect, streams the R console output back live, and
redisplays the figures.

## What you need

- **R** with `Rscript` on your `PATH` and all packages the analysis uses
  (`sf`, `dplyr`, `readr`, `readxl`, `tibble`, `tidyr`, `ggplot2`, `RColorBrewer`,
  `here`, `ks`, `scales`, `cowplot`). These are the same packages the scripts
  already need.
- **Python 3** (standard library only — no `pip install` required). macOS and
  most Linux setups already have it.

## Run it

From the project root:

```bash
python3 Code/ParamExplorer/server.py
```

Then open <http://localhost:8765> in your browser.

If `Rscript` is not on your `PATH`, point the tool at it directly:

```bash
RSCRIPT=/usr/local/bin/Rscript python3 Code/ParamExplorer/server.py
```

To use a different port: `PORT=9000 python3 Code/ParamExplorer/server.py`.

## How it works

The parameters and which figures each one drives:

| Parameter (block) | Drives | Scripts re-run |
|---|---|---|
| `min_stream_order`, `min_error`, `max_error`, `sensitivity_threshold`, `channel_slope_cutoff` (Kuskokwim) | Fig 1, Fig 3, Fig 4 | `01`, `Fig1`, `03e`, `03f` |
| `min_stream_order`, `min_error`, `sensitivity_threshold`, `channel_slope_cutoff`, `porcupine_target` (Yukon) | Fig 3, Fig 4 | same as above |
| `CONTOUR_FILT_THRESH` | Fig 1, Fig 2 | `02`, `Fig1` |

The six figures shown:

1. **Fig 1** — six Kuskokwim annual maps paired with their contour panels
2. **Fig 2** — density contours (per year × basin, filtered to the current threshold)
3. **Fig 3 relative** — nested-catchment CV, Kuskokwim + Yukon
4. **Fig 3 absolute** — nested-catchment CV (absolute), Kuskokwim + Yukon
5. **Fig 4 SD(log)** — Brennan portfolio, primary metric
6. **Fig 4 pairwise** — Brennan portfolio, Brennan-matched view

**"Regenerate affected figures"** runs only what your changed parameters touch.
Because the map-driven figures (1, 3, and 4) all read the output of the full
assignment computation (`01_FullBasinRelativeProdMaps.R`), changing any map
parameter re-runs `01` once and then the dependent figure scripts — this can take
several minutes. **Fig 2** only depends on the contour threshold and is quicker.

Each figure card also has its own **run** button, and there's a **Regenerate all
six** button.

> Note: `02_ContourThreshnew.R` reads the pre-computed sensitivity sweep at
> `Outputs/SensitivitySweep/t0.9/`, so it responds to `CONTOUR_FILT_THRESH` but
> not to the Kuskokwim/Yukon `sensitivity_threshold`. If you want the contours to
> track a different sweep level, regenerate that sweep folder first.

## Safety

- The first time it modifies `params.R`, the tool saves a one-time backup to
  `params.R.explorer.bak` next to it. Delete that file if you want a fresh backup
  taken on the next run.
- Everything runs locally on `127.0.0.1`; nothing leaves your machine.
- The tool only writes numeric values back into the existing assignments in
  `params.R` — comments and formatting are preserved.

## Files

- `server.py` — the local backend (params parsing/rewriting, script runner, figure
  serving).
- `index.html` — the dashboard UI.
