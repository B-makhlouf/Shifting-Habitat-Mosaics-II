# Upstream-reach topology (current)

`kusko_upstream_topology.csv` and `yukon_upstream_topology.csv` are the network
tables that power `Code/CollectUpstream/UpstreamReachFunctions.R`
(find / map all reaches upstream of a given reach).

| column | meaning |
|---|---|
| `reachid` | reach id, matching `reachid` in the edge shapefile |
| `down_reachid` | the reach immediately **downstream**; `-1` at the basin outlet |
| `strahler` | `Str_Order` from the shapefile |
| `n_upstream` | number of reaches upstream of this reach (precomputed sanity value) |
| `is_outlet` | `1` for the basin-outlet reach |
| `component` | connected-component id (`0` = main network) |

Edge shapefiles these are keyed to:
* Kusko: `Data/Spatial Data/AnalysisShapefiles/Kusko_edges_geomorphAdded.shp` (16,994 reaches)
* Yukon: `Data/Spatial Data/AnalysisShapefiles/Yukon_edges_geomorphAdded.shp` (20,989 reaches)

Upstream-of(R) = every reach whose downstream path passes through R.

## Why the old node tables are NOT used here

The old `*_noderelationships.csv` (`rid, fromnode, tonode`) tables in this folder
were built against an earlier edge shapefile. The current
`*_edges_geomorphAdded.shp` layers were renumbered, so the old `rid` is a
**permutation** of the new reaches — joining the old node table by `rid` (or by
`reachid`) attaches the wrong topology to the wrong line (verified: 0/500
adjacency match against the new geometry). The old tables are kept only for
provenance.

The topology above is instead rebuilt directly from the current shapefile
geometry, so it always matches the shapefile in use.

### Validation

* Kusko: one connected network; the outlet reaches all 16,993 other reaches;
  8,491 headwaters have 0 upstream; upstream-set size rises monotonically with
  stream order (order 2 ≈ 4 … order 7 ≈ 8,186, outlet = 16,993).
* Yukon: one connected network; the outlet reaches all 20,988 other reaches;
  10,495 headwaters have 0 upstream; monotonic with stream order
  (order 4 ≈ 5 … order 8 ≈ 1,372, outlet = 20,988).
* Rendered maps (`Figures/Maps/UpstreamTests/`) show contiguous upstream
  catchments draining to the seed reach.

## Regenerating

Run `Code/CollectUpstream/build_upstream_topology.R` with `BASIN <- "Kusko"` or
`"Yukon"`. Re-run whenever an edge shapefile changes.
