################################################################################
# build_upstream_topology.R
#
# Rebuild the upstream-reach topology DIRECTLY from an edge shapefile's geometry
# and write a sidecar table keyed to that shapefile's own `reachid`.
#
# Run once per basin (set BASIN below). Output:
#   Data/UpstreamReaches/<basin>_upstream_topology.csv
#   columns: reachid, down_reachid, strahler, n_upstream, is_outlet, component
#
# Method (matches the validated Python reference used to build the shipped CSVs):
#   1. Each reach is one line; its two endpoints are the rounded first/last
#      coordinates. Reaches that share an endpoint are network neighbours.
#   2. Within each connected component, the outlet is the terminal reach (a reach
#      with a degree-1 endpoint) of highest Str_Order.
#   3. Root each component at its outlet; a reach's downstream neighbour is its
#      parent. Upstream-of(R) = descendants of R.
################################################################################

library(sf)
library(here)

# ---- CONFIG -----------------------------------------------------------------
BASIN <- "Kusko"   # "Kusko" or "Yukon"

SHP_DIR <- here("Data", "Spatial Data", "AnalysisShapefiles")
EDGES <- if (BASIN == "Kusko")
  file.path(SHP_DIR, "Kusko_edges_geomorphAdded.shp") else
  file.path(SHP_DIR, "Yukon_edges_geomorphAdded.shp")
OUT_CSV <- here("Data", "UpstreamReaches", paste0(tolower(BASIN), "_upstream_topology.csv"))

# ---- READ -------------------------------------------------------------------
edges <- st_read(EDGES, quiet = TRUE)
stopifnot(all(c("reachid", "Str_Order") %in% names(edges)))
edges$reachid <- as.integer(round(edges$reachid))

# ---- endpoints per reach ----------------------------------------------------
co  <- st_coordinates(edges)
fid <- co[, ncol(co)]                       # feature index in last column
reach_of_fid <- edges$reachid[fid]
key <- paste(round(co[, "X"]), round(co[, "Y"]), sep = "_")

firstlast <- do.call(rbind, lapply(split(seq_len(nrow(co)), fid), function(ix)
  ix[c(1, length(ix))]))
ep_reach <- reach_of_fid[as.vector(firstlast)]
ep_key   <- key[as.vector(firstlast)]

term <- tapply(ep_key, ep_reach, function(k) unique(k), simplify = FALSE)
reaches <- as.integer(names(term))
fp <- vapply(term, `[`, "", 1); lp <- vapply(term, function(z) z[min(2, length(z))], "")
names(fp) <- names(lp) <- reaches

# ---- point -> reaches, reach adjacency --------------------------------------
pt <- list()
for (i in seq_along(reaches)) {
  pt[[fp[i]]] <- c(pt[[fp[i]]], reaches[i])
  if (lp[i] != fp[i]) pt[[lp[i]]] <- c(pt[[lp[i]]], reaches[i])
}
adj <- vector("list", length(reaches)); names(adj) <- as.character(reaches)
for (p in names(pt)) {
  rs <- pt[[p]]
  if (length(rs) > 1) for (r in rs) adj[[as.character(r)]] <- c(adj[[as.character(r)]], setdiff(rs, r))
}
adj <- lapply(adj, unique)

strahler <- setNames(edges$Str_Order[match(reaches, edges$reachid)], as.character(reaches))
deg1 <- vapply(seq_along(reaches), function(i)
  length(pt[[fp[i]]]) == 1 || length(pt[[lp[i]]]) == 1, logical(1))

# ---- root each connected component at its highest-order terminal reach -------
parent <- setNames(rep(NA_integer_, length(reaches)), as.character(reaches))
comp   <- setNames(rep(NA_integer_, length(reaches)), as.character(reaches))
visited <- setNames(logical(length(reaches)), as.character(reaches))
cid <- 0L
seq_by_comp <- list()
ord_pool <- reaches[order(-ifelse(is.na(strahler[as.character(reaches)]), -1,
                                  strahler[as.character(reaches)]))]
for (start in ord_pool) {
  if (visited[as.character(start)]) next
  # collect component members
  stack <- start; members <- c(); seenc <- setNames(TRUE, as.character(start))
  while (length(stack) > 0) {
    u <- stack[1]; stack <- stack[-1]; members <- c(members, u)
    for (v in adj[[as.character(u)]]) if (is.na(seenc[as.character(v)])) {
      seenc[as.character(v)] <- TRUE; stack <- c(stack, v)
    }
  }
  tips <- members[deg1[match(members, reaches)]]
  pool <- if (length(tips)) tips else members
  root <- pool[which.max(ifelse(is.na(strahler[as.character(pool)]), -1,
                                strahler[as.character(pool)]))]
  # BFS root the component
  parent[as.character(root)] <- NA_integer_
  q <- root; visited[as.character(root)] <- TRUE; comp[as.character(root)] <- cid; seqc <- c()
  while (length(q) > 0) {
    u <- q[1]; q <- q[-1]; seqc <- c(seqc, u)
    for (v in adj[[as.character(u)]]) if (!visited[as.character(v)]) {
      visited[as.character(v)] <- TRUE; parent[as.character(v)] <- u
      comp[as.character(v)] <- cid; q <- c(q, v)
    }
  }
  seq_by_comp[[length(seq_by_comp) + 1]] <- seqc
  cid <- cid + 1L
}

# ---- descendant counts (post-order per component) ---------------------------
children <- split(as.integer(names(parent)), parent)
upcount <- setNames(integer(length(reaches)), as.character(reaches))
for (seqc in seq_by_comp) for (u in rev(seqc)) {
  ch <- children[[as.character(u)]]
  upcount[as.character(u)] <- if (is.null(ch)) 0L else sum(1L + upcount[as.character(ch)])
}

# ---- write ------------------------------------------------------------------
topo <- data.frame(
  reachid      = reaches,
  down_reachid = ifelse(is.na(parent[as.character(reaches)]), -1L, parent[as.character(reaches)]),
  strahler     = ifelse(is.na(strahler[as.character(reaches)]), -1L, as.integer(strahler[as.character(reaches)])),
  n_upstream   = as.integer(upcount[as.character(reaches)]),
  is_outlet    = as.integer(is.na(parent[as.character(reaches)])),
  component    = as.integer(comp[as.character(reaches)])
)
topo <- topo[order(topo$reachid), ]
write.csv(topo, OUT_CSV, row.names = FALSE)
cat("wrote", OUT_CSV, "(", nrow(topo), "rows ); components:", length(unique(topo$component)),
    " outlets:", sum(topo$is_outlet), " headwaters:", sum(topo$n_upstream == 0), "\n")
