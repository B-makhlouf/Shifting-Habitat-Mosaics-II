################################################################################
# PortfolioCV.R
#
# Portfolio effect: interannual CV of salmon production at nested spatial scales
# defined by stream (Strahler) order, for the Kuskokwim and Yukon basins.
#
# QUESTION
# --------
# If you observe production at a tributary of order k (and everything upstream
# of it), how variable is total production from year to year? Walking from small
# headwater tributaries down to the whole basin, does interannual CV decline as
# production from many subpopulations is aggregated (the portfolio effect)?
#
# UNIT OF ANALYSIS
# ----------------
# A "unit" at order k is a MAXIMAL order-k tributary: the order-k reach at its
# MOUTH (its immediate downstream reach is of HIGHER order, or it is the basin
# outlet) PLUS every reach upstream of it. This matches the grouping logic in
# Code/CollectUpstream/Collect_GROUPStrOrd.R and avoids counting the many
# contiguous same-order reaches of one tributary stem as separate nested units.
#
# For each unit: annual time series = sum of assignment_individuals over all
# member reaches, per year. CV = sd / mean (sample sd), computed across years.
# Units are grouped by stream order. The whole-basin (outlet) unit is the
# largest estimable unit in each basin.
#
# WHOLE-REGION (Yukon + Kuskokwim) UNIT
# -------------------------------------
# Conceptually the largest unit is the two basins combined. It is NOT estimated
# here: a regional production value requires contemporaneous sampling in both
# basins, and the otolith records overlap in only two years (2018 & 2021), too
# few for a meaningful CV. The region is therefore shown on the figure as a
# conceptual end-point of the hierarchy, not a computed value.
#
# Production metric : assignment_individuals (absolute estimated fish)
# Years             : Kusko 2017-2022 (6); Yukon 2015/2016/2018/2021 (4)
#
# Topology tables (built by Code/CollectUpstream/build_upstream_topology.R):
#   Data/UpstreamReaches/kusko_upstream_topology.csv
#   Data/UpstreamReaches/yukon_upstream_topology.csv
#
# Outputs:
#   Outputs/PortfolioEffect/<Basin>_unit_CVs.csv     (per-unit detail)
#   Outputs/PortfolioEffect/<Basin>_CV_by_order.csv  (by-order summary)
#   Figures/PortfolioEffect/CV_by_streamorder.(png|pdf)
################################################################################

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(ggplot2)
})

out_dir <- here("Outputs", "PortfolioEffect")
fig_dir <- here("Figures", "PortfolioEffect")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

BASINS <- list(
  Kusko = list(
    topo      = here("Data", "UpstreamReaches", "kusko_upstream_topology.csv"),
    prod_dir  = here("Outputs", "ProductionData", "Kusko"),
    prod_glob = "_Kusko_Assignment_Results.csv$"
  ),
  Yukon = list(
    topo      = here("Data", "UpstreamReaches", "yukon_upstream_topology.csv"),
    prod_dir  = here("Outputs", "ProductionData", "Yukon_full"),
    prod_glob = "_Yukon_Full_Assignment_Results.csv$"
  )
)

# ---- spatial restriction ----------------------------------------------------
# To compare the SAME relative portion of each watershed, restrict the analysis
# to the parts upstream of the large tributaries of a chosen Strahler order:
# order 6 in the (smaller) Kuskokwim, order 8 in the (larger) Yukon. Only units
# nested inside one of those order-K0 sub-catchments are kept; the higher-order
# mainstem (and any small tributaries hanging directly off it) are dropped.
# Set an entry to NA to analyse the whole basin.
RESTRICT_ORDER <- c(Kusko = 6L, Yukon = 8L)

# ---- helpers ----------------------------------------------------------------

## Load all annual assignment CSVs for a basin into a wide matrix:
## rows = reaches (aligned to `reachids`), cols = years, value = individuals.
load_production <- function(prod_dir, prod_glob, reachids) {
  files <- list.files(prod_dir, pattern = prod_glob, full.names = TRUE)
  files <- files[str_detect(basename(files), "^[0-9]{4}_")]
  years <- as.integer(str_extract(basename(files), "^[0-9]{4}"))
  files <- files[order(years)]; years <- sort(years)

  P <- matrix(0, nrow = length(reachids), ncol = length(years),
              dimnames = list(NULL, as.character(years)))
  for (j in seq_along(files)) {
    d <- read_csv(files[j], show_col_types = FALSE,
                  col_select = c("reachid", "assignment_individuals"))
    d$reachid <- as.integer(round(d$reachid))
    idx <- match(d$reachid, reachids)
    keep <- !is.na(idx)
    P[idx[keep], j] <- d$assignment_individuals[keep]
  }
  list(P = P, years = years)
}

## Upstream-inclusive cumulative production for EVERY reach, per year:
##   cum[R] = prod[R] + sum(cum[children of R])
## Children of R = reaches whose down_reachid == R. Processed in ascending
## n_upstream order so all children are done before their parent.
upstream_inclusive <- function(topo, P) {
  n <- nrow(topo)
  down_row <- match(topo$down_reachid, topo$reachid)      # NA at outlet (-1)
  children_by_parent <- split(seq_len(n), down_row)        # name = parent row
  cum <- matrix(0, nrow = n, ncol = ncol(P))
  for (i in order(topo$n_upstream)) {                      # ascending
    ch <- children_by_parent[[as.character(i)]]
    if (is.null(ch)) {
      cum[i, ] <- P[i, ]
    } else if (length(ch) == 1L) {
      cum[i, ] <- P[i, ] + cum[ch, ]
    } else {
      cum[i, ] <- P[i, ] + colSums(cum[ch, , drop = FALSE])
    }
  }
  cum
}

# ---- core per-basin routine -------------------------------------------------

run_basin <- function(name, cfg) {
  topo <- read_csv(cfg$topo, show_col_types = FALSE)
  topo$reachid      <- as.integer(round(topo$reachid))
  topo$down_reachid <- as.integer(round(topo$down_reachid))

  pr    <- load_production(cfg$prod_dir, cfg$prod_glob, topo$reachid)
  P     <- pr$P
  years <- pr$years
  cum   <- upstream_inclusive(topo, P)

  # tributary mouths: outlet, or downstream reach is of higher order
  down_row   <- match(topo$down_reachid, topo$reachid)
  down_order <- topo$strahler[down_row]                    # NA at outlet
  is_mouth   <- topo$is_outlet == 1 | is.na(down_row) | (down_order > topo$strahler)

  # restriction domain: reaches upstream of (and including) a chosen order-K0
  # tributary mouth. A reach is in-domain if it IS an order-K0 mouth or its
  # downstream reach is in-domain. Processing parents (more upstream reaches)
  # before children lets the flag propagate up each order-K0 sub-catchment.
  K0 <- RESTRICT_ORDER[[name]]
  if (is.na(K0)) {
    in_domain <- rep(TRUE, nrow(topo))
  } else {
    is_k0_mouth <- is_mouth & (topo$strahler == K0)
    in_domain   <- logical(nrow(topo))
    for (i in order(topo$n_upstream, decreasing = TRUE)) {  # parents first
      pr  <- down_row[i]
      pin <- if (is.na(pr)) FALSE else in_domain[pr]
      in_domain[i] <- is_k0_mouth[i] || pin
    }
  }

  # STRICT NESTING: keep a unit only if its containing unit (the next tributary
  # mouth downstream) is exactly one Strahler order higher, recursively up to the
  # order-K0 sub-basin. This drops "skip" tributaries (e.g. an order-4 draining
  # straight into the order-6 mainstem) and reproduces moving up one river
  # network: every order-k tributary kept is upstream of an order-(k+1) we keep.
  # The branching tree is preserved -- an order-6 unit still holds several nested
  # order-5 units, each holding several order-4 units, and so on.
  K0_eff <- if (is.na(K0)) max(topo$strahler) else K0
  cm <- integer(nrow(topo))                       # nearest mouth at/downstream of a reach
  for (i in order(topo$n_upstream, decreasing = TRUE)) {  # downstream first
    cm[i] <- if (is_mouth[i]) i else cm[down_row[i]]
  }
  containing <- ifelse(is.na(down_row), NA_integer_, cm[down_row])  # containing mouth row
  nested <- logical(nrow(topo))
  midx <- which(is_mouth & in_domain)
  midx <- midx[order(topo$strahler[midx], decreasing = TRUE)]       # K0 mouths first
  for (i in midx) {
    k <- topo$strahler[i]
    if (k >= K0_eff) { nested[i] <- TRUE; next }
    p <- containing[i]
    nested[i] <- !is.na(p) && topo$strahler[p] == k + 1L && nested[p]
  }

  mean_p <- rowMeans(cum)
  sd_p   <- apply(cum, 1, sd)                              # sample sd (n-1)
  cv     <- ifelse(mean_p > 0, sd_p / mean_p, NA_real_)

  prod_cols <- as.data.frame(cum)
  names(prod_cols) <- paste0("prod_", years)

  units <- tibble(
    basin              = name,
    reachid            = topo$reachid,
    stream_order       = topo$strahler,
    n_upstream_reaches = topo$n_upstream,
    n_unit_reaches     = topo$n_upstream + 1L,
    is_outlet          = topo$is_outlet,
    in_domain          = in_domain,
    nested             = nested,
    mean_production    = mean_p,
    sd_production      = sd_p,
    cv                 = cv,
    n_years            = length(years)
  ) %>%
    bind_cols(prod_cols) %>%
    filter(is_mouth) %>%
    arrange(stream_order, reachid)

  write_csv(units, file.path(out_dir, paste0(name, "_unit_CVs.csv")))

  summ <- units %>%
    filter(in_domain, nested) %>%
    group_by(stream_order) %>%
    summarise(
      n_units              = sum(!is.na(cv)),
      n_units_zero_prod    = sum(is.na(cv)),
      mean_CV              = mean(cv, na.rm = TRUE),
      median_CV            = median(cv, na.rm = TRUE),
      sd_CV                = sd(cv, na.rm = TRUE),
      mean_unit_reaches    = mean(n_unit_reaches),
      mean_unit_production = mean(mean_production),
      .groups = "drop"
    ) %>%
    mutate(basin = name, .before = 1)

  write_csv(summ, file.path(out_dir, paste0(name, "_CV_by_order.csv")))

  outlet_cv <- units$cv[units$is_outlet == 1][1]
  dom <- units %>% filter(in_domain, nested)
  K0  <- RESTRICT_ORDER[[name]]
  message(sprintf("[%s] years %s | restrict order %s | %d nested units (%d with production) | orders %s",
                  name, paste(years, collapse = ","),
                  ifelse(is.na(K0), "none", as.character(K0)),
                  nrow(dom), sum(!is.na(dom$cv)),
                  paste(sort(unique(dom$stream_order[!is.na(dom$cv)])), collapse = ",")))

  list(units = units, summ = summ, years = years, outlet_cv = outlet_cv)
}

# ---- run both basins --------------------------------------------------------

res <- lapply(names(BASINS), function(nm) run_basin(nm, BASINS[[nm]]))
names(res) <- names(BASINS)

units_all <- bind_rows(lapply(res, `[[`, "units"))
summ_all  <- bind_rows(lapply(res, `[[`, "summ"))

# ============================================================================
# FIGURE — CV vs spatial scale (stream order), both basins, region conceptual
# ============================================================================

basin_cols <- c(Kusko = "#C44536", Yukon = "#4D7298")

# cap the y-axis for readability; a handful of tiny headwater units have very
# high CV and would otherwise compress the informative range. Boxplots are
# computed on the FULL (uncapped) CVs and the axis is clipped at the top, so
# only extreme upper whiskers are cut off (their outliers are hidden because
# the jittered points already show them).
Y_CAP <- 1.25
# keep only estimable units that fall inside a restriction sub-catchment AND form
# a strict nested chain (each upstream of a unit one order higher; no order skips)
plot_pts <- units_all %>% filter(!is.na(cv), in_domain, nested)
n_above  <- sum(plot_pts$cv > Y_CAP)
plot_pts <- plot_pts %>% mutate(cv_plot = pmin(cv, Y_CAP))

any_restrict <- any(!is.na(RESTRICT_ORDER))

# per-basin facet titles: watershed name + sample-year span only
facet_labs <- setNames(
  sprintf("%s (%d-%d)", names(res),
          sapply(res, function(r) min(r$years)),
          sapply(res, function(r) max(r$years))),
  names(res))

p <- ggplot(plot_pts, aes(stream_order, group = stream_order)) +
  # distribution of unit CVs (capped for display)
  geom_jitter(aes(y = cv_plot, colour = basin),
              width = 0.18, height = 0, size = 0.7, alpha = 0.16,
              show.legend = FALSE) +
  # boxplot per stream order, on the FULL CVs (outliers hidden -> shown as pts)
  geom_boxplot(aes(y = cv, colour = basin, fill = basin),
               width = 0.55, alpha = 0.25, linewidth = 0.6,
               outlier.shape = NA, show.legend = FALSE) +
  # scales = "free" so the y-axis (and its labels) is drawn on BOTH panels;
  # coord_cartesian fixes the same visible y-range across panels
  facet_wrap(~ basin, nrow = 1, scales = "free",
             labeller = as_labeller(facet_labs)) +
  scale_colour_manual(values = basin_cols, guide = "none") +
  scale_fill_manual(values = basin_cols, guide = "none") +
  scale_x_continuous(breaks = sort(unique(plot_pts$stream_order))) +
  coord_cartesian(ylim = c(0, Y_CAP + 0.02)) +
  scale_y_continuous(breaks = seq(0, 1.25, 0.25),
                     expand = expansion(c(0, 0.02))) +
  labs(
    title = "Interannual Variability in Return Spawners by Stream Order",
    x = "Stream order",
    y = "Coefficient of Variation (CV)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "grey70"),
    axis.ticks = element_line(colour = "grey70"),
    panel.spacing = unit(2, "lines"),
    axis.title = element_text(size = 18),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(size = 14, colour = "grey20"),
    strip.text = element_text(face = "bold", size = 17),
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5,
                              margin = margin(b = 14)),
    plot.title.position = "plot"
  )

fig_stub <- if (any_restrict) "CV_by_streamorder_subbasin" else "CV_by_streamorder"
ggsave(file.path(fig_dir, paste0(fig_stub, ".png")), p,
       width = 12, height = 6.5, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, paste0(fig_stub, ".pdf")), p,
       width = 12, height = 6.5, bg = "white")

message("Figure written to ", file.path(fig_dir, paste0(fig_stub, ".png")))
print(summ_all)
