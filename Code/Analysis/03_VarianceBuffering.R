################################################################################
# 03_VarianceBuffering.R
# ==============================================================================
# Brennan et al. (2019), Fig. 4A/B-style analysis for Yukon and Kuskokwim.
#
# Observed:
#   1. Read assignment_rescale directly: each local reach's stored proportion
#      of the annual total run. These values already sum to one and are not
#      transformed or renormalized. (Despite its name, assignment_norm is an
#      intermediate per-record quantity and does not sum to one.)
#   2. Accumulate each reach plus all upstream reaches.
#   3. For every chronological pair of return years calculate the signed,
#      symmetric percentage difference:
#          200 * (later - earlier) / (later + earlier)
#      This retains complete turn-ons (+200%) and turn-offs (-200%) and treats
#      increases and decreases symmetrically. Only zero-to-zero pairs are absent.
#   4. Plot the distribution of pairwise changes by nested-catchment stream order.
#
# Expected under independent populations:
#   Each unique reach is an independent lognormal population whose long-term mean
#   equals its proportional channel length in the basin and whose CV is set by
#   NULL_CVS. Thus, a reach containing 2% of eligible basin channel length has
#   expected production equal to 2% of basin production, and reach expectations
#   sum exactly to one.
#   Simulated reach production is accumulated through the same network, and the
#   5th/95th percentiles of pairwise change form the reference fans.
#
# Independent production is generated on the raw scale and then normalized to
# sum to one within each simulated year before network accumulation. This keeps
# the simulated response on the same compositional scale as the observed annual
# production proportions while preserving independence before closure.
#
# Uses base R only.
################################################################################

set.seed(20260717)
source(file.path("Code", "Analysis", "params.R"))
N_SIM <- 250L
NULL_CVS <- c(0.25, 1.0)
RUN_WORKBOOK <- file.path("Data", "AYKEscapement.xlsx")
# Manuscript assumption: both basins use the current 0.7 assignment threshold
# from params.R. Read the canonical production outputs made by step 01; the
# sensitivity-sweep folders may contain stale years from earlier runs.
ASSIGNMENT_SETTING <- "current"
OUTPUT_TAG <- "_t0.7"
out_dir <- file.path("Outputs", "PortfolioEffect")
fig_dir <- file.path("Figures", "03_PortfolioEffect")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

BASINS <- list(
  Kuskokwim = list(
    topology = file.path("Data", "UpstreamReaches", "kusko_upstream_topology.csv"),
    lengths = file.path("Data", "UpstreamReaches", "kusko_reach_length.csv"),
    workbook_river = "Kusko", mainstem_order = 7L,
    min_stream_order = KUSKO_PARAMS$min_stream_order,
    pdir = if (ASSIGNMENT_SETTING == "current")
      file.path("Outputs", "ProductionData", "Kusko") else
      file.path("Outputs", "SensitivitySweep", paste0("t", ASSIGNMENT_SETTING), "Kusko"),
    pattern = "^[0-9]{4}_Kusko_Assignment_Results[.]csv$", colour = "#C7472F"),
  Yukon = list(
    topology = file.path("Data", "UpstreamReaches", "yukon_upstream_topology.csv"),
    lengths = file.path("Data", "UpstreamReaches", "yukon_reach_length.csv"),
    workbook_river = "Yukon", mainstem_order = 8L,
    min_stream_order = YUKON_PARAMS$min_stream_order,
    pdir = if (ASSIGNMENT_SETTING == "current")
      file.path("Outputs", "ProductionData", "Yukon_full") else
      file.path("Outputs", "SensitivitySweep", paste0("t", ASSIGNMENT_SETTING), "Yukon"),
    pattern = "^[0-9]{4}_Yukon_Full_Assignment_Results[.]csv$", colour = "#2F6D9E")
)

accumulate <- function(mat, topo_order, downidx) {
  out <- mat
  for (i in topo_order) {
    d <- downidx[i]
    if (!is.na(d)) out[d, ] <- out[d, ] + out[i, ]
  }
  out
}

read_shares <- function(path) {
  z <- readLines(path, warn = FALSE)
  expected <- length(strsplit(z[1L], ",", fixed = TRUE)[[1L]])
  rows <- strsplit(z[-1L], ",", fixed = TRUE); nfield <- lengths(rows)
  reach <- suppressWarnings(as.numeric(vapply(rows, function(x) x[1L], "")))
  share <- rep(0, length(rows)); good <- nfield == expected
  share[good] <- suppressWarnings(as.numeric(vapply(rows[good], function(x) x[5L], "")))
  individuals <- rep(0, length(rows))
  individuals[good] <- suppressWarnings(as.numeric(vapply(rows[good], function(x) x[7L], "")))
  share[!is.finite(share) | share < 0] <- 0
  individuals[!is.finite(individuals) | individuals < 0] <- 0
  list(reachid = round(reach), share = share, individuals = individuals,
       malformed = which(!good), n_rows = length(rows))
}

# Read the simple inline-string worksheet directly with base R. Columns A, B,
# and D are Year, Total_Run, and River in the source workbook.
read_total_runs <- function(path) {
  xml <- paste(readLines(unz(path, "xl/worksheets/sheet1.xml"), warn = FALSE),
               collapse = "")
  rows <- regmatches(xml, gregexpr("<row\\b[^>]*>.*?</row>", xml, perl = TRUE))[[1L]]
  cell <- function(row, column, numeric = FALSE) {
    tag <- regmatches(row, regexpr(paste0("<c\\b[^>]*r=\"", column,
                                         "[0-9]+\"[^>]*>.*?</c>"),
                                   row, perl = TRUE))
    if (!length(tag) || is.na(tag)) return(NA)
    pattern <- if (numeric) "<v>([^<]*)</v>" else "<t[^>]*>([^<]*)</t>"
    value <- sub(paste0(".*", pattern, ".*"), "\\1", tag, perl = TRUE)
    if (numeric) as.numeric(value) else value
  }
  rows <- rows[-1L]
  out <- data.frame(
    year = vapply(rows, cell, numeric(1), column = "A", numeric = TRUE),
    total_run = vapply(rows, cell, numeric(1), column = "B", numeric = TRUE),
    river = vapply(rows, cell, character(1), column = "D", numeric = FALSE),
    stringsAsFactors = FALSE)
  if (any(!is.finite(out$year) | !is.finite(out$total_run) | out$total_run <= 0)) {
    stop("Invalid Year or Total_Run value in ", path)
  }
  out
}

run_table <- read_total_runs(RUN_WORKBOOK)
for (basin in names(BASINS)) {
  r <- run_table[run_table$river == BASINS[[basin]]$workbook_river, ]
  if (!nrow(r) || anyDuplicated(r$year)) stop("Invalid run series for ", basin)
  BASINS[[basin]]$long_term_return_cv <- stats::sd(r$total_run) / mean(r$total_run)
}

pairwise_change <- function(X, years) {
  pairs <- utils::combn(seq_along(years), 2L)
  out <- vector("list", ncol(pairs))
  for (k in seq_len(ncol(pairs))) {
    a <- pairs[1L, k]; b <- pairs[2L, k]
    val <- rep(NA_real_, nrow(X))
    denom <- X[, a] + X[, b]
    valid <- denom > 0
    val[valid] <- 200 * (X[valid, b] - X[valid, a]) / denom[valid]
    out[[k]] <- data.frame(year_earlier = years[a], year_later = years[b],
                           pct_change = val, stringsAsFactors = FALSE)
  }
  out
}

observed_rows <- null_rows <- null_length_rows <- validation_rows <- list()
tributary_cv_rows <- tributary_null_cv_rows <- list()
tributary_abs_cv_rows <- tributary_null_abs_cv_rows <- list()
tributary_abs_cv_value_rows <- list()
tributary_absfish_rows <- list()
tributary_unit_rows <- list()
sample_basin_cv_rows <- list()
all_relative_cv_rows <- list()
all_relative_cv_null_rows <- list()
pairwise_tributary_unit_rows <- list()
pairwise_tributary_null_rows <- list()
pairwise_tributary_length_null_rows <- list()

for (basin in names(BASINS)) {
  message("Brennan pairwise analysis: ", basin)
  cfg <- BASINS[[basin]]
  topo <- read.csv(cfg$topology, stringsAsFactors = FALSE)
  topo$reachid <- round(topo$reachid)
  topo$down_reachid <- suppressWarnings(round(topo$down_reachid))
  rid <- topo$reachid; stream_order <- as.integer(topo$strahler)
  downidx <- match(topo$down_reachid, rid)
  topo_order <- order(topo$n_upstream, method = "radix")
  # Network self-check.
  chk <- accumulate(matrix(1, length(rid), 1L), topo_order, downidx)[, 1L]
  stopifnot(all(abs(chk - (topo$n_upstream + 1)) < 1e-8))

  reach_lengths <- read.csv(cfg$lengths, stringsAsFactors = FALSE)
  length_m <- reach_lengths$length_m[match(rid, round(reach_lengths$reachid))]
  if (any(!is.finite(length_m) | length_m <= 0)) {
    stop(basin, ": every topology reach must have a positive finite length")
  }
  eligible_reach <- !is.na(stream_order) & stream_order >= cfg$min_stream_order
  if (!any(eligible_reach)) {
    stop(basin, ": min_stream_order excludes the entire river network")
  }
  expected_share <- ifelse(eligible_reach, length_m, 0)
  expected_share <- expected_share / sum(expected_share)
  stopifnot(abs(sum(expected_share) - 1) < 1e-12)
  upstream_length_km <- accumulate(matrix(length_m, ncol = 1L),
                                   topo_order, downidx)[, 1L] / 1000
  assigned_upstream_length_km <- accumulate(
    matrix(ifelse(eligible_reach, length_m, 0), ncol = 1L),
    topo_order, downidx
  )[, 1L] / 1000

  files <- sort(list.files(cfg$pdir, pattern = cfg$pattern, full.names = TRUE))
  years <- as.integer(substr(basename(files), 1L, 4L))
  configured_years <- ANALYSIS_YEARS[[basin]]
  keep_files <- years %in% configured_years
  files <- files[keep_files]
  years <- years[keep_files]
  missing_years <- setdiff(configured_years, years)
  if (length(missing_years)) {
    stop(basin, ": missing assignment files for configured year(s): ",
         paste(missing_years, collapse = ", "))
  }
  S <- matrix(0, length(rid), length(files), dimnames = list(rid, years))
  basin_runs <- run_table[run_table$river == cfg$workbook_river, ]
  run_idx <- match(years, basin_runs$year)
  if (anyNA(run_idx)) stop(basin, ": sampled year missing from ", RUN_WORKBOOK)
  total_runs <- basin_runs$total_run[run_idx]
  embedded_assignment_totals <- numeric(length(files))
  for (j in seq_along(files)) {
    d <- read_shares(files[j]); valid_id <- is.finite(d$reachid)
    agg <- tapply(d$share[valid_id], d$reachid[valid_id], sum)
    idx <- match(as.numeric(names(agg)), rid); ok <- !is.na(idx)
    S[idx[ok], j] <- as.numeric(agg[ok])
    embedded_assignment_totals[j] <- sum(d$individuals)
    raw_sum <- sum(S[, j])
    if (!isTRUE(all.equal(raw_sum, 1, tolerance = 1e-8))) {
      stop(
        basin, " ", years[j],
        ": stored assignment_rescale values must sum to 1; observed sum = ",
        format(raw_sum, digits = 12)
      )
    }
    validation_rows[[length(validation_rows) + 1L]] <- data.frame(
      basin = basin, year = years[j], raw_share_sum = raw_sum,
      normalization_factor = 1,
      n_malformed_rows = length(d$malformed),
      malformed_reachids = paste(d$reachid[d$malformed], collapse = ";"),
      workbook_total_run = total_runs[j],
      assignment_individuals_sum = embedded_assignment_totals[j],
      run_total_difference = embedded_assignment_totals[j] - total_runs[j],
      null_expected_share_sum = NA_real_, null_mean_basis = "",
      stringsAsFactors = FALSE)
  }

  Acc <- accumulate(S, topo_order, downidx)
  AbsAcc <- sweep(Acc, 2L, total_runs, "*")
  sample_basin_cv_rows[[length(sample_basin_cv_rows) + 1L]] <- data.frame(
    basin = basin, n_years = length(total_runs), years = paste(years, collapse = ";"),
    sampled_year_total_run_cv = stats::sd(total_runs) / mean(total_runs),
    stringsAsFactors = FALSE)
  mean_acc <- rowMeans(Acc)
  # Retain every catchment that contributes in at least one observed year.
  keep <- rowSums(Acc > 0) > 0

  # Observed temporal CV at the tributary level. A tributary is represented
  # once, at the downstream end of its maximal same-order segment: either the
  # next reach has a higher Strahler order or the reach is the basin outlet.
  # This avoids treating every nested reach along a tributary as an independent
  # observation while retaining tributaries of every order.
  observed_relative_cv <- apply(
    Acc, 1L, function(v) {
      m <- mean(v)
      if (is.finite(m) && m > 0) stats::sd(v) / m else NA_real_
    }
  )
  tributary_outlet <- is.na(downidx)
  has_downstream <- !is.na(downidx)
  tributary_outlet[has_downstream] <-
    stream_order[downidx[has_downstream]] > stream_order[has_downstream]
  all_relative_keep <- eligible_reach & keep & tributary_outlet &
    is.finite(observed_relative_cv)
  tributary_pair_keep <- eligible_reach & keep & tributary_outlet
  tributary_length_log <- log10(
    assigned_upstream_length_km[tributary_pair_keep]
  )
  tributary_length_breaks <- seq(
    min(tributary_length_log),
    max(tributary_length_log),
    length.out = 13L
  )
  tributary_pair_length_bin <- rep(NA_integer_, length(rid))
  tributary_pair_length_bin[tributary_pair_keep] <- cut(
    tributary_length_log,
    breaks = tributary_length_breaks,
    include.lowest = TRUE,
    labels = FALSE
  )
  pairwise_tributary_unit_rows[[
    length(pairwise_tributary_unit_rows) + 1L
  ]] <- data.frame(
    basin = basin,
    reachid = rid[tributary_pair_keep],
    stream_order = stream_order[tributary_pair_keep],
    upstream_length_km = upstream_length_km[tributary_pair_keep],
    assigned_upstream_length_km =
      assigned_upstream_length_km[tributary_pair_keep],
    assigned_length_bin =
      tributary_pair_length_bin[tributary_pair_keep],
    stringsAsFactors = FALSE
  )
  all_relative_cv_rows[[length(all_relative_cv_rows) + 1L]] <- data.frame(
    basin = basin,
    reachid = rid[all_relative_keep],
    stream_order = stream_order[all_relative_keep],
    observed_cv = observed_relative_cv[all_relative_keep],
    n_years = ncol(Acc),
    stringsAsFactors = FALSE
  )

  # Distinct tributary units below the basin-specific mainstem cutoff. At the
  # cutoff, use exactly one value: the basin outlet and its entire upstream
  # network. Orders above the cutoff are not treated as separate scales.
  outlets_by_order <- list()
  for (o in sort(unique(stream_order[stream_order <= cfg$mainstem_order]))) {
    outlets <- if (o == cfg$mainstem_order) {
      which(is.na(downidx) & keep)
    } else {
      which(stream_order == o & stream_order[downidx] > o & keep)
    }
    if (!length(outlets)) next
    vals <- apply(Acc[outlets, , drop = FALSE], 1L, function(v) stats::sd(v) / mean(v))
    vals <- vals[is.finite(vals)]
    if (!length(vals)) next
    qs <- stats::quantile(vals, c(.25, .5, .75), na.rm = TRUE)
    tributary_cv_rows[[length(tributary_cv_rows) + 1L]] <- data.frame(
      basin = basin, stream_order = o, n_distinct_tributaries = length(vals),
      q25_cv = qs[1L], median_cv = qs[2L], q75_cv = qs[3L],
      stringsAsFactors = FALSE)
    abs_all <- apply(AbsAcc[outlets, , drop = FALSE], 1L,
                     function(v) stats::sd(v) / mean(v))
    abs_ok <- is.finite(abs_all)
    abs_vals <- abs_all[abs_ok]
    abs_qs <- stats::quantile(abs_vals, c(.25, .5, .75), na.rm = TRUE)
    tributary_abs_cv_rows[[length(tributary_abs_cv_rows) + 1L]] <- data.frame(
      basin = basin, stream_order = o, n_distinct_tributaries = length(abs_vals),
      q25_cv = abs_qs[1L], median_cv = abs_qs[2L], q75_cv = abs_qs[3L],
      stringsAsFactors = FALSE)
    tributary_abs_cv_value_rows[[length(tributary_abs_cv_value_rows) + 1L]] <-
      data.frame(basin = basin, stream_order = o, reachid = rid[outlets][abs_ok],
                 absolute_cv = abs_vals, stringsAsFactors = FALSE)
    tributary_unit_rows[[length(tributary_unit_rows) + 1L]] <-
      data.frame(basin = basin, tributary_order = o, reachid = rid[outlets],
                 stringsAsFactors = FALSE)
    # Distribution of absolute fish held by each distinct tributary outlet.
    fish_vals <- as.numeric(AbsAcc[outlets, , drop = FALSE])
    fish_vals <- fish_vals[is.finite(fish_vals)]
    fq <- stats::quantile(fish_vals, c(.05, .25, .5, .75, .95), na.rm = TRUE)
    tributary_absfish_rows[[length(tributary_absfish_rows) + 1L]] <- data.frame(
      basin = basin, stream_order = o, n_catchments = length(outlets),
      n_catchment_years = length(fish_vals),
      pct_zero = 100 * mean(fish_vals == 0),
      p05_fish = fq[1L], q25_fish = fq[2L], median_fish = fq[3L],
      q75_fish = fq[4L], p95_fish = fq[5L],
      mean_fish = mean(fish_vals),
      mean_total_fish_at_order = mean(colSums(AbsAcc[outlets, , drop = FALSE])),
      mean_basin_total_run = mean(total_runs),
      stringsAsFactors = FALSE)
    outlets_by_order[[as.character(o)]] <- outlets
  }
  length_breaks <- seq(min(log10(upstream_length_km[keep])),
                       max(log10(upstream_length_km[keep])), length.out = 15L)
  length_bin <- cut(log10(upstream_length_km), breaks = length_breaks,
                    include.lowest = TRUE, labels = FALSE)
  pair_obs <- pairwise_change(Acc, years)
  for (k in seq_along(pair_obs)) {
    z <- pair_obs[[k]]
    z$basin <- basin; z$reachid <- rid; z$stream_order <- stream_order
    z$mean_catchment_share <- mean_acc
    z$upstream_length_km <- upstream_length_km
    z$length_bin <- length_bin
    z <- z[keep & is.finite(z$pct_change),
           c("basin", "reachid", "stream_order", "year_earlier", "year_later",
             "upstream_length_km", "length_bin", "mean_catchment_share", "pct_change")]
    observed_rows[[length(observed_rows) + 1L]] <- z
  }

  # Brennan-style null: expected reach production is exactly proportional to
  # reach length. Expected shares sum to one, while independent lognormal draws
  # are not closed within a realization (closing them would induce dependence).
  mu <- expected_share
  positive <- length_m > 0
  validation_rows[[length(validation_rows) + 1L]] <- data.frame(
    basin = basin, year = NA_integer_, raw_share_sum = NA_real_,
    normalization_factor = NA_real_, n_malformed_rows = NA_integer_,
    malformed_reachids = "", workbook_total_run = NA_real_,
    assignment_individuals_sum = NA_real_, run_total_difference = NA_real_,
    null_expected_share_sum = sum(mu),
    null_mean_basis = "reach_length_fraction", stringsAsFactors = FALSE)
  orders_present <- sort(unique(stream_order[keep]))
  for (null_cv in NULL_CVS) {
    message("  independent reach CV = ", null_cv)
    sigma_log <- sqrt(log1p(null_cv^2))
    mean_log <- rep(NA_real_, length(mu))
    mean_log[positive] <- log(mu[positive]) - 0.5 * sigma_log^2
    by_order <- setNames(vector("list", length(orders_present)), orders_present)
    tributary_by_order <- setNames(
      vector("list", length(orders_present)), orders_present
    )
    tributary_by_length <- setNames(vector("list", 12L), seq_len(12L))
    by_length <- setNames(vector("list", 14L), seq_len(14L))
    for (b in seq_len(N_SIM)) {
      sim <- matrix(0, length(mu), 2L)
      sim[positive, ] <- matrix(stats::rlnorm(2L * sum(positive),
        meanlog = rep(mean_log[positive], 2L), sdlog = sigma_log),
        nrow = sum(positive), ncol = 2L)
      sim <- sweep(sim, 2L, colSums(sim), "/")
      sim_acc <- accumulate(sim, topo_order, downidx)
      pct <- rep(NA_real_, nrow(sim_acc))
      denom <- sim_acc[, 1L] + sim_acc[, 2L]
      valid <- denom > 0
      pct[valid] <- 200 * (sim_acc[valid, 2L] - sim_acc[valid, 1L]) / denom[valid]
      for (o in orders_present) {
        vals <- pct[keep & stream_order == o]
        by_order[[as.character(o)]] <- c(by_order[[as.character(o)]], vals[is.finite(vals)])
        tributary_vals <- pct[tributary_pair_keep & stream_order == o]
        tributary_by_order[[as.character(o)]] <- c(
          tributary_by_order[[as.character(o)]],
          tributary_vals[is.finite(tributary_vals)]
        )
      }
      for (lb in seq_len(12L)) {
        tributary_length_vals <- pct[
          tributary_pair_keep & tributary_pair_length_bin == lb
        ]
        tributary_by_length[[as.character(lb)]] <- c(
          tributary_by_length[[as.character(lb)]],
          tributary_length_vals[is.finite(tributary_length_vals)]
        )
      }
      for (lb in seq_len(14L)) {
        vals <- pct[keep & length_bin == lb]
        by_length[[as.character(lb)]] <- c(by_length[[as.character(lb)]],
                                            vals[is.finite(vals)])
      }
    }
    for (lb in seq_len(12L)) {
      tributary_vals <- tributary_by_length[[as.character(lb)]]
      idx <- tributary_pair_keep & tributary_pair_length_bin == lb
      if (!length(tributary_vals) || !any(idx)) next
      tributary_aqs <- stats::quantile(
        abs(tributary_vals), c(.25, .5, .75), na.rm = TRUE
      )
      pairwise_tributary_length_null_rows[[
        length(pairwise_tributary_length_null_rows) + 1L
      ]] <- data.frame(
        basin = basin,
        assigned_length_bin = lb,
        median_assigned_upstream_length_km = stats::median(
          assigned_upstream_length_km[idx]
        ),
        assumed_cv = null_cv,
        n_simulations = N_SIM,
        n_values = length(tributary_vals),
        q25_absolute_change = tributary_aqs[1L],
        median_absolute_change = tributary_aqs[2L],
        q75_absolute_change = tributary_aqs[3L],
        stringsAsFactors = FALSE
      )
    }
    for (o in orders_present) {
      vals <- by_order[[as.character(o)]]
      qs <- stats::quantile(vals, c(.05, .5, .95), na.rm = TRUE)
      aqs <- stats::quantile(abs(vals), c(.25, .5, .75), na.rm = TRUE)
      null_rows[[length(null_rows) + 1L]] <- data.frame(
        basin = basin, stream_order = o, assumed_cv = null_cv,
        n_simulations = N_SIM, n_values = length(vals),
        p05 = qs[1L], median = qs[2L], p95 = qs[3L],
        q25_absolute_change = aqs[1L], median_absolute_change = aqs[2L],
        q75_absolute_change = aqs[3L], stringsAsFactors = FALSE)

      tributary_vals <- tributary_by_order[[as.character(o)]]
      if (length(tributary_vals)) {
        tributary_aqs <- stats::quantile(
          abs(tributary_vals), c(.25, .5, .75), na.rm = TRUE
        )
        pairwise_tributary_null_rows[[
          length(pairwise_tributary_null_rows) + 1L
        ]] <- data.frame(
          basin = basin,
          stream_order = o,
          assumed_cv = null_cv,
          n_simulations = N_SIM,
          n_values = length(tributary_vals),
          q25_absolute_change = tributary_aqs[1L],
          median_absolute_change = tributary_aqs[2L],
          q75_absolute_change = tributary_aqs[3L],
          stringsAsFactors = FALSE
        )
      }
    }
    for (lb in seq_len(14L)) {
      vals <- by_length[[as.character(lb)]]
      idx <- keep & length_bin == lb
      if (!length(vals) || !any(idx)) next
      qs <- stats::quantile(vals, c(.05, .5, .95), na.rm = TRUE)
      null_length_rows[[length(null_length_rows) + 1L]] <- data.frame(
        basin = basin, length_bin = lb,
        median_upstream_length_km = stats::median(upstream_length_km[idx]),
        assumed_cv = null_cv, n_simulations = N_SIM, n_values = length(vals),
        p05 = qs[1L], median = qs[2L], p95 = qs[3L], stringsAsFactors = FALSE)
    }

    # Full-series CV null for distinct tributary outlets. Each simulation has
    # the same number of years as the observed basin record.
    cv_by_order <- setNames(vector("list", length(outlets_by_order)),
                            names(outlets_by_order))
    abs_cv_by_order <- setNames(vector("list", length(outlets_by_order)),
                                names(outlets_by_order))
    all_closed_cv_by_order <- setNames(
      vector("list", length(orders_present)), orders_present
    )
    for (b in seq_len(N_SIM)) {
      sim <- matrix(0, length(mu), ncol(S))
      sim[positive, ] <- matrix(stats::rlnorm(ncol(S) * sum(positive),
        meanlog = rep(mean_log[positive], ncol(S)), sdlog = sigma_log),
        nrow = sum(positive), ncol = ncol(S))
      sim_acc <- accumulate(sim, topo_order, downidx)
      # Close each simulated year to relative production before accumulation,
      # matching the compositional scale of the observed assignment shares.
      sim_closed <- sweep(sim, 2L, colSums(sim), "/")
      sim_closed_acc <- accumulate(sim_closed, topo_order, downidx)
      for (o in orders_present) {
        idx <- all_relative_keep & stream_order == o
        if (!any(idx)) next
        cvs <- apply(
          sim_closed_acc[idx, , drop = FALSE], 1L,
          function(v) {
            m <- mean(v)
            if (is.finite(m) && m > 0) stats::sd(v) / m else NA_real_
          }
        )
        all_closed_cv_by_order[[as.character(o)]] <- c(
          all_closed_cv_by_order[[as.character(o)]],
          cvs[is.finite(cvs)]
        )
      }
      abs_sim_acc <- sweep(sim_acc, 2L, total_runs, "*")
      for (oname in names(outlets_by_order)) {
        outlets <- outlets_by_order[[oname]]
        cvs <- apply(sim_acc[outlets, , drop = FALSE], 1L,
                     function(v) stats::sd(v) / mean(v))
        cv_by_order[[oname]] <- c(cv_by_order[[oname]], cvs[is.finite(cvs)])
        abs_cvs <- apply(abs_sim_acc[outlets, , drop = FALSE], 1L,
                         function(v) stats::sd(v) / mean(v))
        abs_cv_by_order[[oname]] <- c(abs_cv_by_order[[oname]],
                                      abs_cvs[is.finite(abs_cvs)])
      }
    }
    for (oname in names(cv_by_order)) {
      vals <- cv_by_order[[oname]]
      qs <- stats::quantile(vals, c(.25, .5, .75), na.rm = TRUE)
      tributary_null_cv_rows[[length(tributary_null_cv_rows) + 1L]] <- data.frame(
        basin = basin, stream_order = as.integer(oname), assumed_cv = null_cv,
        q25_cv = qs[1L], median_cv = qs[2L], q75_cv = qs[3L],
        n_values = length(vals), stringsAsFactors = FALSE)
      abs_vals <- abs_cv_by_order[[oname]]
      abs_qs <- stats::quantile(abs_vals, c(.25, .5, .75), na.rm = TRUE)
      tributary_null_abs_cv_rows[[length(tributary_null_abs_cv_rows) + 1L]] <- data.frame(
        basin = basin, stream_order = as.integer(oname), assumed_cv = null_cv,
        q25_cv = abs_qs[1L], median_cv = abs_qs[2L], q75_cv = abs_qs[3L],
        n_values = length(abs_vals), stringsAsFactors = FALSE)
    }
    for (oname in names(all_closed_cv_by_order)) {
      vals <- all_closed_cv_by_order[[oname]]
      if (!length(vals)) next
      qs <- stats::quantile(vals, c(.05, .25, .5, .75, .95), na.rm = TRUE)
      all_relative_cv_null_rows[[
        length(all_relative_cv_null_rows) + 1L
      ]] <- data.frame(
        basin = basin,
        stream_order = as.integer(oname),
        assumed_cv = null_cv,
        n_simulations = N_SIM,
        n_values = length(vals),
        p05 = qs[1L],
        q25 = qs[2L],
        median = qs[3L],
        q75 = qs[4L],
        p95 = qs[5L],
        stringsAsFactors = FALSE
      )
    }
  }
}

observed <- do.call(rbind, observed_rows)
tributary_units <- do.call(rbind, tributary_unit_rows)
null <- do.call(rbind, null_rows)
null_length <- do.call(rbind, null_length_rows)
validation <- do.call(rbind, validation_rows)
tributary_cv <- do.call(rbind, tributary_cv_rows)
tributary_null_cv <- do.call(rbind, tributary_null_cv_rows)
tributary_abs_cv <- do.call(rbind, tributary_abs_cv_rows)
tributary_null_abs_cv <- do.call(rbind, tributary_null_abs_cv_rows)
tributary_abs_cv_values <- do.call(rbind, tributary_abs_cv_value_rows)
tributary_absfish <- do.call(rbind, tributary_absfish_rows)
sample_basin_cv <- do.call(rbind, sample_basin_cv_rows)
all_relative_cv <- do.call(rbind, all_relative_cv_rows)
all_relative_cv_null <- do.call(rbind, all_relative_cv_null_rows)
pairwise_tributary_units <- do.call(rbind, pairwise_tributary_unit_rows)
pairwise_tributary_null <- do.call(rbind, pairwise_tributary_null_rows)
pairwise_tributary_length_null <- do.call(
  rbind, pairwise_tributary_length_null_rows
)
sample_cv_lookup <- setNames(sample_basin_cv$sampled_year_total_run_cv,
                             sample_basin_cv$basin)
for (nm in c("q25", "median", "q75")) {
  tributary_abs_cv[[paste0(nm, "_cv_ratio_to_basin")]] <-
    tributary_abs_cv[[paste0(nm, "_cv")]] / sample_cv_lookup[tributary_abs_cv$basin]
  tributary_null_abs_cv[[paste0(nm, "_cv_ratio_to_basin")]] <-
    tributary_null_abs_cv[[paste0(nm, "_cv")]] /
    sample_cv_lookup[tributary_null_abs_cv$basin]
}
tributary_abs_cv_values$basin_cv <-
  sample_cv_lookup[tributary_abs_cv_values$basin]
tributary_abs_cv_values$cv_ratio_to_basin <-
  tributary_abs_cv_values$absolute_cv / tributary_abs_cv_values$basin_cv
tributary_abs_cv_values$anomaly_pct <-
  100 * (tributary_abs_cv_values$cv_ratio_to_basin - 1)
tributary_anomaly <- do.call(rbind, lapply(
  split(tributary_abs_cv_values,
        list(tributary_abs_cv_values$basin, tributary_abs_cv_values$stream_order),
        drop = TRUE),
  function(z) data.frame(
    basin = z$basin[1L], stream_order = z$stream_order[1L],
    n_distinct_tributaries = nrow(z),
    mean_anomaly_pct = mean(z$anomaly_pct),
    median_anomaly_pct = stats::median(z$anomaly_pct),
    q25_anomaly_pct = unname(stats::quantile(z$anomaly_pct, .25)),
    q75_anomaly_pct = unname(stats::quantile(z$anomaly_pct, .75)),
    stringsAsFactors = FALSE)))
tributary_anomaly <- tributary_anomaly[
  order(tributary_anomaly$basin, tributary_anomaly$stream_order), ]
obs_summary <- do.call(rbind, lapply(split(observed, list(observed$basin, observed$stream_order), drop = TRUE),
  function(z) {
    qs <- stats::quantile(z$pct_change,
                          c(.05, .15, .25, .50, .75, .85, .95), na.rm = TRUE)
    aqs <- stats::quantile(abs(z$pct_change), c(.25, .5, .75), na.rm = TRUE)
    data.frame(basin = z$basin[1L], stream_order = z$stream_order[1L],
      n_catchments = length(unique(z$reachid)), n_pairwise_values = nrow(z),
      p05 = qs[1L], p10 = qs[2L], q25 = qs[3L], median = qs[4L],
      q75 = qs[5L], p90 = qs[6L], p95 = qs[7L],
      q25_absolute_change = aqs[1L], median_absolute_change = aqs[2L],
      q75_absolute_change = aqs[3L],
      mean_absolute_change = mean(abs(z$pct_change)), stringsAsFactors = FALSE)
  }))
obs_summary <- obs_summary[order(obs_summary$basin, obs_summary$stream_order), ]

# Same nested accumulation, but retain one downstream endpoint per maximal
# same-order tributary rather than every reach along each tributary.
pairwise_tributary_observed <- merge(
  observed,
  pairwise_tributary_units[
    , c(
      "basin", "reachid", "stream_order", "upstream_length_km",
      "assigned_upstream_length_km", "assigned_length_bin"
    )
  ],
  by = c("basin", "reachid", "stream_order", "upstream_length_km"),
  all = FALSE
)
pairwise_tributary_summary <- do.call(rbind, lapply(
  split(
    pairwise_tributary_observed,
    list(
      pairwise_tributary_observed$basin,
      pairwise_tributary_observed$stream_order
    ),
    drop = TRUE
  ),
  function(z) {
    aqs <- stats::quantile(abs(z$pct_change), c(.25, .5, .75), na.rm = TRUE)
    data.frame(
      basin = z$basin[1L],
      stream_order = z$stream_order[1L],
      n_tributaries = length(unique(z$reachid)),
      n_pairwise_values = nrow(z),
      median_tributary_length_km = stats::median(z$upstream_length_km),
      q25_absolute_change = aqs[1L],
      median_absolute_change = aqs[2L],
      q75_absolute_change = aqs[3L],
      mean_absolute_change = mean(abs(z$pct_change)),
      stringsAsFactors = FALSE
    )
  }
))
pairwise_tributary_summary <- pairwise_tributary_summary[
  order(
    pairwise_tributary_summary$basin,
    pairwise_tributary_summary$stream_order
  ),
]

pairwise_tributary_length_summary <- do.call(rbind, lapply(
  split(
    pairwise_tributary_observed,
    list(
      pairwise_tributary_observed$basin,
      pairwise_tributary_observed$assigned_length_bin
    ),
    drop = TRUE
  ),
  function(z) {
    aqs <- stats::quantile(abs(z$pct_change), c(.25, .5, .75), na.rm = TRUE)
    data.frame(
      basin = z$basin[1L],
      assigned_length_bin = z$assigned_length_bin[1L],
      n_tributaries = length(unique(z$reachid)),
      n_pairwise_values = nrow(z),
      median_assigned_upstream_length_km =
        stats::median(z$assigned_upstream_length_km),
      q25_absolute_change = aqs[1L],
      median_absolute_change = aqs[2L],
      q75_absolute_change = aqs[3L],
      mean_absolute_change = mean(abs(z$pct_change)),
      stringsAsFactors = FALSE
    )
  }
))
pairwise_tributary_length_summary <- pairwise_tributary_length_summary[
  order(
    pairwise_tributary_length_summary$basin,
    pairwise_tributary_length_summary$assigned_length_bin
  ),
]
obs_length_summary <- do.call(rbind, lapply(
  split(observed, list(observed$basin, observed$length_bin), drop = TRUE),
  function(z) {
    qs <- stats::quantile(z$pct_change,
                          c(.05, .10, .25, .50, .75, .90, .95), na.rm = TRUE)
    data.frame(basin = z$basin[1L], length_bin = z$length_bin[1L],
      n_catchments = length(unique(z$reachid)), n_pairwise_values = nrow(z),
      median_upstream_length_km = stats::median(z$upstream_length_km),
      p05 = qs[1L], p10 = qs[2L], q25 = qs[3L], median = qs[4L],
      q75 = qs[5L], p90 = qs[6L], p95 = qs[7L],
      mean_absolute_change = mean(abs(z$pct_change)), stringsAsFactors = FALSE)
  }))
obs_length_summary <- obs_length_summary[order(obs_length_summary$basin,
                                                obs_length_summary$length_bin), ]

# Same proportional pairwise-change data, restricted to one accumulated outlet
# per distinct tributary (plus the single basin outlet at the cutoff).
tributary_observed <- merge(observed, tributary_units,
                            by = c("basin", "reachid"), all = FALSE)
tributary_observed$tributary_length_bin <- NA_integer_
for (basin in names(BASINS)) {
  idx <- tributary_observed$basin == basin
  lx <- log10(tributary_observed$upstream_length_km[idx])
  br <- seq(min(lx), max(lx), length.out = 15L)
  tributary_observed$tributary_length_bin[idx] <-
    cut(lx, breaks = br, include.lowest = TRUE, labels = FALSE)
}
tributary_length_summary <- do.call(rbind, lapply(
  split(tributary_observed,
        list(tributary_observed$basin, tributary_observed$tributary_length_bin),
        drop = TRUE),
  function(z) {
    qs <- stats::quantile(z$pct_change,
                          c(.05, .15, .25, .50, .75, .85, .95), na.rm = TRUE)
    data.frame(
      basin = z$basin[1L], length_bin = z$tributary_length_bin[1L],
      n_tributaries = length(unique(z$reachid)), n_pairwise_values = nrow(z),
      min_upstream_length_km = min(z$upstream_length_km),
      median_upstream_length_km = stats::median(z$upstream_length_km),
      max_upstream_length_km = max(z$upstream_length_km),
      p05 = qs[1L], p15 = qs[2L], q25 = qs[3L], median = qs[4L],
      q75 = qs[5L], p85 = qs[6L], p95 = qs[7L],
      stringsAsFactors = FALSE)
  }))
tributary_length_summary <- tributary_length_summary[
  order(tributary_length_summary$basin, tributary_length_summary$length_bin), ]

write.csv(observed, file.path(out_dir, paste0("BrennanPairwise_observed", OUTPUT_TAG, ".csv")), row.names = FALSE)
write.csv(obs_summary, file.path(out_dir, paste0("BrennanPairwise_observed_by_order", OUTPUT_TAG, ".csv")), row.names = FALSE)
write.csv(null, file.path(out_dir, paste0("BrennanPairwise_independentCV_null", OUTPUT_TAG, ".csv")), row.names = FALSE)
write.csv(
  pairwise_tributary_observed,
  file.path(
    out_dir, paste0("BrennanPairwise_tributary_observed", OUTPUT_TAG, ".csv")
  ),
  row.names = FALSE
)
write.csv(
  pairwise_tributary_summary,
  file.path(
    out_dir,
    paste0("BrennanPairwise_tributary_observed_by_order", OUTPUT_TAG, ".csv")
  ),
  row.names = FALSE
)
write.csv(
  pairwise_tributary_null,
  file.path(
    out_dir,
    paste0("BrennanPairwise_tributary_lengthNull", OUTPUT_TAG, ".csv")
  ),
  row.names = FALSE
)
write.csv(
  pairwise_tributary_length_summary,
  file.path(
    out_dir,
    paste0(
      "BrennanPairwise_tributary_observed_by_assignedLength",
      OUTPUT_TAG, ".csv"
    )
  ),
  row.names = FALSE
)
write.csv(
  pairwise_tributary_length_null,
  file.path(
    out_dir,
    paste0(
      "BrennanPairwise_tributary_assignedLengthNull",
      OUTPUT_TAG, ".csv"
    )
  ),
  row.names = FALSE
)
write.csv(obs_length_summary, file.path(out_dir, paste0("BrennanPairwise_observed_by_upstreamLength", OUTPUT_TAG, ".csv")), row.names = FALSE)
write.csv(tributary_observed, file.path(out_dir, paste0("BrennanPairwise_distinctTributaries", OUTPUT_TAG, ".csv")), row.names = FALSE)
write.csv(tributary_length_summary, file.path(out_dir, paste0("BrennanPairwise_distinctTributaries_by_upstreamLength", OUTPUT_TAG, ".csv")), row.names = FALSE)
write.csv(null_length, file.path(out_dir, paste0("BrennanPairwise_null_by_upstreamLength", OUTPUT_TAG, ".csv")), row.names = FALSE)
write.csv(validation, file.path(out_dir, paste0("BrennanPairwise_validation", OUTPUT_TAG, ".csv")), row.names = FALSE)
write.csv(tributary_cv, file.path(out_dir, paste0("DistinctTributary_CV_by_order", OUTPUT_TAG, ".csv")), row.names = FALSE)
write.csv(tributary_null_cv, file.path(out_dir, paste0("DistinctTributary_CV_null_by_order", OUTPUT_TAG, ".csv")), row.names = FALSE)
write.csv(tributary_abs_cv, file.path(out_dir, paste0("DistinctTributary_absoluteCV_by_order", OUTPUT_TAG, ".csv")), row.names = FALSE)
write.csv(tributary_null_abs_cv, file.path(out_dir, paste0("DistinctTributary_absoluteCV_null_by_order", OUTPUT_TAG, ".csv")), row.names = FALSE)
write.csv(tributary_abs_cv_values, file.path(out_dir, paste0("DistinctTributary_absoluteCV_values", OUTPUT_TAG, ".csv")), row.names = FALSE)
write.csv(tributary_anomaly, file.path(out_dir, paste0("DistinctTributary_CV_anomaly_by_order", OUTPUT_TAG, ".csv")), row.names = FALSE)
write.csv(tributary_absfish, file.path(out_dir, paste0("DistinctTributary_absoluteFish_by_order", OUTPUT_TAG, ".csv")), row.names = FALSE)
write.csv(sample_basin_cv, file.path(out_dir, paste0("SampledYear_TotalRun_CV", OUTPUT_TAG, ".csv")), row.names = FALSE)
write.csv(
  all_relative_cv,
  file.path(out_dir, paste0("AllCatchments_relativeCV", OUTPUT_TAG, ".csv")),
  row.names = FALSE
)
write.csv(
  all_relative_cv_null,
  file.path(
    out_dir, paste0("AllCatchments_relativeCV_closedNull", OUTPUT_TAG, ".csv")
  ),
  row.names = FALSE
)

# ---- Single portfolio-effect figure -----------------------------------------
length_png <- file.path(fig_dir, paste0("BrennanPairwiseChange_upstreamLength", OUTPUT_TAG, ".png"))
grDevices::png(length_png, width = 3600, height = 5200, res = 300, bg = "white")
op <- par(no.readonly = TRUE)
layout(matrix(1:10, nrow = 5, byrow = TRUE), heights = c(1.65, 1.65, 1, 1, 1))
par(mar = c(4.5, 5.4, 3.6, 1.2), oma = c(1.0, 1.0, 3.3, .5),
    las = 1, family = "sans", cex.axis = .92, cex.lab = 1.05)
for (basin in names(BASINS)) {
  d <- observed[observed$basin == basin, ]
  nd <- null_length[null_length$basin == basin, ]
  od <- obs_length_summary[obs_length_summary$basin == basin, ]
  od <- od[order(od$median_upstream_length_km), ]
  x <- log10(d$upstream_length_km)
  limvals <- c(stats::quantile(d$pct_change, c(.01, .99), na.rm = TRUE),
               nd$p05, nd$p95)
  ylim <- range(limvals, finite = TRUE); pad <- diff(ylim) * .07; ylim <- ylim + c(-pad, pad)
  xlim <- range(x, finite = TRUE) + c(0, .14)
  plot(x, d$pct_change, type = "n", ylim = ylim,
       xlim = xlim,
       xaxt = "n", xlab = "Total upstream channel length (km)",
       ylab = "Interannual variability\n(signed symmetric % difference)")
  ticks <- pretty(range(x)); axis(1, at = ticks, labels = format(10^ticks, big.mark = ",", scientific = FALSE))
  abline(h = 0, col = "#777777", lwd = 1)
  odx <- log10(od$median_upstream_length_km)
  points(x, d$pct_change, pch = 16, cex = .20,
         col = paste0(BASINS[[basin]]$colour, "20"))
  polygon(c(odx, rev(odx)), c(od$p05, rev(od$p95)),
          col = paste0(BASINS[[basin]]$colour, "18"), border = NA)
  polygon(c(odx, rev(odx)), c(od$p10, rev(od$p90)),
          col = paste0(BASINS[[basin]]$colour, "30"), border = NA)
  polygon(c(odx, rev(odx)), c(od$q25, rev(od$q75)),
          col = paste0(BASINS[[basin]]$colour, "50"), border = NA)
  lines(odx, od$p10, col = paste0(BASINS[[basin]]$colour, "A0"), lwd = .8)
  lines(odx, od$p90, col = paste0(BASINS[[basin]]$colour, "A0"), lwd = .8)
  lines(odx, od$q25, col = BASINS[[basin]]$colour, lwd = 1)
  lines(odx, od$q75, col = BASINS[[basin]]$colour, lwd = 1)
  for (v in unique(nd$assumed_cv)) {
    z <- nd[abs(nd$assumed_cv - v) < 1e-8, ]; z <- z[order(z$median_upstream_length_km), ]
    zx <- log10(z$median_upstream_length_km)
    lt <- if (abs(v - 0.25) < 1e-8) 3 else 2
    lines(zx, z$p05, col = "#222222", lty = lt, lwd = 2)
    lines(zx, z$p95, col = "#222222", lty = lt, lwd = 2)
  }
  title(main = paste0(basin, "  (long-term return CV = ",
                      formatC(BASINS[[basin]]$long_term_return_cv,
                              digits = 3, format = "f"), ")"),
        font.main = 2, col.main = BASINS[[basin]]$colour)
  if (basin == "Kuskokwim") legend("topright",
    legend = c("Observed central 50%", "Observed central 80%",
               "Observed central 90%",
               "Independent reaches: CV = 0.25",
               "Independent reaches: CV = 1.0"),
    fill = c(paste0(BASINS[[basin]]$colour, "68"),
             paste0(BASINS[[basin]]$colour, "40"),
             paste0(BASINS[[basin]]$colour, "22"), NA, NA),
    border = NA, col = c(NA, NA, NA, "#222222", "#222222"),
    lty = c(NA, NA, NA, 3, 2), lwd = c(NA, NA, NA, 2, 2),
    bty = "n", cex = .72)
}

# Alternative display: identical proportional pairwise analysis, but only one
# accumulated outlet per distinct tributary is shown.
for (basin in names(BASINS)) {
  d <- tributary_observed[tributary_observed$basin == basin, ]
  nd <- null_length[null_length$basin == basin, ]
  od <- tributary_length_summary[tributary_length_summary$basin == basin, ]
  od <- od[order(od$median_upstream_length_km), ]
  x <- log10(d$upstream_length_km)
  display_probs <- if (basin == "Kuskokwim") c(.025, .975) else c(.01, .99)
  limvals <- c(stats::quantile(d$pct_change, display_probs, na.rm = TRUE),
               nd$p05, nd$p95)
  ylim <- range(limvals, finite = TRUE)
  pad <- diff(ylim) * .07; ylim <- ylim + c(-pad, pad)
  xlim <- range(x, finite = TRUE) + c(0, .14)
  plot(x, d$pct_change, type = "n", ylim = ylim, xlim = xlim,
       xaxt = "n", xlab = "Total upstream channel length (km)",
       ylab = "Interannual variability\n(signed symmetric % difference)")
  ticks <- pretty(range(x))
  axis(1, at = ticks,
       labels = format(10^ticks, big.mark = ",", scientific = FALSE))
  abline(h = 0, col = "#777777", lwd = 1)
  odx <- log10(od$median_upstream_length_km)
  polygon(c(odx, rev(odx)), c(od$p05, rev(od$p95)),
          col = paste0(BASINS[[basin]]$colour, "22"), border = NA)
  polygon(c(odx, rev(odx)), c(od$p15, rev(od$p85)),
          col = paste0(BASINS[[basin]]$colour, "40"), border = NA)
  polygon(c(odx, rev(odx)), c(od$q25, rev(od$q75)),
          col = paste0(BASINS[[basin]]$colour, "68"), border = NA)
  lines(odx, od$p15, col = paste0(BASINS[[basin]]$colour, "A0"), lwd = .8)
  lines(odx, od$p85, col = paste0(BASINS[[basin]]$colour, "A0"), lwd = .8)
  lines(odx, od$q25, col = BASINS[[basin]]$colour, lwd = 1)
  lines(odx, od$q75, col = BASINS[[basin]]$colour, lwd = 1)
  points(x, d$pct_change, pch = 16, cex = .38,
         col = paste0(BASINS[[basin]]$colour, "B0"))
  for (v in unique(nd$assumed_cv)) {
    z <- nd[abs(nd$assumed_cv - v) < 1e-8, ]
    z <- z[order(z$median_upstream_length_km), ]
    zx <- log10(z$median_upstream_length_km)
    lt <- if (abs(v - 0.25) < 1e-8) 3 else 2
    lines(zx, z$p05, col = "#222222", lty = lt, lwd = 2)
    lines(zx, z$p95, col = "#222222", lty = lt, lwd = 2)
  }
  title(main = paste0(basin, " - distinct tributary units"), font.main = 2,
        col.main = BASINS[[basin]]$colour)
  if (basin == "Kuskokwim") legend("topright",
    legend = c("Observed central 50%", "Observed central 70%",
               "Observed central 90%", "Independent reaches: CV = 0.25",
               "Independent reaches: CV = 1.0"),
    fill = c(paste0(BASINS[[basin]]$colour, "50"),
             paste0(BASINS[[basin]]$colour, "30"),
             paste0(BASINS[[basin]]$colour, "18"), NA, NA),
    border = NA, col = c(NA, NA, NA, "#222222", "#222222"),
    lty = c(NA, NA, NA, 3, 2), lwd = c(NA, NA, NA, 2, 2),
    bty = "n", cex = .68)
}

for (basin in names(BASINS)) {
  s <- tributary_cv[tributary_cv$basin == basin, ]
  nd <- tributary_null_cv[tributary_null_cv$basin == basin, ]
  s <- s[order(s$stream_order), ]
  ylim <- range(c(s$q25_cv, s$q75_cv, nd$q25_cv, nd$q75_cv), finite = TRUE)
  ylim <- c(0, ylim[2L] * 1.15)
  plot(s$stream_order, s$median_cv, type = "n", ylim = ylim,
       xlab = "Stream order (cutoff = basin outlet)",
       ylab = "Interannual CV of basin contribution", xaxt = "n")
  axis(1, at = s$stream_order)
  polygon(c(s$stream_order, rev(s$stream_order)),
          c(s$q25_cv, rev(s$q75_cv)),
          col = paste0(BASINS[[basin]]$colour, "30"), border = NA)
  lines(s$stream_order, s$median_cv,
        col = BASINS[[basin]]$colour, lwd = 3)
  points(s$stream_order, s$median_cv, pch = 21, cex = 1.35,
         lwd = 2, col = "white", bg = BASINS[[basin]]$colour)
  for (v in NULL_CVS) {
    z <- nd[abs(nd$assumed_cv - v) < 1e-8, ]
    z <- z[order(z$stream_order), ]
    lt <- if (v == 0.25) 3 else 2
    lines(z$stream_order, z$median_cv,
          col = "#222222", lwd = 2.2, lty = lt)
    points(z$stream_order, z$median_cv,
           pch = 21, cex = 1.05, col = "#222222", bg = "white")
  }
  if (basin == "Kuskokwim") legend("topright",
    legend = c("Distinct tributaries; cutoff = basin outlet",
               "Independent null median: CV = 0.25",
               "Independent null median: CV = 1.0"),
    col = c(BASINS[[basin]]$colour, "#222222", "#222222"),
    lty = c(1, 3, 2), lwd = c(3, 2.2, 2.2), pch = c(21, 21, 21),
    pt.bg = c(BASINS[[basin]]$colour, "white", "white"), bty = "n", cex = .72)
}
draw_violin <- function(values, x, width, fill, border) {
  values <- values[is.finite(values)]
  if (length(unique(values)) > 1L) {
    den <- stats::density(values, from = min(values), to = max(values), n = 256)
    half <- width * den$y / max(den$y)
    polygon(c(x - half, rev(x + half)), c(den$x, rev(den$x)),
            col = fill, border = border, lwd = 1.2)
  }
  points(jitter(rep(x, length(values)), amount = width * .35), values,
         pch = 16, cex = .42, col = paste0(border, "70"))
  points(x, stats::median(values), pch = 21, cex = 1.15,
         bg = "white", col = border, lwd = 1.6)
}

# All distinct-tributary absolute-production CV values in their original CV
# units. The horizontal reference is the basin-wide CV for the sampled years.
for (basin in names(BASINS)) {
  v <- tributary_abs_cv_values[tributary_abs_cv_values$basin == basin, ]
  v <- v[v$stream_order < BASINS[[basin]]$mainstem_order, ]
  orders <- sort(unique(v$stream_order))
  basin_cv <- sample_cv_lookup[[basin]]
  ylim <- range(v$absolute_cv, basin_cv, finite = TRUE)
  ylim <- c(ylim[1L] / 1.18, ylim[2L] * 1.18)
  plot(NA, xlim = range(orders) + c(-.6, .6), ylim = ylim, log = "y",
       xaxt = "n", yaxt = "n",
       xlab = "Stream order (cutoff = basin outlet)",
       ylab = "Absolute-production CV (log scale)")
  axis(1, at = orders)
  cv_ticks <- c(.05, .1, .2, .5, 1, 2, 5)
  cv_ticks <- cv_ticks[cv_ticks >= ylim[1L] & cv_ticks <= ylim[2L]]
  axis(2, at = cv_ticks, labels = format(cv_ticks, trim = TRUE))
  abline(h = basin_cv, col = "#555555", lty = 3, lwd = 1.3)
  for (o in orders) {
    draw_violin(v$absolute_cv[v$stream_order == o], o, .36,
                paste0(BASINS[[basin]]$colour, "38"), BASINS[[basin]]$colour)
  }
  if (basin == "Kuskokwim") legend("topright",
    legend = c("All tributaries; cutoff = basin outlet", "Median", "Basin-wide CV"),
    pch = c(16, 21, NA), pt.bg = c(NA, "white", NA),
    col = c(BASINS[[basin]]$colour, BASINS[[basin]]$colour, "#555555"),
    lty = c(NA, NA, 3), lwd = c(NA, NA, 1.3), bty = "n", cex = .78)
}

# Mean anomaly by stream order. Positive values are more variable than the
# basin-wide run; negative values are more buffered than the basin-wide run.
for (basin in names(BASINS)) {
  v <- tributary_abs_cv_values[tributary_abs_cv_values$basin == basin, ]
  v <- v[v$stream_order < BASINS[[basin]]$mainstem_order, ]
  orders <- sort(unique(v$stream_order))
  stats_by_order <- lapply(orders, function(o) {
    x <- v$anomaly_pct[v$stream_order == o]
    c(mean = mean(x))
  })
  a <- do.call(rbind, stats_by_order)
  ylim <- range(a[, "mean"], 0, finite = TRUE)
  pad <- max(diff(ylim) * .12, 5); ylim <- ylim + c(-pad, pad)
  barplot(a[, "mean"], names.arg = orders, col = "#003049", border = NA,
          ylim = ylim, xlab = "Stream order (cutoff = basin outlet)",
          ylab = "Mean % difference from basin-wide CV")
  abline(h = 0, col = "#444444", lwd = 1.2)
}
mtext("Pairwise changes in production portions along a continuous habitat scale",
      side = 3, outer = TRUE, line = 1.5, cex = 1.5, font = 2)
mtext(paste0("Assignment threshold = ", if (ASSIGNMENT_SETTING == "current") "0.0" else ASSIGNMENT_SETTING,
             "; mainstem cutoffs: Kuskokwim order 7, Yukon order 8"),
      side = 3, outer = TRUE, line = .2, cex = .88, col = "#444444")
par(op); dev.off()
message("Wrote Brennan-style pairwise-change outputs and figure.")
