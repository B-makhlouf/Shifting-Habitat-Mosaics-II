################################################################################
# Shifting Habitat Mosaics II - Parameter Explorer (Shiny)
#
# A native R/Shiny dashboard for exploring how the six main publication figures
# change as you vary the parameters in Code/Analysis/params.R.
#
# It edits params.R in place, then re-runs ONLY the analysis scripts that the
# changed parameters affect (in dependency order, in this same R session), and
# redisplays the figures.
#
# RUN (from the project, e.g. in RStudio):
#   shiny::runApp("Code/ParamExplorer")
# or open this file in RStudio and click "Run App".
#
# Requires the same packages the analysis already uses (sf, dplyr, ggplot2, ks,
# cowplot, here, ...) plus `shiny`.
################################################################################

library(shiny)

# --------------------------------------------------------------------------- #
# Locate the project
# --------------------------------------------------------------------------- #
find_root <- function(start = getwd()) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in 1:12) {
    if (file.exists(file.path(d, "Shifting-Habitat-Mosaics-II.Rproj")) ||
        file.exists(file.path(d, "Code", "Analysis", "params.R"))) return(d)
    parent <- dirname(d); if (parent == d) break; d <- parent
  }
  getwd()
}
PROJECT_ROOT <- find_root()
PARAMS_FILE  <- file.path(PROJECT_ROOT, "Code", "Analysis", "params.R")
ANALYSIS_DIR <- file.path(PROJECT_ROOT, "Code", "Analysis")
FIGURES_DIR  <- file.path(PROJECT_ROOT, "Figures")
setwd(PROJECT_ROOT)
addResourcePath("shmfigs", FIGURES_DIR)

# --------------------------------------------------------------------------- #
# Parameter schema
#   block: "KUSKO", "YUKON", or "TOP" (a bare top-level assignment)
#   affects: figure keys that must be regenerated when this param changes
# --------------------------------------------------------------------------- #
MAP_FIGS <- c("fig1", "fig3_rel", "fig3_abs", "fig4_sdlog", "fig4_pairwise")

SCHEMA <- list(
  list(id="kusko_min_stream_order", key="min_stream_order", block="KUSKO", group="Kuskokwim",
       label="Min stream order", type="int", min=1, max=9, step=1, affects=MAP_FIGS,
       help="Minimum Strahler stream order included."),
  list(id="kusko_min_error", key="min_error", block="KUSKO", group="Kuskokwim",
       label="Min error (clamp)", type="float", min=0.0001, max=0.005, step=0.0001, decimals=5, affects=MAP_FIGS,
       help="Lower-bound clamp on pid_isose error."),
  list(id="kusko_max_error", key="max_error", block="KUSKO", group="Kuskokwim",
       label="Max error (clamp)", type="float", min=0.0002, max=0.005, step=0.0001, decimals=5, affects=MAP_FIGS,
       help="Upper-bound clamp (quartiles analysis)."),
  list(id="kusko_sensitivity_threshold", key="sensitivity_threshold", block="KUSKO", group="Kuskokwim",
       label="Sensitivity threshold", type="float", min=0, max=1, step=0.05, decimals=2, affects=MAP_FIGS,
       help="Rescaled assignment values below this -> 0."),
  list(id="kusko_channel_slope_cutoff", key="channel_slope_cutoff", block="KUSKO", group="Kuskokwim",
       label="Channel slope cutoff", type="float", min=0, max=10, step=0.5, decimals=1, affects=MAP_FIGS,
       help="Channel_sl above this -> excluded (NewHabitatPrior)."),

  list(id="yukon_min_stream_order", key="min_stream_order", block="YUKON", group="Yukon",
       label="Min stream order", type="int", min=1, max=9, step=1, affects=MAP_FIGS,
       help="Minimum Strahler stream order included."),
  list(id="yukon_min_error", key="min_error", block="YUKON", group="Yukon",
       label="Min error (clamp)", type="float", min=0.0001, max=0.01, step=0.0001, decimals=5, affects=MAP_FIGS,
       help="Lower-bound clamp on pid_isose error."),
  list(id="yukon_sensitivity_threshold", key="sensitivity_threshold", block="YUKON", group="Yukon",
       label="Sensitivity threshold", type="float", min=0, max=1, step=0.05, decimals=2, affects=MAP_FIGS,
       help="Rescaled assignment values below this -> 0."),
  list(id="yukon_channel_slope_cutoff", key="channel_slope_cutoff", block="YUKON", group="Yukon",
       label="Channel slope cutoff", type="float", min=0, max=10, step=0.5, decimals=1, affects=MAP_FIGS,
       help="Channel_sl above this -> excluded (NewHabitatPrior)."),
  list(id="yukon_porcupine_target", key="porcupine_target", block="YUKON", group="Yukon",
       label="Porcupine target", type="float", min=0, max=0.5, step=0.01, decimals=2, affects=MAP_FIGS,
       help="Target proportion of Canadian basin assigned to Porcupine."),

  list(id="contour_filt_thresh", key="CONTOUR_FILT_THRESH", block="TOP", group="Contours",
       label="Contour filter threshold", type="float", min=0, max=1, step=0.1, decimals=1, affects=c("fig1", "fig2"),
       help="assignment_norm minimum for a reach to appear in Fig 2 contours.")
)
names(SCHEMA) <- vapply(SCHEMA, function(p) p$id, character(1))
schema_by_id  <- function(id) SCHEMA[[id]]

# --------------------------------------------------------------------------- #
# Figures + scripts
# --------------------------------------------------------------------------- #
SCRIPT_FILES <- list(
  "01"  = "01_FullBasinRelativeProdMaps.R",
  "02"  = "02_ContourThreshnew.R",
  "fig1"= "PresentationFigures.R",
  "03e" = "05_PortfolioEffect.R",
  "03f" = "05_PortfolioEffect.R"
)
SCRIPT_ORDER <- c("01", "02", "fig1", "03e", "03f")

FIGURES <- list(
  fig1 = list(title="Fig 1 - Kuskokwim maps and contours",
              scripts=c("01","02","fig1"),
              files="00_PubFigures/Fig1_KuskokwimMapsContours.png"),
  fig2 = list(title="Fig 2 - Density contours", scripts=c("02"), glob="02_Contours"),
  fig3_rel = list(title="Fig 3 - Nested CV (relative)", scripts=c("01","03e"),
                  files=c("00_PubFigures/Fig3_KuskoNestedCV.jpg","00_PubFigures/Fig3_YukonNestedCV.jpg")),
  fig3_abs = list(title="Fig 3 - Nested CV (absolute)", scripts=c("01","03e"),
                  files=c("00_PubFigures/Fig3_KuskoNestedCV_absolute.jpg","00_PubFigures/Fig3_YukonNestedCV_absolute.jpg")),
  fig4_sdlog = list(title="Fig 4 - Portfolio SD(log)", scripts=c("01","03f"),
                    files="00_PubFigures/Fig4_Portfolio_SDlog.jpg"),
  fig4_pairwise = list(title="Fig 4 - Portfolio pairwise", scripts=c("01","03f"),
                       files="00_PubFigures/Fig4_Portfolio_pairwise.jpg")
)
FIGURE_ORDER <- c("fig1","fig2","fig3_rel","fig3_abs","fig4_sdlog","fig4_pairwise")

scripts_for_targets <- function(targets) {
  needed <- unique(unlist(lapply(targets, function(t) FIGURES[[t]]$scripts)))
  SCRIPT_ORDER[SCRIPT_ORDER %in% needed]
}

# --------------------------------------------------------------------------- #
# params.R parsing / rewriting  (line-based, block-aware)
# --------------------------------------------------------------------------- #
NUM_P <- "[-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?"

.block_of_lines <- function(lines) {
  # returns a character vector (same length) tagging each line KUSKO/YUKON/NA
  tag <- rep(NA_character_, length(lines)); cur <- NA_character_; depth <- 0
  for (i in seq_along(lines)) {
    l <- lines[i]
    if (is.na(cur)) {
      if (grepl("KUSKO_PARAMS\\s*<-\\s*list\\s*\\(", l)) { cur <- "KUSKO"; depth <- 0 }
      else if (grepl("YUKON_PARAMS\\s*<-\\s*list\\s*\\(", l)) { cur <- "YUKON"; depth <- 0 }
    }
    tag[i] <- cur
    if (!is.na(cur)) {
      depth <- depth + lengths(regmatches(l, gregexpr("\\(", l))) -
                        lengths(regmatches(l, gregexpr("\\)", l)))
      if (depth <= 0) cur <- NA_character_
    }
  }
  tag
}

read_params <- function() {
  lines <- readLines(PARAMS_FILE, warn = FALSE)
  tag <- .block_of_lines(lines)
  out <- list()
  for (p in SCHEMA) {
    val <- NA_real_
    if (p$block == "TOP") {
      hit <- grep(paste0("^\\s*", p$key, "\\s*<-\\s*", NUM_P), lines)
      if (length(hit)) {
        m <- regmatches(lines[hit[1]], regexpr(paste0(p$key, "\\s*<-\\s*", NUM_P), lines[hit[1]]))
        val <- as.numeric(sub(paste0(".*<-\\s*(", NUM_P, ").*"), "\\1", m, perl = TRUE))
      }
    } else {
      idx <- which(tag == p$block & grepl(paste0("\\b", p$key, "\\s*="), lines))
      if (length(idx)) {
        l <- lines[idx[1]]
        val <- as.numeric(sub(paste0(".*\\b", p$key, "\\s*=\\s*(", NUM_P, ").*"), "\\1", l, perl = TRUE))
      }
    }
    if (p$type == "int" && !is.na(val)) val <- as.integer(round(val))
    out[[p$id]] <- val
  }
  out
}

.fmt <- function(p, v) {
  if (p$type == "int") return(as.character(as.integer(round(v))))
  formatC(v, format = "f", digits = 10, drop0trailing = TRUE)
}

write_params <- function(values) {
  lines <- readLines(PARAMS_FILE, warn = FALSE)
  tag <- .block_of_lines(lines)
  for (id in names(values)) {
    p <- schema_by_id(id); if (is.null(p)) next
    v <- values[[id]]; if (is.null(v) || is.na(v)) next
    newnum <- .fmt(p, v)
    if (p$block == "TOP") {
      idx <- grep(paste0("^\\s*", p$key, "\\s*<-\\s*", NUM_P), lines)
      if (length(idx))
        lines[idx[1]] <- sub(paste0("(", p$key, "\\s*<-\\s*)", NUM_P),
                             paste0("\\1", newnum), lines[idx[1]], perl = TRUE)
    } else {
      idx <- which(tag == p$block & grepl(paste0("\\b", p$key, "\\s*="), lines))
      if (length(idx))
        lines[idx[1]] <- sub(paste0("(\\b", p$key, "\\s*=\\s*)", NUM_P),
                             paste0("\\1", newnum), lines[idx[1]], perl = TRUE)
    }
  }
  writeLines(lines, PARAMS_FILE)
  invisible(TRUE)
}

backup_params <- function() {
  bak <- paste0(PARAMS_FILE, ".explorer.bak")
  if (!file.exists(bak)) file.copy(PARAMS_FILE, bak)
}

# --------------------------------------------------------------------------- #
# Figure file discovery
# --------------------------------------------------------------------------- #
figure_entries <- function(fkey, contour_val) {
  fdef <- FIGURES[[fkey]]
  paths <- character(0)
  if (!is.null(fdef$glob)) {
    d <- file.path(FIGURES_DIR, fdef$glob)
    tag <- sprintf("thresh%.1f", as.numeric(contour_val))
    if (dir.exists(d)) {
      all <- list.files(d, pattern = "\\.(png|jpg|jpeg)$", full.names = FALSE)
      paths <- file.path(fdef$glob, sort(all[grepl(tag, all, fixed = TRUE)]))
    }
  } else {
    for (rel in fdef$files) if (file.exists(file.path(FIGURES_DIR, rel))) paths <- c(paths, rel)
  }
  lapply(paths, function(rel) {
    fp <- file.path(FIGURES_DIR, rel)
    list(rel = rel, name = basename(rel),
         mtime = as.numeric(file.info(fp)$mtime))
  })
}

img_tags <- function(entries, gallery = FALSE) {
  if (!length(entries)) return(tags$div(class = "empty", "not generated yet"))
  lapply(entries, function(e) {
    src <- paste0("shmfigs/", utils::URLencode(e$rel), "?v=", as.integer(e$mtime))
    tags$img(src = src, title = e$name,
             style = if (gallery) "max-width:calc(50% - 6px);border-radius:6px;background:#fff;margin:3px"
                     else "max-width:100%;border-radius:6px;background:#fff")
  })
}

# --------------------------------------------------------------------------- #
# Run a script natively in this session, capturing console + messages
# --------------------------------------------------------------------------- #
run_script <- function(path) {
  buf <- textConnection("captured", "w", local = TRUE)
  sink(buf); sink(buf, type = "message")
  ok <- tryCatch({
    sys.source(path, envir = new.env(parent = globalenv()))
    TRUE
  }, error = function(e) { message("ERROR: ", conditionMessage(e)); FALSE })
  sink(type = "message"); sink(); close(buf)
  list(ok = ok, log = get("captured"))
}

# =========================================================================== #
# UI
# =========================================================================== #
css <- "
body{background:#0f1419;color:#e7edf3}
.wrap{padding:6px 4px}
h4.grp{font-size:12px;text-transform:uppercase;letter-spacing:.06em;color:#8ea3b5;margin:14px 0 6px}
.help{font-size:11px;color:#8ea3b5;margin:-6px 0 4px}
.aff{font-size:10px;color:#7d93a6;margin:-2px 0 8px}
.card{background:#171e26;border:1px solid #2b3947;border-radius:12px;margin-bottom:16px;overflow:hidden}
.card.stale{outline:1px solid #f0a53d}
.card .hd{display:flex;justify-content:space-between;align-items:center;padding:9px 12px;border-bottom:1px solid #2b3947}
.card .hd .t{font-weight:650;font-size:13px}
.card .hd .sc{font-size:10px;color:#8ea3b5}
.card .bd{padding:10px;display:flex;flex-wrap:wrap;gap:6px;justify-content:center;align-items:center;min-height:110px}
.empty{color:#8ea3b5;font-size:12px;padding:22px}
.badge{font-size:11px;padding:2px 8px;border-radius:20px;border:1px solid #2b3947;color:#8ea3b5}
.badge.stale{color:#f0a53d;border-color:#f0a53d}
.badge.ok{color:#4cc38a;border-color:#4cc38a}
.sidebar{position:sticky;top:10px}
pre{background:#0a0e12;color:#b8c7d6;border:1px solid #2b3947;border-radius:8px;max-height:320px;overflow:auto;font-size:11px}
.btn-primary{background:#4aa3df;border-color:#4aa3df;color:#04121d;font-weight:650}
.form-group{margin-bottom:10px}
.irs--shiny .irs-bar,.irs--shiny .irs-single{background:#4aa3df}
label{color:#cfe0ee}
"

control_ui <- function(p) {
  aff <- paste(gsub("_", " ", gsub("fig", "Fig ", p$affects)), collapse = ", ")
  ctrl <- if (isTRUE(!is.null(p$decimals) && p$decimals >= 4)) {
    numericInput(p$id, p$label, value = NA, min = p$min, max = p$max, step = p$step, width = "100%")
  } else {
    sliderInput(p$id, p$label, min = p$min, max = p$max, value = p$min, step = p$step, width = "100%")
  }
  tagList(ctrl, tags$div(class = "help", p$help), tags$div(class = "aff", paste0("affects: ", aff)))
}

groups <- unique(vapply(SCHEMA, function(p) p$group, character(1)))

sidebar_controls <- tagList(
  lapply(groups, function(g) {
    tagList(tags$h4(class = "grp", g),
            lapply(Filter(function(p) p$group == g, SCHEMA), control_ui))
  })
)

ui <- fluidPage(
  tags$head(tags$style(HTML(css))),
  titlePanel(tags$div(style = "font-size:17px",
    "Shifting Habitat Mosaics II - Parameter Explorer",
    tags$span(id = "rbadge", class = "badge ok", style = "margin-left:12px", "native R / Shiny"))),
  sidebarLayout(
    sidebarPanel(width = 4, class = "sidebar",
      sidebar_controls,
      tags$hr(),
      actionButton("regen_affected", "Regenerate affected figures", class = "btn-primary", width = "100%"),
      tags$div(style = "height:8px"),
      actionButton("regen_all", "Regenerate all six", width = "100%"),
      tags$div(style = "height:8px"),
      actionButton("reset", "Reset to file defaults", width = "100%"),
      tags$p(class = "help", style = "margin-top:12px",
        "Map-driven figures (1, 3, 4) re-run the full assignment computation and can take
         several minutes. Fig 2 depends only on the contour threshold and is quicker.")
    ),
    mainPanel(width = 8,
      uiOutput("figure_grid"),
      tags$h4(class = "grp", "R console"),
      verbatimTextOutput("log")
    )
  )
)

# =========================================================================== #
# Server
# =========================================================================== #
server <- function(input, output, session) {

  applied  <- reactiveVal(read_params())   # last-applied (on-disk) values
  refresh  <- reactiveVal(0)               # bump to re-scan figure files
  logval   <- reactiveVal("Ready. Change parameters, then regenerate.")
  busy     <- reactiveVal(FALSE)

  # initialise inputs from params.R
  observe({
    vals <- isolate(applied())
    for (p in SCHEMA) {
      v <- vals[[p$id]]; if (is.null(v) || is.na(v)) next
      if (isTRUE(!is.null(p$decimals) && p$decimals >= 4))
        updateNumericInput(session, p$id, value = v)
      else
        updateSliderInput(session, p$id, value = v)
    }
  })

  current_values <- reactive({
    setNames(lapply(SCHEMA, function(p) {
      v <- input[[p$id]]; if (is.null(v)) applied()[[p$id]] else v
    }), names(SCHEMA))
  })

  changed_ids <- reactive({
    cur <- current_values(); ap <- applied()
    Filter(function(id) {
      a <- ap[[id]]; c <- cur[[id]]
      !is.null(a) && !is.null(c) && !is.na(a) && !is.na(c) && abs(a - c) > 1e-12
    }, names(SCHEMA))
  })

  affected_figs <- reactive({
    unique(unlist(lapply(changed_ids(), function(id) schema_by_id(id)$affects)))
  })

  # ----- figure grid -------------------------------------------------------- #
  output$figure_grid <- renderUI({
    refresh()
    aff <- affected_figs()
    cval <- current_values()[["contour_filt_thresh"]]
    rows <- lapply(FIGURE_ORDER, function(fkey) {
      fdef <- FIGURES[[fkey]]
      entries <- figure_entries(fkey, cval)
      stale <- fkey %in% aff
      column(6,
        tags$div(class = paste("card", if (stale) "stale" else ""),
          tags$div(class = "hd",
            tags$div(tags$div(class = "t", fdef$title),
                     tags$div(class = "sc", paste(fdef$scripts, collapse = " -> "))),
            tags$div(style = "display:flex;gap:6px;align-items:center",
              if (stale) tags$span(class = "badge stale", "stale"),
              actionButton(paste0("run_", fkey), "run",
                           class = "btn-xs", style = "padding:3px 9px;font-size:12px"))),
          tags$div(class = "bd", img_tags(entries, gallery = length(entries) > 1)))
      )
    })
    do.call(fluidRow, rows)
  })

  output$log <- renderText(logval())

  # ----- run engine --------------------------------------------------------- #
  do_run <- function(targets) {
    if (busy()) return(invisible())
    if (!length(targets)) { showNotification("No changes to regenerate.", type = "message"); return(invisible()) }
    busy(TRUE); on.exit(busy(FALSE), add = TRUE)

    backup_params()
    write_params(current_values())

    scripts <- scripts_for_targets(targets)
    acc <- c(sprintf("Targets: %s", paste(targets, collapse = ", ")),
             sprintf("Scripts: %s", paste(scripts, collapse = " -> ")), "")
    logval(paste(acc, collapse = "\n"))

    ok_all <- TRUE
    withProgress(message = "Regenerating figures", value = 0, {
      n <- length(scripts)
      for (i in seq_along(scripts)) {
        sk <- scripts[[i]]; fname <- SCRIPT_FILES[[sk]]
        setProgress(value = (i - 1) / n, detail = sprintf("Running %s (%d/%d)", fname, i, n))
        res <- run_script(file.path(ANALYSIS_DIR, fname))
        acc <- c(acc, sprintf("===== %s =====", fname), res$log, "")
        logval(paste(acc, collapse = "\n"))
        if (!isTRUE(res$ok)) {
          ok_all <- FALSE
          showNotification(sprintf("Error in %s - see R console panel.", fname),
                           type = "error", duration = 8)
          break
        }
      }
      setProgress(value = 1)
    })

    if (ok_all) {
      # mark params clean only where ALL their affected figures were regenerated
      cur <- current_values(); ap <- applied()
      for (p in SCHEMA) if (all(p$affects %in% targets)) ap[[p$id]] <- cur[[p$id]]
      applied(ap)
      refresh(refresh() + 1)
      showNotification("Done - figures updated.", type = "message", duration = 4)
    }
    invisible()
  }

  observeEvent(input$regen_affected, do_run(affected_figs()), ignoreInit = TRUE)
  observeEvent(input$regen_all,      do_run(FIGURE_ORDER),     ignoreInit = TRUE)

  # per-figure run buttons
  for (fkey in FIGURE_ORDER) local({
    k <- fkey
    observeEvent(input[[paste0("run_", k)]], do_run(k), ignoreInit = TRUE)
  })

  observeEvent(input$reset, {
    applied(read_params())
    vals <- applied()
    for (p in SCHEMA) {
      v <- vals[[p$id]]; if (is.null(v) || is.na(v)) next
      if (isTRUE(!is.null(p$decimals) && p$decimals >= 4))
        updateNumericInput(session, p$id, value = v)
      else
        updateSliderInput(session, p$id, value = v)
    }
    refresh(refresh() + 1)
    showNotification("Reset to values in params.R", type = "message", duration = 3)
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
