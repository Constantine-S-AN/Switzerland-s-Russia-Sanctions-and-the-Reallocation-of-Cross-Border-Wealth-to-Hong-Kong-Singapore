# Switzerland sanctions → wealth reallocation (CH ↘ vs HK/SG ↗)
# Ready-to-run R scaffold for VSCode (SCM + Event-Study DiD + Figures)
# -------------------------------------------------------------------
# HOW TO USE
# 1) Copy this file to the root of your repo:
#    https://github.com/Constantine-S-AN/Switzerland-s-Russia-Sanctions-and-the-Reallocation-of-Cross-Border-Wealth-to-Hong-Kong-Singapore
# 2) Open in VSCode → Run chunks/sections (Ctrl+Enter) or source the file.
# 3) Edit the file paths in the CONFIG block so they point to your CSVs.
#    If a file is missing, the script will skip that input and continue (you'll get a message).
# 4) Outputs will be written to ./out/ as PNGs and CSVs.
# -------------------------------------------------------------------

# -------------------------
# PACKAGES (auto-install)
# -------------------------
needed <- c(
  "tidyverse", "lubridate", "zoo", "scales",
  "readr", "glue", "fs",
  "Synth",     # classic synthetic control
  "synthdid",  # alternative SCM (matrix-based)
  "fixest",    # fast DiD / event study
  "broom"
)

for (pkg in needed) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(tidyverse)
library(lubridate)
library(zoo)
library(scales)
library(readr)
library(glue)
library(fs)
library(Synth)
library(synthdid)
library(fixest)
library(broom)

# -------------------------
# CONFIG: paths & settings
# -------------------------
# *** Edit these to match your repo file names ***
# Expectation for BIS quarterly CSVs: a tidy file with at least columns like either
#   (A) TIME_PERIOD (e.g., 2015-Q1) and OBS_VALUE, or
#   (B) quarter (YYYY-QX) and value
# If your column names differ, tweak read_series() below accordingly.

paths <- list(
  # BIS quarterly: cross-border liabilities to non-banks by reporting center
  # Main trio
  CH = "data/bis_ch_quarterly.csv",
  HK = "data/bis_hk_quarterly.csv",
  SG = "data/bis_sg_quarterly.csv",
  # Donors (add or remove as available in your repo)
  GB = "data/bis_uk_quarterly.csv",
  LU = "data/bis_lu_quarterly.csv",
  NL = "data/bis_nl_quarterly.csv",
  IE = "data/bis_ie_quarterly.csv",
  AE = "data/bis_uae_quarterly.csv",  # optional

  # Controls (monthly) — optional; if present, they will be aggregated to quarterly means
  EXSZUS = "data/fx_chf_usd_monthly.csv",  # FRED EXSZUS or equivalent
  EXHKUS = "data/fx_hkd_usd_monthly.csv",  # FRED EXHKUS
  EXSIUS = "data/fx_sgd_usd_monthly.csv",  # FRED EXSIUS
  VIX     = "data/vix_daily.csv"            # FRED VIXCLS (Date, Value)
)

# Study window
q_start <- as.yearqtr("2015 Q1")
q_end   <- as.yearqtr("2025 Q2")

# Treatment quarter for Switzerland
q_treat <- as.yearqtr("2022 Q1")

# Index base for plots
index_base <- as.yearqtr("2019 Q4")

# Output dir
out_dir <- "out"
dir_create(out_dir)

# -------------------------
# HELPERS
# -------------------------
message_if_missing <- function(path) {
  if (!file_exists(path)) message(glue("[warn] Missing file: {path} — skipping."))
}

parse_quarter <- function(x) {
  # Accepts formats like "2015-Q1", "2015Q1", "2015 Q1", or full dates → converts to yearqtr
  x <- trimws(as.character(x))
  x <- toupper(gsub("[./]", "-", x))
  x <- gsub("\\s+", "-", x)

  # Normalize common forms to "YYYY Qn"
  x <- gsub("^(\\d{4})-?Q([1-4])$", "\\1 Q\\2", x, perl = TRUE)
  x <- gsub("^(\\d{4})-([1-4])$", "\\1 Q\\2", x, perl = TRUE)

  out <- suppressWarnings(as.yearqtr(x))

  # Fallback: if still NA and looks like a date, convert via Date → yearqtr
  needs_date <- is.na(out) & grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  if (any(needs_date)) {
    out[needs_date] <- suppressWarnings(as.yearqtr(as.Date(x[needs_date])))
  }
  out
}

read_series <- function(path, id) {
  if (!file_exists(path)) return(NULL)
  df <- suppressMessages(read_csv(path, show_col_types = FALSE))

  # Try to detect column names
  if (all(c("TIME_PERIOD", "OBS_VALUE") %in% names(df))) {
    out <- df %>%
      transmute(id = id,
                quarter = parse_quarter(TIME_PERIOD),
                value = as.numeric(OBS_VALUE))
  } else if (all(c("quarter", "value") %in% names(df))) {
    out <- df %>%
      transmute(id = id,
                quarter = parse_quarter(quarter),
                value = as.numeric(value))
  } else if (all(c("date", "value") %in% names(df))) {
    out <- df %>%
      transmute(id = id,
                quarter = parse_quarter(date),
                value = as.numeric(value))
  } else {
    stop(glue("Unrecognized column names in {path}. Expect (TIME_PERIOD, OBS_VALUE) or (quarter, value)."))
  }
  out %>% filter(!is.na(quarter))
}

load_panel <- function(paths_list, ids_needed) {
  series_list <- list()
  for (cc in ids_needed) {
    p <- paths_list[[cc]]
    if (is.null(p)) next
    message_if_missing(p)
    s <- tryCatch(read_series(p, cc), error = function(e) { message("[warn] ", e$message); NULL })
    if (!is.null(s)) series_list[[cc]] <- s
  }
  bind_rows(series_list)
}

# Aggregate monthly controls to quarter means
read_monthly <- function(path, name) {
  if (!file_exists(path)) return(NULL)
  df <- suppressMessages(read_csv(path, show_col_types = FALSE))

  # Heuristics for common column names
  date_col <- intersect(names(df), c("DATE", "Date", "date"))[1]
  val_col  <- setdiff(names(df), c("DATE", "Date", "date"))[1]
  if (is.na(date_col) || is.na(val_col)) stop(glue("Cannot detect date/value columns in {path}"))

  df %>%
    transmute(
      quarter = as.yearqtr(as.Date(.data[[date_col]])),
      !!name := as.numeric(.data[[val_col]])
    ) %>%
    group_by(quarter) %>%
    summarise(!!name := mean(.data[[name]], na.rm = TRUE), .groups = "drop")
}

# Index helper (base = 100 at given quarter)
index_series <- function(df, base_quarter) {
  base_val <- df %>% filter(quarter == base_quarter) %>% summarise(v = mean(value, na.rm = TRUE)) %>% pull(v)
  df %>% mutate(index = 100 * value / base_val)
}

# -------------------------
# LOAD DATA
# -------------------------
ids_main   <- c("CH", "HK", "SG")
ids_donors <- c("GB", "NL", "IE") # donor pool trimmed to balanced, positive series

main_panel <- load_panel(paths, ids_main)
donor_panel <- load_panel(paths, ids_donors)

panel <- bind_rows(main_panel, donor_panel) %>%
  filter(quarter >= q_start, quarter <= q_end) %>%
  filter(value > 0) %>%
  arrange(id, quarter)

# Basic check
message(glue("Loaded rows: main={nrow(main_panel)}, donors={nrow(donor_panel)}"))
if (nrow(panel) == 0) stop("No data loaded. Check your paths.* in the CONFIG block.")

# Trim to overlapping quarters across units
complete_q <- panel %>% count(id) %>% pull(n) %>% min()
common_quarters <- panel %>% group_by(id) %>% slice_tail(n = complete_q) %>% ungroup() %>% pull(quarter) %>% unique() %>% sort()

panel <- panel %>% filter(quarter %in% common_quarters)

# Drop any units that still have gaps in the retained window
panel <- panel %>% group_by(id) %>% filter(!any(is.na(value))) %>% ungroup()

# Add index (2019Q4=100) and logs
panel <- panel %>% group_by(id) %>% arrange(quarter, .by_group = TRUE) %>%
  mutate(
    value = as.numeric(value),
    log_value = log(value),
    .keep = "all"
  ) %>% ungroup()

panel_indexed <- panel %>% group_by(id) %>% group_modify(~ index_series(.x, index_base)) %>% ungroup()

# Controls (optional)
fx_chf <- read_monthly(paths$EXSZUS, "EXSZUS")
fx_hkd <- read_monthly(paths$EXHKUS, "EXHKUS")
fx_sgd <- read_monthly(paths$EXSIUS, "EXSIUS")
vix_df <- {
  if (file_exists(paths$VIX)) {
    df <- suppressMessages(read_csv(paths$VIX, show_col_types = FALSE))
    date_col <- intersect(names(df), c("DATE", "Date", "date"))[1]
    val_col  <- setdiff(names(df), c("DATE", "Date", "date"))[1]
    df %>%
      transmute(quarter = as.yearqtr(as.Date(.data[[date_col]])), VIX = as.numeric(.data[[val_col]])) %>%
      group_by(quarter) %>% summarise(VIX = mean(VIX, na.rm = TRUE), .groups = "drop")
  } else NULL
}

ctrl_list <- list(fx_chf, fx_hkd, fx_sgd, vix_df) %>% purrr::discard(is.null)
controls_q <- if (length(ctrl_list) > 0) {
  purrr::reduce(ctrl_list, full_join, by = "quarter")
} else {
  tibble(quarter = sort(unique(panel$quarter)))
}

# -------------------------
# FIGURE 1: Levels/index for CH/HK/SG
# -------------------------
fig1 <- panel_indexed %>%
  filter(id %in% ids_main) %>%
  ggplot(aes(x = as.Date(quarter), y = index, color = id)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = as.Date(q_treat), linetype = "dashed") +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Cross-border liabilities to non-banks (BIS LBS, index = 100 at 2019Q4)",
       subtitle = "Vertical line = 2022Q1 (Switzerland adopts EU Russia sanctions)",
       x = NULL, y = "Index (2019Q4=100)", color = "Center") +
  theme_minimal(base_size = 12)

ggsave(file.path(out_dir, "fig1_levels_index_CH_HK_SG.png"), fig1, width = 9, height = 5.2, dpi = 200)

# -------------------------
# SYNTHETIC CONTROL via synthdid (primary path)
# -------------------------
# Build a wide matrix Y: rows = units (CH + donors), cols = quarters, values = log_value
units_for_ch <- c("CH", intersect(ids_donors, unique(panel$id)))

ch_mat <- panel %>% filter(id %in% units_for_ch) %>%
  select(id, quarter, log_value) %>%
  pivot_wider(names_from = quarter, values_from = log_value) %>%
  arrange(factor(id, levels = units_for_ch))

# Ensure no missing rows
if (nrow(ch_mat) < 2) stop("Need at least Switzerland + one donor with overlapping data.")

Y <- as.matrix(ch_mat[,-1])
rownames(Y) <- ch_mat$id

# Identify pre/post split columns
all_q <- as.yearqtr(colnames(ch_mat)[-1])
T0 <- sum(all_q < q_treat)
N0 <- nrow(Y) - 1  # controls

if (T0 <= 3) warning("Pre-period is short; SCM fit may be weak.")

# Estimate synthdid for Switzerland treated at q_treat
set.seed(123)
ch_sd <- synthdid_estimate(Y, N0 = N0, T0 = T0)

# Summaries (CI helper not available in this synthdid build)
ch_tau <- tryCatch(as.numeric(ch_sd), error = function(e) { message("[warn] coef() failed: ", e$message); NA_real_ })
if (!is.na(ch_tau)) print(glue("synthdid ATT (post-mean log gap): {round(ch_tau, 4)}"))

# Plot treated vs synthetic path (skip silently if plotting fails)
tryCatch({
  png(file.path(out_dir, "fig2_synthdid_paths_CH.png"), width = 900, height = 520)
  plot(ch_sd)
  invisible(dev.off())
}, error = function(e) message("[warn] synthdid plot failed: ", e$message))

# Plot post-treatment gaps with placebo ribbons (robust version) + permutation p-val + RMSPE + weights
units_all <- rownames(Y)

placebo_df <- purrr::map_dfr(units_all, function(u){
  rot <- match(u, units_all)
  Y_rot <- Y[c(rot, setdiff(seq_len(nrow(Y)), rot)), , drop = FALSE]
  N0_rot <- nrow(Y_rot) - 1
  est <- tryCatch(synthdid_estimate(Y_rot, N0 = N0_rot, T0 = T0), error = function(e) NULL)
  tibble(unit = u, att = tryCatch(as.numeric(est), error = function(e) NA_real_))
}) %>% tidyr::drop_na(att)

readr::write_csv(placebo_df, file.path(out_dir, "synthdid_placebo_att.csv"))

# Permutation p-value (two-sided)
tau_ch <- tryCatch(as.numeric(ch_sd), error = function(e) NA_real_)
perm_p <- if (!is.na(tau_ch) && nrow(placebo_df) > 0) mean(abs(placebo_df$att) >= abs(tau_ch)) else NA_real_
readr::write_csv(tibble(tau_ch = tau_ch, perm_p = perm_p), file.path(out_dir, "synthdid_perm_pval.csv"))
if (!is.na(perm_p)) message(glue("Permutation p-value = {round(perm_p, 3)} (|tau_CH| vs placebos)"))

fig3 <- placebo_df %>%
  mutate(is_CH = (unit == "CH")) %>%
  ggplot(aes(x = reorder(unit, att), y = att, fill = is_CH)) +
  geom_col() +
  coord_flip() +
  guides(fill = "none") +
  labs(title = "Placebo ATT distribution (log scale)", y = "Post-mean ATT (log)", x = NULL,
       subtitle = glue("CH highlighted; N_placebo = {nrow(placebo_df)}; perm p = {round(perm_p,3)}")) +
  theme_minimal(base_size = 12)

ggsave(file.path(out_dir, "fig3_placebo_ATT.png"), fig3, width = 8, height = 6, dpi = 200)

# Donor and time weights + RMSPE ratio
wt <- tryCatch(synthdid::weights(ch_sd), error = function(e) NULL)
omega <- if (!is.null(wt)) wt$omega else NULL
lambda <- if (!is.null(wt)) wt$lambda else NULL
if (!is.null(omega)) readr::write_csv(tibble(donor = rownames(Y)[-1], weight = as.numeric(omega)), file.path(out_dir, "synthdid_donor_weights.csv"))
if (!is.null(lambda)) readr::write_csv(tibble(quarter = as.character(all_q), weight = as.numeric(lambda)), file.path(out_dir, "synthdid_time_weights.csv"))

Y_treat <- as.numeric(Y[1, ])
Y_synth <- if (!is.null(omega)) as.numeric(drop(omega %*% Y[-1, ])) else rep(NA_real_, ncol(Y))
pre_idx  <- 1:T0
post_idx <- (T0 + 1):ncol(Y)
rmspe_pre  <- sqrt(mean((Y_treat[pre_idx]  - Y_synth[pre_idx])^2, na.rm = TRUE))
rmspe_post <- sqrt(mean((Y_treat[post_idx] - Y_synth[post_idx])^2, na.rm = TRUE))
rmspe_ratio <- rmspe_post / rmspe_pre
readr::write_csv(tibble(rmspe_pre, rmspe_post, rmspe_ratio), file.path(out_dir, "synthdid_rmspe.csv"))

# Leave-one-out donors (robustness)
loo <- purrr::map_dfr(2:nrow(Y), function(i){
  Y_sub <- Y[c(1, setdiff(2:nrow(Y), i)), , drop = FALSE]
  est <- tryCatch(synthdid_estimate(Y_sub, N0 = nrow(Y_sub) - 1, T0 = T0), error = function(e) NULL)
  tibble(dropped = rownames(Y)[i], att = tryCatch(as.numeric(est), error = function(e) NA_real_))
}) %>% tidyr::drop_na(att)
readr::write_csv(loo, file.path(out_dir, "synthdid_leave_one_out.csv"))

fig3b <- loo %>% ggplot(aes(x = reorder(dropped, att), y = att)) +
  geom_point() + coord_flip() +
  labs(title = "Leave-one-out ATT (drop each donor)", x = "Dropped donor", y = "ATT (log)") +
  theme_minimal(base_size = 12)

ggsave(file.path(out_dir, "fig3b_synthdid_leave_one_out.png"), fig3b, width = 8, height = 6, dpi = 200)

# In-time placebo (fake treatment at 2019Q1)
pretend_q <- as.yearqtr("2019 Q1")
T0_ip <- sum(all_q < pretend_q)
if (T0_ip > 3 && T0_ip < ncol(Y) - 2) {
  ch_sd_ip <- synthdid_estimate(Y, N0 = N0, T0 = T0_ip)
  png(file.path(out_dir, "fig2b_synthdid_in_time_placebo_CH.png"), width = 900, height = 520)
  tryCatch({
    plot(ch_sd_ip)
    abline(v = T0_ip + 0.5, lty = 2)
  }, error = function(e) message("[warn] in-time placebo plot failed: ", e$message))
  invisible(dev.off())
}

# -------------------------
# CLASSIC Synth (optional, with simple predictors = pre means/slopes)
# -------------------------
# Build a clean panel for Synth::dataprep (Switzerland + donors)
synth_ok <- tryCatch({
  panel_synth <- panel %>% filter(id %in% units_for_ch) %>%
    mutate(
      unit_id = as.numeric(factor(id, levels = units_for_ch)),
      time_id = as.numeric(factor(quarter, levels = sort(unique(quarter))))
    )

  pre_quarters  <- sort(unique(panel_synth$quarter[panel_synth$quarter < q_treat]))
  plot_quarters <- sort(unique(panel_synth$quarter))

  # Predictors: pre-period averages and pre-period linear trend of log_value
  pred_df <- panel_synth %>%
    group_by(id) %>%
    arrange(time_id, .by_group = TRUE) %>%
    mutate(trend_pre = if_else(quarter < q_treat, row_number(), NA_integer_)) %>%
    ungroup()

  prep <- dataprep(
    foo = pred_df,
    predictors = c("log_value"),
    predictors.op = c("mean"),
    dependent = "log_value",
    unit.variable = "unit_id",
    time.variable = "time_id",
    treatment.identifier = 1,                # CH is first in units_for_ch
    controls.identifier = setdiff(unique(pred_df$unit_id), 1),
    time.predictors.prior = pred_df %>% filter(quarter %in% pre_quarters) %>% pull(time_id) %>% unique(),
    time.optimize.ssr   = pred_df %>% filter(quarter %in% pre_quarters) %>% pull(time_id) %>% unique(),
    time.plot           = pred_df %>% pull(time_id) %>% unique()
  )

  syn <- tryCatch(synth(prep), error = function(e) { message("[Synth] ", e$message); NULL })
  if (!is.null(syn)) {
    png(file.path(out_dir, "fig4_synth_paths_CH.png"), width = 900, height = 520)
    synth.path.plot(syn, prep, Plot.unit.names = FALSE)
    invisible(dev.off())

    png(file.path(out_dir, "fig5_synth_gaps_CH.png"), width = 900, height = 520)
    synth.gap.plot(syn, prep)
    invisible(dev.off())
  }
  TRUE
}, error = function(e) { message("[Synth dataprep] ", e$message); FALSE })

# -------------------------
# EVENT-STUDY DiD (supporting)
# -------------------------
# Controls (FX/VIX) are merged if present.
panel_es <- panel %>%
  left_join(controls_q, by = "quarter") %>%
  mutate(
    treated = (id == "CH"),
    time_id = as.integer(factor(quarter, levels = sort(unique(quarter))))
  )

# Reference period = last pre-treat quarter
ref_time <- max(panel_es$time_id[panel_es$quarter < q_treat])

# Formula with dynamic i(time, treated) and two-way FE
fml <- as.formula("log_value ~ i(time_id, treated, ref = ref_time) | id + time_id")

es <- feols(fml, data = panel_es, cluster = ~id)

# Tidy results for plotting (clean extractor)
es_tidy <- broom::tidy(es) %>%
  dplyr::filter(stringr::str_detect(term, "^time_id::")) %>%
  dplyr::mutate(
    time_id = suppressWarnings(as.integer(stringr::str_extract(term, "[0-9]+"))),
    event_time = time_id - ref_time
  ) %>%
  tidyr::drop_na(time_id)

fig6 <- es_tidy %>%
  ggplot(aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.2) +
  labs(title = "Event-study for Switzerland (log outcome)", x = "Event time (quarters, 0 = first post)", y = "Estimate (log points)") +
  theme_minimal(base_size = 12)

ggsave(file.path(out_dir, "fig6_event_study_CH.png"), fig6, width = 8.5, height = 5.2, dpi = 200)

# Event-study with donors-only (exclude HK & SG from controls)
panel_es_donors <- panel %>% filter(id %in% c("CH", intersect(ids_donors, unique(id)))) %>%
  mutate(treated = (id == "CH"), time_id = as.integer(factor(quarter)))
ref_time_d <- max(panel_es_donors$time_id[panel_es_donors$quarter < q_treat])
es_d <- feols(log_value ~ i(time_id, treated, ref = ref_time_d) | id + time_id, data = panel_es_donors, cluster = ~id)

es_d_tidy <- broom::tidy(es_d) %>%
  dplyr::filter(stringr::str_detect(term, "^time_id::")) %>%
  dplyr::mutate(time_id = suppressWarnings(as.integer(stringr::str_extract(term, "[0-9]+"))),
                event_time = time_id - ref_time_d) %>%
  tidyr::drop_na(time_id)

fig6b <- es_d_tidy %>%
  ggplot(aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.2) +
  labs(title = "Event-study (donors-only controls)", x = "Event time (quarters)", y = "Estimate (log)") +
  theme_minimal(base_size = 12)

ggsave(file.path(out_dir, "fig6b_event_study_CH_donors_only.png"), fig6b, width = 8.5, height = 5.2, dpi = 200)

# -------------------------
# OPTIONAL: HK and SG as "treated" in separate runs (positive leg of H1)
# -------------------------
estimate_event_study <- function(panel_df, treated_id, q0) {
  df <- panel_df %>%
    mutate(treated = (id == treated_id), time_id = as.integer(factor(quarter)))
  ref <- max(df$time_id[df$quarter < q0])
  model <- feols(log_value ~ i(time_id, treated, ref = ref) | id + time_id, data = df, cluster = ~id)
  broom::tidy(model) %>%
    filter(str_detect(term, "^time_id::")) %>%
    mutate(time_id = as.integer(str_extract(term, "\\d+")), event_time = time_id - ref,
           unit = treated_id)
}

hk_es <- tryCatch(estimate_event_study(panel, "HK", q_treat), error = function(e) NULL)
sg_es <- tryCatch(estimate_event_study(panel, "SG", q_treat), error = function(e) NULL)

es_both <- bind_rows(hk_es, sg_es)
if (nrow(es_both) > 0) {
  fig7 <- es_both %>%
    ggplot(aes(x = event_time, y = estimate, color = unit)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.2) +
    labs(title = "Event-study (HK & SG as separate treated runs)", x = "Event time", y = "Estimate (log)", color = "Unit") +
    theme_minimal(base_size = 12)
  ggsave(file.path(out_dir, "fig7_event_study_HK_SG.png"), fig7, width = 9, height = 5.2, dpi = 200)
}

# -------------------------
# SAVE CLEAN PANEL for reproducibility
# -------------------------
write_csv(panel, file.path(out_dir, "panel_quarterly_CH_HK_SG_and_donors.csv"))
write_csv(panel_indexed, file.path(out_dir, "panel_quarterly_indexed.csv"))

message("\nDone. Check the ./out/ folder for figures and CSVs.\n")
