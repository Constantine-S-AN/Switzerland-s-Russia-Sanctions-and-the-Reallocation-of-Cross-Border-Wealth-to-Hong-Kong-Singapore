############################################################
# Switzerland sanctions → Synthetic Control (synthdid + Synth)
# Standalone file: SCM only (no DiD)
############################################################

###########################
# 0. Packages & options
###########################
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Prefer local vendored packages (./r_libs) to avoid network installs
local_lib <- file.path(getwd(), "r_libs")
if (dir.exists(local_lib)) {
  .libPaths(c(local_lib, .libPaths()))
}

needed <- c(
  "tidyverse", "lubridate", "zoo", "scales",
  "readr", "glue", "fs",
  "Synth", "synthdid"
)
for (pkg in needed) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package %s is missing. Install it or place it in ./r_libs", pkg))
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

###########################
# 1. Paths & settings
###########################
paths <- list(
  CH = "data/bis_ch_quarterly.csv",
  HK = "data/bis_hk_quarterly.csv",
  SG = "data/bis_sg_quarterly.csv",
  GB = "data/bis_uk_quarterly.csv",
  NL = "data/bis_nl_quarterly.csv",
  IE = "data/bis_ie_quarterly.csv",
  ES = "data/bis_es_quarterly.csv",
  FI = "data/bis_fi_quarterly.csv",
  BE = "data/bis_be_quarterly.csv",
  SE = "data/bis_se_quarterly.csv",
  NO = "data/bis_no_quarterly.csv",
  DK = "data/bis_dk_quarterly.csv",
  IT = "data/bis_it_quarterly.csv"
)

gdp_paths <- list(
  CH = "Raw_data/Raw_data-selected/GDP growth/GDP_Switzerland.csv",
  GB = "Raw_data/Raw_data-selected/GDP growth/GDP_United_Kingdom.csv",
  NL = "Raw_data/Raw_data-selected/GDP growth/GDP_Netherlands.csv",
  IE = "Raw_data/Raw_data-selected/GDP growth/GDP_Ireland.csv",
  ES = "Raw_data/Raw_data-selected/GDP growth/GDP_Spain.csv",
  FI = "Raw_data/Raw_data-selected/GDP growth/GDP_Finland.csv",
  BE = "Raw_data/Raw_data-selected/GDP growth/GDP_Belgium.csv",
  SE = "Raw_data/Raw_data-selected/GDP growth/GDP_Sweden.csv",
  NO = "Raw_data/Raw_data-selected/GDP growth/GDP_Norway.csv",
  DK = "Raw_data/Raw_data-selected/GDP growth/GDP_Denmark.csv",
  IT = "Raw_data/Raw_data-selected/GDP growth/GDP_Italy.csv"
)

ids_main   <- c("CH")
ids_donors <- c("GB", "NL", "IE", "ES", "FI", "BE", "SE", "NO", "DK", "IT")  # HK/SG excluded by design

q_start <- as.yearqtr("2015 Q1")
q_end   <- as.yearqtr("2025 Q2")
q_treat <- as.yearqtr("2022 Q2")
index_base <- as.yearqtr("2019 Q4")

out_dir <- "out"
dir_create(out_dir)
fig_dir <- "fig"
dir_create(fig_dir)

###########################
# 2. Helpers
###########################
message_if_missing <- function(path) {
  if (!file_exists(path)) message(glue("[warn] Missing file: {path} — skipping."))
}

parse_quarter <- function(x) {
  x <- trimws(as.character(x))
  x <- toupper(gsub("[./]", "-", x))
  x <- gsub("\\s+", "-", x)
  x <- gsub("^(\\d{4})-?Q([1-4])$", "\\1 Q\\2", x, perl = TRUE)
  x <- gsub("^(\\d{4})-([1-4])$", "\\1 Q\\2", x, perl = TRUE)
  out <- suppressWarnings(as.yearqtr(x))
  needs_date <- is.na(out) & grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  if (any(needs_date)) out[needs_date] <- suppressWarnings(as.yearqtr(as.Date(x[needs_date])))
  out
}

read_series <- function(path, id) {
  if (!file_exists(path)) return(NULL)
  df <- suppressMessages(read_csv(path, show_col_types = FALSE))
  if (all(c("TIME_PERIOD", "OBS_VALUE") %in% names(df))) {
    out <- df %>% transmute(id = id, quarter = parse_quarter(TIME_PERIOD), value = as.numeric(OBS_VALUE))
  } else if (all(c("quarter", "value") %in% names(df))) {
    out <- df %>% transmute(id = id, quarter = parse_quarter(quarter), value = as.numeric(value))
  } else if (all(c("date", "value") %in% names(df))) {
    out <- df %>% transmute(id = id, quarter = parse_quarter(date), value = as.numeric(value))
  } else {
    stop(glue("Unrecognized columns in {path}. Expect (TIME_PERIOD, OBS_VALUE) or (quarter, value)."))
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

read_gdp_series <- function(path, id) {
  if (!file_exists(path)) return(NULL)
  df <- suppressMessages(read_csv(path, show_col_types = FALSE))
  if (!all(c("year", "GDP_growth") %in% names(df))) {
    stop(glue("Unrecognized GDP columns in {path}. Expect (year, GDP_growth)."))
  }
  df %>% transmute(id = id, year = as.integer(year), gdp_growth = as.numeric(GDP_growth)) %>% filter(!is.na(year))
}

load_gdp <- function(paths_list) {
  out <- list()
  for (cc in names(paths_list)) {
    p <- paths_list[[cc]]
    if (is.null(p)) next
    message_if_missing(p)
    g <- tryCatch(read_gdp_series(p, cc), error = function(e) { message("[warn] ", e$message); NULL })
    if (!is.null(g)) out[[cc]] <- g
  }
  bind_rows(out)
}

read_inflation_series <- function(path, id) {
  if (!file_exists(path)) return(NULL)
  df <- suppressMessages(read_csv(path, show_col_types = FALSE))
  if (!all(c("year", "CPI") %in% names(df))) {
    stop(glue("Unrecognized inflation columns in {path}. Expect (year, CPI)."))
  }
  df %>% transmute(id = id, year = as.integer(year), inflation = as.numeric(CPI)) %>% filter(!is.na(year))
}

load_inflation <- function(paths_list) {
  out <- list()
  for (cc in names(paths_list)) {
    p <- paths_list[[cc]]
    if (is.null(p)) next
    message_if_missing(p)
    inf <- tryCatch(read_inflation_series(p, cc), error = function(e) { message("[warn] ", e$message); NULL })
    if (!is.null(inf)) out[[cc]] <- inf
  }
  bind_rows(out)
}

###########################
# 3. Load data
###########################
main_panel  <- load_panel(paths, ids_main)
donor_panel <- load_panel(paths, ids_donors)

panel <- bind_rows(main_panel, donor_panel) %>%
  group_by(id, quarter) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  filter(quarter >= q_start, quarter <= q_end) %>%
  filter(value > 0) %>%
  arrange(id, quarter) %>%
  group_by(id) %>%
  mutate(value = as.numeric(value), log_value = log(value), .keep = "all") %>%
  ungroup()

if (nrow(panel) == 0) stop("No data loaded. Check paths.")

# Attach GDP growth (annual, repeated within year)
gdp_panel <- load_gdp(gdp_paths)
panel <- panel %>%
  mutate(year = lubridate::year(as.Date(quarter))) %>%
  left_join(gdp_panel, by = c("id", "year")) %>%
  select(-year)

###########################
# 4. synthdid SCM
###########################
gdp_available <- intersect(names(gdp_paths), unique(panel$id))
donor_set <- intersect(ids_donors, gdp_available)
if (length(donor_set) < 2) stop("Need at least two donor countries for SCM.")
units_scm <- c(donor_set, "CH")

panel_scm <- panel %>%
  filter(id %in% units_scm, quarter >= as.yearqtr("2018 Q1")) %>%
  select(id, quarter, log_value, gdp_growth)

# Residualize outcome on GDP growth (pre-treatment) for synthdid
pre_fit <- panel_scm %>%
  filter(quarter < q_treat, !is.na(log_value), !is.na(gdp_growth))
use_cov_synthdid <- nrow(pre_fit) >= 5 && sd(pre_fit$gdp_growth, na.rm = TRUE) > 0
if (use_cov_synthdid) {
  fit_gdp <- lm(log_value ~ gdp_growth, data = pre_fit)
  pred_gdp <- as.numeric(predict(fit_gdp, newdata = panel_scm))
  panel_scm <- panel_scm %>%
    mutate(
      log_value_resid = log_value - pred_gdp,
      log_value_resid = if_else(is.na(log_value_resid), log_value, log_value_resid),
      outcome_synthdid = log_value_resid
    )
} else {
  panel_scm <- panel_scm %>% mutate(outcome_synthdid = log_value)
}
panel_scm <- panel_scm %>% mutate(quarter_chr = as.character(quarter))

scm_mat <- panel_scm %>%
  select(id, quarter_chr, outcome_synthdid) %>%
  pivot_wider(names_from = quarter_chr, values_from = outcome_synthdid) %>%
  arrange(factor(id, levels = units_scm))

Y <- as.matrix(scm_mat[, -1])
rownames(Y) <- scm_mat$id
complete_cols <- colSums(!is.na(Y)) == nrow(Y)
Y <- Y[, complete_cols, drop = FALSE]
col_quarters <- colnames(scm_mat)[-1]
message("[info] synthdid columns (first few): ", paste(head(col_quarters), collapse = ", "))
quarters_scm <- as.yearqtr(col_quarters)[complete_cols]
if (any(is.na(quarters_scm))) {
  keep_cols <- !is.na(quarters_scm)
  Y <- Y[, keep_cols, drop = FALSE]
  quarters_scm <- quarters_scm[keep_cols]
}
T0 <- sum(quarters_scm < q_treat)
if (nrow(Y) < 2 || T0 < 2) {
  message("[err] synthdid matrix dims: rows=", nrow(Y), " cols=", ncol(Y), " T0=", T0, " complete_cols=", sum(complete_cols))
  stop("Insufficient donors or pre-period for SCM.")
}
N0 <- length(donor_set)

set.seed(123)
sd_est <- tryCatch(
  synthdid_estimate(Y, N0 = N0, T0 = T0),
  error = function(e) {
    message("[warn] synthdid failed: ", e$message)
    NULL
  }
)
sd_tau <- if (!is.null(sd_est)) as.numeric(sd_est) else NA_real_
sd_se <- NA_real_
sd_p <- NA_real_
sd_se_method <- NA_character_
sd_placebo_p <- NA_real_
sd_placebo_p_one_sided <- NA_real_
sd_placebo_att <- NULL
if (!is.null(sd_est)) {
  se_methods <- c("jackknife", "placebo")
  for (m in se_methods) {
    se_try <- tryCatch(as.numeric(synthdid_se(sd_est, method = m)), error = function(e) NA_real_)
    if (!is.na(se_try) && is.finite(se_try) && se_try > 0) {
      sd_se <- se_try
      sd_p <- 2 * pnorm(-abs(sd_tau / sd_se))
      sd_se_method <- m
      break
    }
  }
  if (is.na(sd_se)) message("[warn] synthdid_se failed for all methods tried (jackknife, placebo).")

  # Randomization inference via donor-as-treated placebos
  units_all <- rownames(Y)
  placebo_atts <- c()
  for (i in seq_along(units_all)) {
    if (units_all[i] == "CH") next
    Yp <- rbind(Y[-i, , drop = FALSE], Y[i, , drop = FALSE])
    att_i <- tryCatch(
      as.numeric(synthdid_estimate(Yp, N0 = nrow(Yp) - 1, T0 = T0)),
      error = function(e) NA_real_
    )
    placebo_atts <- c(placebo_atts, att_i)
  }
  placebo_atts <- placebo_atts[!is.na(placebo_atts)]
  sd_placebo_att <- placebo_atts
  if (!is.na(sd_tau) && length(placebo_atts) > 0) {
    sd_placebo_p <- (sum(abs(placebo_atts) >= abs(sd_tau)) + 1) / (length(placebo_atts) + 1)
    sd_placebo_p_one_sided <- (sum(placebo_atts <= sd_tau) + 1) / (length(placebo_atts) + 1)
  }

  # Placebo-in-time (pre-treatment fake intervention times)
}

wt_sd <- if (!is.null(sd_est)) attr(sd_est, "weights") else NULL
omega <- if (!is.null(wt_sd)) wt_sd$omega else NULL
lambda <- if (!is.null(wt_sd)) wt_sd$lambda else NULL

Y_treat <- as.numeric(Y[nrow(Y), ])
Y_synth <- if (!is.null(omega)) as.numeric(drop(t(omega) %*% Y[seq_len(length(omega)), , drop = FALSE])) else rep(NA_real_, length(Y_treat))

gap_df <- tibble(
  quarter = quarters_scm,
  CH      = Y_treat,
  synth   = Y_synth,
  gap     = Y_treat - Y_synth
)
write_csv(gap_df, file.path(out_dir, "synthdid_paths_gap.csv"))
write_csv(
  tibble(
    att = sd_tau,
    se = sd_se,
    p_value = sd_p,
    p_value_placebo = sd_placebo_p,
    p_value_placebo_one_sided = sd_placebo_p_one_sided,
    se_method = sd_se_method
  ),
  file.path(out_dir, "synthdid_att.csv")
)
if (!is.null(sd_placebo_att)) {
  write_csv(tibble(placebo_att = sd_placebo_att), file.path(out_dir, "synthdid_placebo_atts.csv"))
}

fig_sd_paths <- gap_df %>%
  select(quarter, CH, synth) %>%
  pivot_longer(-quarter, names_to = "series", values_to = "log_value") %>%
  ggplot(aes(x = as.Date(quarter), y = log_value, color = series)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = as.Date(q_treat), linetype = "dashed") +
  labs(
    title = "Switzerland vs synthetic donors (synthdid)",
    subtitle = "Log cross-border liabilities; donors exclude HK/SG",
    x = NULL, y = "Log liabilities", color = NULL
  ) +
  theme_minimal(base_size = 12)

fig_sd_gap <- gap_df %>%
  drop_na(gap) %>%
  ggplot(aes(x = as.Date(quarter), y = gap)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(color = "firebrick", linewidth = 1) +
  geom_vline(xintercept = as.Date(q_treat), linetype = "dashed") +
  labs(
    title = "Gap: CH minus synthetic (synthdid)",
    x = NULL, y = "Log gap"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(fig_dir, "fig_synthdid_paths_CH.png"), fig_sd_paths, width = 9, height = 5.2, dpi = 200)
ggsave(file.path(fig_dir, "fig_synthdid_gap_CH.png"), fig_sd_gap, width = 9, height = 4.5, dpi = 200)

if (!is.null(omega)) {
  donors_all <- setdiff(rownames(Y), "CH")
  weights_df <- tibble(donor = donors_all, weight = 0)
  if (!is.null(names(omega))) {
    weights_df$weight <- weights_df$donor %>% match(names(omega)) %>% { omega[.] } %>% replace_na(0)
  } else if (length(omega) == length(donors_all)) {
    weights_df$weight <- as.numeric(omega)
  }
  write_csv(weights_df, file.path(out_dir, "synthdid_donor_weights.csv"))
  fig_weights <- weights_df %>%
    ggplot(aes(x = reorder(donor, weight), y = weight)) +
    geom_col(fill = "gray40") +
    coord_flip() +
    labs(title = "synthdid donor weights", x = NULL, y = "Weight") +
    theme_minimal(base_size = 12)
  ggsave(file.path(fig_dir, "fig_synthdid_weights.png"), fig_weights, width = 6, height = 4, dpi = 200)
}

###########################
# 5. Classic Synth (fixed donor set, log outcome)
###########################
synth_df <- panel %>%
  filter(id %in% units_scm, quarter %in% quarters_scm, !is.na(gdp_growth)) %>%
  mutate(
    unit_num = as.numeric(factor(id, levels = units_scm)),
    time_num = as.numeric(factor(quarter, levels = quarters_scm))
  )

pre_t  <- sort(unique(synth_df$time_num[synth_df$quarter < q_treat]))
post_t <- sort(unique(synth_df$time_num[synth_df$quarter >= q_treat]))
treat_num <- max(synth_df$unit_num)
control_nums <- setdiff(unique(synth_df$unit_num), treat_num)

dp <- Synth::dataprep(
  foo = as.data.frame(synth_df),
  predictors = c("log_value", "gdp_growth"),
  predictors.op = "mean",
  special.predictors = list(
    list("log_value", tail(pre_t, 4), "mean"),
    list("log_value", tail(pre_t, 8), "mean"),
    list("gdp_growth", unique(pre_t), "mean")
  ),
  dependent = "log_value",
  unit.variable = "unit_num",
  time.variable = "time_num",
  unit.names.variable = "id",
  treatment.identifier = treat_num,
  controls.identifier = control_nums,
  time.optimize.ssr = pre_t,
  time.predictors.prior = pre_t,
  time.plot = c(pre_t, post_t)
)

sc_res <- tryCatch(Synth::synth(dp), error = function(e) { message("[warn] synth failed: ", e$message); NULL })

if (!is.null(sc_res)) {
  synth_series <- as.numeric(dp$Y0plot %*% sc_res$solution.w)
  actual       <- as.numeric(dp$Y1plot)
  time_vec     <- quarters_scm[c(pre_t, post_t)]

  sc_gap <- tibble(
    quarter = time_vec,
    CH      = actual,
    synth   = synth_series,
    gap     = actual - synth_series
  )
  write_csv(sc_gap, file.path(out_dir, "synth_classic_paths_gap.csv"))

  donor_nums <- if (is.null(names(sc_res$solution.w))) seq_along(sc_res$solution.w) else as.integer(names(sc_res$solution.w))
  weights_tbl <- tibble(
    donor_num = donor_nums,
    weight    = as.numeric(sc_res$solution.w)
  ) %>%
    left_join(tibble(donor_num = control_nums, donor = units_scm[control_nums]), by = "donor_num")
  write_csv(weights_tbl, file.path(out_dir, "synth_classic_weights.csv"))

  fig_classic_paths <- sc_gap %>%
    select(quarter, CH, synth) %>%
    pivot_longer(-quarter, names_to = "series", values_to = "log_value") %>%
    ggplot(aes(x = as.Date(quarter), y = log_value, color = series)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = as.Date(q_treat), linetype = "dashed") +
    labs(
      title = "Classic Synth: CH vs donors (log liabilities)",
      x = NULL, y = "Log liabilities", color = NULL
    ) +
    theme_minimal(base_size = 12)

  fig_classic_gap <- sc_gap %>%
    ggplot(aes(x = as.Date(quarter), y = gap)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line(color = "firebrick", linewidth = 1) +
    geom_vline(xintercept = as.Date(q_treat), linetype = "dashed") +
    labs(title = "Classic Synth gap (CH - synthetic)", x = NULL, y = "Log gap") +
    theme_minimal(base_size = 12)

  ggsave(file.path(fig_dir, "fig_synth_classic_paths.png"), fig_classic_paths, width = 9, height = 5.2, dpi = 200)
  ggsave(file.path(fig_dir, "fig_synth_classic_gap.png"), fig_classic_gap, width = 9, height = 4.5, dpi = 200)
}

###########################
# 6. Save panel
###########################
write_csv(panel, file.path(out_dir, "panel_quarterly_CH_and_donors.csv"))

message("SCM outputs written to ./out and figures to ./fig")
