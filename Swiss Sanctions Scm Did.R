############################################################
# Switzerland sanctions → wealth reallocation (CH ↘ vs HK/SG ↗)
# Synthetic control + placebo + DiD event-study
# Data paths assume the CSVs already in ./data (see paths below)
############################################################

###########################
# 0. Packages & options
###########################
options(repos = c(CRAN = "https://cloud.r-project.org"))

needed <- c(
  "tidyverse", "lubridate", "zoo", "scales",
  "readr", "glue", "fs",
  "Synth", "synthdid", "fixest", "broom"
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
  LU = "data/Luxembourg——liability.csv",
  SA = "data/Saudi_Arabia_cross_lia.csv",

  # FX controls (monthly)
  EXSZUS = "Raw_data/Exchange rate/Swiss_Francs_to_One_USd.csv",
  EXHKUS = "Raw_data/Exchange rate/HK_Dollars_to_One_USDollar.csv",
  EXSIUS = "Raw_data/Exchange rate/Singapore_Dollars_to_One_USDollar.csv"
)

ids_main   <- c("CH", "HK", "SG", "SA")         # include Saudi as a main unit we track
ids_donors <- c("GB", "NL", "IE", "LU")        # donors for Switzerland; LU added

q_start <- as.yearqtr("2015 Q1")
q_end   <- as.yearqtr("2025 Q2")
q_treat <- as.yearqtr("2022 Q2")
index_base <- as.yearqtr("2019 Q4")

out_dir <- "out"
dir_create(out_dir)

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

index_series <- function(df, base_quarter) {
  base_val <- df %>% filter(quarter == base_quarter) %>% summarise(v = mean(value, na.rm = TRUE)) %>% pull(v)
  df %>% mutate(index = 100 * value / base_val)
}

# Aggregate monthly to quarterly means for controls
read_monthly <- function(path, name) {
  if (!file_exists(path)) return(NULL)
  df <- suppressMessages(read_csv(path, show_col_types = FALSE))
  date_col <- intersect(names(df), c("DATE", "Date", "date", "observation_date"))[1]
  val_col  <- setdiff(names(df), c("DATE", "Date", "date", "observation_date"))[1]
  if (is.na(date_col) || is.na(val_col)) stop(glue("Cannot detect date/value columns in {path}"))

  df %>%
    transmute(
      quarter = as.yearqtr(as.Date(.data[[date_col]])),
      !!name := as.numeric(.data[[val_col]])
    ) %>%
    group_by(quarter) %>%
    summarise(!!name := mean(.data[[name]], na.rm = TRUE), .groups = "drop")
}

###########################
# 3. Load data
###########################
main_panel  <- load_panel(paths, ids_main)
donor_panel <- load_panel(paths, ids_donors)

panel <- bind_rows(main_panel, donor_panel) %>%
  filter(quarter >= q_start, quarter <= q_end) %>%
  filter(value > 0) %>%
  arrange(id, quarter)

message(glue("Loaded rows: main={nrow(main_panel)}, donors={nrow(donor_panel)}"))
if (nrow(panel) == 0) stop("No data loaded. Check paths.")

complete_q <- panel %>% count(id) %>% pull(n) %>% min()
common_quarters <- panel %>% group_by(id) %>% slice_tail(n = complete_q) %>% ungroup() %>% pull(quarter) %>% unique() %>% sort()
panel <- panel %>% filter(quarter %in% common_quarters)
panel <- panel %>% group_by(id) %>% filter(!any(is.na(value))) %>% ungroup()

panel <- panel %>% group_by(id) %>% arrange(quarter, .by_group = TRUE) %>%
  mutate(value = as.numeric(value), log_value = log(value), .keep = "all") %>% ungroup()

panel_indexed <- panel %>% group_by(id) %>% group_modify(~ index_series(.x, index_base)) %>% ungroup()

fx_chf <- read_monthly(paths$EXSZUS, "EXSZUS")
fx_hkd <- read_monthly(paths$EXHKUS, "EXHKUS")
fx_sgd <- read_monthly(paths$EXSIUS, "EXSIUS")

ctrl_list <- list(fx_chf, fx_hkd, fx_sgd) %>% purrr::discard(is.null)
controls_q <- if (length(ctrl_list) > 0) {
  purrr::reduce(ctrl_list, full_join, by = "quarter")
} else {
  tibble(quarter = sort(unique(panel$quarter)))
}

###########################
# 4. Figure: levels/index
###########################
fig1 <- panel_indexed %>%
  filter(id %in% ids_main) %>%
  ggplot(aes(x = as.Date(quarter), y = index, color = id)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = as.Date(q_treat), linetype = "dashed") +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Cross-border liabilities to non-banks (index = 2019Q4=100)",
       subtitle = "Vertical line = 2022Q1 (Switzerland adopts EU Russia sanctions)",
       x = NULL, y = "Index (2019Q4=100)", color = "Center") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "fig1_levels_index_CH_HK_SG.png"), fig1, width = 9, height = 5.2, dpi = 200)

###########################
# 4b. Triad shares: CH vs HK+SG
###########################

triad <- panel %>%
  filter(id %in% c("CH", "HK", "SG")) %>%
  group_by(quarter) %>%
  mutate(
    tri_total = sum(value, na.rm = TRUE),
    share_tri = value / tri_total
  ) %>%
  ungroup()

triad_agg <- triad %>%
  mutate(
    group = case_when(
      id == "CH"              ~ "CH",
      id %in% c("HK", "SG")   ~ "HK+SG",
      TRUE                    ~ NA_character_
    )
  ) %>%
  filter(!is.na(group)) %>%
  group_by(group, quarter) %>%
  summarise(
    share_tri = sum(share_tri, na.rm = TRUE),
    .groups   = "drop"
  )

fig_triad <- triad_agg %>%
  ggplot(aes(x = as.Date(as.yearqtr(quarter)), y = share_tri, color = group)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = as.Date(q_treat), linetype = "dashed") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "Share of triad liabilities (CH vs HK+SG)",
    subtitle = "Dashed line = treatment quarter",
    x        = NULL,
    y        = "Share of (CH + HK + SG)",
    color    = "Group"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(out_dir, "fig_triad_share_CH_vs_HKSG.png"),
  fig_triad, width = 9, height = 5.2, dpi = 200
)

###########################
# 4c. Event-study: CH vs HK+SG (share within triad)
###########################

triad_es <- triad_agg %>%
  mutate(
    treated = (group == "CH"),
    time_id = as.integer(factor(quarter, levels = sort(unique(quarter))))
  )

ref_time_tri <- max(triad_es$time_id[triad_es$quarter < q_treat])

es_triad <- feols(
  share_tri ~ i(time_id, treated, ref = ref_time_tri) | group + time_id,
  data    = triad_es,
  cluster = ~ group
)

es_triad_tidy <- broom::tidy(es_triad) %>%
  dplyr::filter(stringr::str_detect(term, "^time_id::")) %>%
  mutate(
    time_id    = suppressWarnings(as.integer(stringr::str_extract(term, "[0-9]+"))),
    event_time = time_id - ref_time_tri
  ) %>%
  tidyr::drop_na(time_id)

fig_es_triad <- es_triad_tidy %>%
  ggplot(aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbar(
    aes(ymin = estimate - 1.96 * std.error,
        ymax = estimate + 1.96 * std.error),
    width = 0.2
  ) +
  labs(
    title = "Event-study: CH vs HK+SG (share within triad)",
    x     = "Event time (quarters)",
    y     = "Difference in triad share (CH − HK+SG)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(out_dir, "fig_es_triad_CH_vs_HKSG.png"),
  fig_es_triad, width = 8.5, height = 5.2, dpi = 200
)

###########################
# 5. synthdid SCM
###########################
units_for_ch <- c(intersect(ids_donors, unique(panel$id)), "CH")  # donors first, CH last

ch_mat <- panel %>%
  filter(id %in% units_for_ch) %>%
  select(id, quarter, log_value) %>%
  pivot_wider(names_from = quarter, values_from = log_value) %>%
  arrange(factor(id, levels = units_for_ch))

if (nrow(ch_mat) < 2) stop("Need at least Switzerland + one donor with overlapping data.")

Y <- as.matrix(ch_mat[, -1])
rownames(Y) <- ch_mat$id

all_q <- as.yearqtr(colnames(ch_mat)[-1])
T0   <- sum(all_q < q_treat)
N0   <- min(length(ids_donors), nrow(Y) - 1)   # donors = first rows, CH = last; safeguard if some donors missing
if (T0 <= 3) warning("Pre-period is short; SCM fit may be weak.")

set.seed(123)
ch_sd <- synthdid_estimate(Y, N0 = N0, T0 = T0)
ch_tau <- tryCatch(as.numeric(ch_sd), error = function(e) { message("[warn] coef() failed: ", e$message); NA_real_ })
if (!is.na(ch_tau)) print(glue("synthdid ATT (post-mean log gap): {round(ch_tau, 4)}"))

tryCatch({
  png(file.path(out_dir, "fig2_synthdid_paths_CH.png"), width = 900, height = 520)
  plot(ch_sd)
  invisible(dev.off())
}, error = function(e) message("[warn] synthdid plot failed: ", e$message))

placebo_df <- purrr::map_dfr(rownames(Y), function(u) {
  donors_u <- setdiff(rownames(Y), u)

  # controls first, treated last
  Y_rot <- rbind(
    Y[donors_u, , drop = FALSE],
    Y[u, , drop = FALSE]
  )
  N0_rot <- nrow(Y_rot) - 1

  est <- tryCatch(
    synthdid_estimate(Y_rot, N0 = N0_rot, T0 = T0),
    error = function(e) NULL
  )

  tibble(
    unit = u,
    att  = tryCatch(as.numeric(est), error = function(e) NA_real_)
  )
}) %>% drop_na(att)
write_csv(placebo_df, file.path(out_dir, "synthdid_placebo_att.csv"))

tau_ch   <- tryCatch(as.numeric(ch_sd), error = function(e) NA_real_)
placebos <- placebo_df %>% filter(unit != "CH")
perm_p   <- if (!is.na(tau_ch) && nrow(placebos) > 0) mean(abs(placebos$att) >= abs(tau_ch)) else NA_real_
write_csv(tibble(tau_ch = tau_ch, perm_p = perm_p), file.path(out_dir, "synthdid_perm_pval.csv"))
if (!is.na(perm_p)) message(glue("Permutation p-value = {round(perm_p, 3)} (|tau_CH| vs placebos)"))

fig3 <- placebo_df %>%
  mutate(is_CH = (unit == "CH")) %>%
  ggplot(aes(x = reorder(unit, att), y = att, fill = is_CH)) +
  geom_col() +
  coord_flip() +
  guides(fill = "none") +
  labs(title = "Placebo ATT distribution (log scale)", y = "Post-mean ATT (log)", x = NULL,
       subtitle = glue("N_placebo = {nrow(placebo_df)}; perm p = {round(perm_p,3)}")) +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "fig3_placebo_ATT.png"), fig3, width = 8, height = 6, dpi = 200)

wt <- tryCatch(synthdid::weights(ch_sd), error = function(e) NULL)
omega <- if (!is.null(wt)) wt$omega else NULL
lambda <- if (!is.null(wt)) wt$lambda else NULL
if (!is.null(omega)) write_csv(tibble(donor = rownames(Y)[-1], weight = as.numeric(omega)), file.path(out_dir, "synthdid_donor_weights.csv"))
if (!is.null(lambda)) write_csv(tibble(quarter = as.character(all_q), weight = as.numeric(lambda)), file.path(out_dir, "synthdid_time_weights.csv"))

Y_treat <- as.numeric(Y[1, ])
Y_synth <- if (!is.null(omega)) as.numeric(drop(omega %*% Y[-1, ])) else rep(NA_real_, ncol(Y))
pre_idx  <- 1:T0
post_idx <- (T0 + 1):ncol(Y)
rmspe_pre  <- sqrt(mean((Y_treat[pre_idx]  - Y_synth[pre_idx])^2, na.rm = TRUE))
rmspe_post <- sqrt(mean((Y_treat[post_idx] - Y_synth[post_idx])^2, na.rm = TRUE))
write_csv(tibble(rmspe_pre, rmspe_post, rmspe_ratio = rmspe_post / rmspe_pre), file.path(out_dir, "synthdid_rmspe.csv"))

donor_names <- setdiff(rownames(Y), "CH")

loo <- purrr::map_dfr(donor_names, function(dropped) {
  keep_donors <- setdiff(donor_names, dropped)

  Y_sub <- rbind(
    Y[keep_donors, , drop = FALSE],  # controls
    Y["CH", , drop = FALSE]          # treated, last row
  )

  est <- tryCatch(
    synthdid_estimate(Y_sub, N0 = nrow(Y_sub) - 1, T0 = T0),
    error = function(e) NULL
  )

  tibble(
    dropped = dropped,
    att     = tryCatch(as.numeric(est), error = function(e) NA_real_)
  )
}) %>% drop_na(att)
write_csv(loo, file.path(out_dir, "synthdid_leave_one_out.csv"))

fig3b <- loo %>% ggplot(aes(x = reorder(dropped, att), y = att)) +
  geom_point() + coord_flip() +
  labs(title = "Leave-one-out ATT (drop each donor)", x = "Dropped donor", y = "ATT (log)") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "fig3b_synthdid_leave_one_out.png"), fig3b, width = 8, height = 6, dpi = 200)

###########################
# 6. Event-study DiD
###########################
panel_es <- panel %>%
  left_join(controls_q, by = "quarter") %>%
  mutate(
    treated = (id == "CH"),
    time_id = as.integer(factor(quarter, levels = sort(unique(quarter))))
  )

ref_time <- max(panel_es$time_id[panel_es$quarter < q_treat])
es <- feols(log_value ~ i(time_id, treated, ref = ref_time) | id + time_id, data = panel_es, cluster = ~id)

es_tidy <- broom::tidy(es) %>%
  filter(str_detect(term, "^time_id::")) %>%
  mutate(time_id = suppressWarnings(as.integer(str_extract(term, "[0-9]+"))),
         event_time = time_id - ref_time) %>%
  drop_na(time_id)

fig6 <- es_tidy %>%
  ggplot(aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
  labs(title = "Event-study for Switzerland (log outcome)", x = "Event time (quarters, 0 = first post)", y = "Estimate (log points)") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "fig6_event_study_CH.png"), fig6, width = 8.5, height = 5.2, dpi = 200)

panel_es_donors <- panel %>% filter(id %in% c("CH", intersect(ids_donors, unique(id)))) %>%
  mutate(treated = (id == "CH"), time_id = as.integer(factor(quarter)))
ref_time_d <- max(panel_es_donors$time_id[panel_es_donors$quarter < q_treat])
es_d <- feols(log_value ~ i(time_id, treated, ref = ref_time_d) | id + time_id, data = panel_es_donors, cluster = ~id)

es_d_tidy <- broom::tidy(es_d) %>%
  filter(str_detect(term, "^time_id::")) %>%
  mutate(time_id = suppressWarnings(as.integer(str_extract(term, "[0-9]+"))),
         event_time = time_id - ref_time_d) %>%
  drop_na(time_id)

fig6b <- es_d_tidy %>%
  ggplot(aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
  labs(title = "Event-study (donors-only controls)", x = "Event time (quarters)", y = "Estimate (log)") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "fig6b_event_study_CH_donors_only.png"), fig6b, width = 8.5, height = 5.2, dpi = 200)

###########################
# 6b. Event-study: HK+SG share vs donors
###########################

shares <- panel %>%
  group_by(quarter) %>%
  mutate(
    total_value = sum(value, na.rm = TRUE),
    share       = value / total_value
  ) %>%
  ungroup()

shares_es <- shares %>%
  mutate(
    group = case_when(
      id %in% c("HK", "SG") ~ "HKSG",
      id %in% ids_donors    ~ "donor",
      TRUE                  ~ "other"
    )
  ) %>%
  filter(group != "other") %>%
  mutate(
    treated = (group == "HKSG"),
    time_id = as.integer(factor(quarter, levels = sort(unique(quarter))))
  )

ref_time_hksg <- max(shares_es$time_id[shares_es$quarter < q_treat])

es_hksg <- feols(
  share ~ i(time_id, treated, ref = ref_time_hksg) | id + time_id,
  data    = shares_es,
  cluster = ~ id
)

es_hksg_tidy <- broom::tidy(es_hksg) %>%
  filter(str_detect(term, "^time_id::")) %>%
  mutate(
    time_id    = suppressWarnings(as.integer(str_extract(term, "[0-9]+"))),
    event_time = time_id - ref_time_hksg
  ) %>%
  drop_na(time_id)

fig_HKSG <- es_hksg_tidy %>%
  ggplot(aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbar(
    aes(ymin = estimate - 1.96 * std.error,
        ymax = estimate + 1.96 * std.error),
    width = 0.2
  ) +
  labs(
    title = "Event-study: HK+SG share vs donor centres",
    x     = "Event time (quarters)",
    y     = "Change in share of global liabilities"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(out_dir, "fig_event_HKSG_share.png"),
  fig_HKSG, width = 8.5, height = 5.2, dpi = 200
)

###########################
# 6c. Event-study: Saudi Arabia vs donor centres
###########################

sa_es <- panel %>%
  filter(id %in% c("SA", ids_donors)) %>%
  mutate(
    treated = (id == "SA"),
    time_id = as.integer(factor(quarter, levels = sort(unique(quarter))))
  )

if (nrow(sa_es) > 0 && length(unique(sa_es$treated)) > 1) {
  ref_time_sa <- max(sa_es$time_id[sa_es$quarter < q_treat])

  sa_ok <- tryCatch({
    es_sa <- feols(
      log_value ~ i(time_id, treated, ref = ref_time_sa) | id + time_id,
      data    = sa_es,
      cluster = ~ id
    )

    es_sa_tidy <- broom::tidy(es_sa) %>%
      dplyr::filter(stringr::str_detect(term, "^time_id::")) %>%
      mutate(
        time_id    = suppressWarnings(as.integer(stringr::str_extract(term, "[0-9]+"))),
        event_time = time_id - ref_time_sa
      ) %>%
      tidyr::drop_na(time_id)

    fig_sa <- es_sa_tidy %>%
      ggplot(aes(x = event_time, y = estimate)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point() +
      geom_errorbar(
        aes(ymin = estimate - 1.96 * std.error,
            ymax = estimate + 1.96 * std.error),
        width = 0.2
      ) +
      labs(
        title = "Event-study: Saudi Arabia vs donor centres",
        x     = "Event time (quarters)",
        y     = "Estimate (log)"
      ) +
      theme_minimal(base_size = 12)

    ggsave(
      file.path(out_dir, "fig_event_SA_vs_donors.png"),
      fig_sa, width = 8.5, height = 5.2, dpi = 200
    )
    TRUE
  }, error = function(e) { message("[warn] SA event-study skipped: ", e$message); FALSE })
} else {
  message("[warn] SA event-study skipped: insufficient data")
}

###########################
# 7. Save panel
###########################
write_csv(panel, file.path(out_dir, "panel_quarterly_CH_HK_SG_and_donors.csv"))
write_csv(panel_indexed, file.path(out_dir, "panel_quarterly_indexed.csv"))

message("\nDone. Check the ./out/ folder for figures and CSVs.\n")
