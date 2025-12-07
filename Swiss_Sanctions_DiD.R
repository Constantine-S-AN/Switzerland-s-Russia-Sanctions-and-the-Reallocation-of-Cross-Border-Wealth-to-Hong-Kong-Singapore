############################################################
# Switzerland sanctions → triad DiD (CH vs HK/SG)
# Standalone file: only DiD/Event-study pieces (no SCM)
############################################################

###########################
# 0. Packages & options
###########################
options(repos = c(CRAN = "https://cloud.r-project.org"))

needed <- c(
  "tidyverse", "lubridate", "zoo", "scales",
  "readr", "glue", "fs", "fixest", "broom"
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
library(fixest)
library(broom)
fixest::setFixest_notes(FALSE)

###########################
# 1. Paths & settings
###########################
paths <- list(
  CH = "data/bis_ch_quarterly.csv",
  HK = "data/bis_hk_quarterly.csv",
  SG = "data/bis_sg_quarterly.csv"
)

ids_main <- c("CH", "HK", "SG")

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

index_series <- function(df, base_quarter) {
  base_val <- df %>% filter(quarter == base_quarter) %>% summarise(v = mean(value, na.rm = TRUE)) %>% pull(v)
  df %>% mutate(index = 100 * value / base_val)
}

###########################
# 3. Load data
###########################
panel <- load_panel(paths, ids_main) %>%
  group_by(id, quarter) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  filter(quarter >= q_start, quarter <= q_end) %>%
  filter(value > 0) %>%
  arrange(id, quarter) %>%
  group_by(id) %>%
  mutate(value = as.numeric(value), log_value = log(value), .keep = "all") %>%
  ungroup()

if (nrow(panel) == 0) stop("No data loaded. Check paths.")

panel_indexed <- panel %>% group_by(id) %>% group_modify(~ index_series(.x, index_base)) %>% ungroup()

###########################
# 4. Triad plots and DiD
###########################
triad_agg <- panel %>%
  filter(id %in% c("CH", "HK", "SG")) %>%
  transmute(center = id, time_qtr = quarter, value = value) %>%
  group_by(time_qtr) %>%
  mutate(tri_total = sum(value, na.rm = TRUE), share_tri = value / tri_total) %>%
  ungroup() %>%
  mutate(group = if_else(center == "CH", "CH", "HK+SG")) %>%
  group_by(group, time_qtr) %>%
  summarise(share_tri = sum(share_tri, na.rm = TRUE), .groups = "drop")

fig_triad <- triad_agg %>%
  filter(time_qtr >= as.yearqtr("2017 Q1")) %>%
  ggplot(aes(x = as.Date(as.yearqtr(time_qtr)), y = share_tri, color = group)) +
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
ggsave(file.path(fig_dir, "fig_triad_share_CH_vs_HKSG.png"), fig_triad, width = 9, height = 5.2, dpi = 200)

triad_panel <- panel %>%
  filter(id %in% c("CH", "HK", "SG")) %>%
  transmute(center = id, time_qtr = quarter, liab = value) %>%
  group_by(time_qtr) %>%
  mutate(triad_total = sum(liab, na.rm = TRUE), triad_share = liab / triad_total) %>%
  ungroup() %>%
  mutate(treat_CH = as.integer(center == "CH"), post = as.integer(time_qtr >= q_treat)) %>%
  arrange(time_qtr, center) %>%
  mutate(t_id = as.integer(factor(time_qtr)))

event_qtr <- q_treat
event_t_id <- triad_panel %>% filter(time_qtr == event_qtr) %>% pull(t_id) %>% unique()
triad_panel <- triad_panel %>% mutate(g_triad = if_else(center == "CH", event_t_id, 0L))
triad_sample <- triad_panel %>% filter(time_qtr >= as.yearqtr("2018 Q1"))

did_triad <- feols(
  triad_share ~ post:treat_CH | center + time_qtr,
  data    = triad_sample,
  cluster = ~ center
)

did_triad_tidy <- broom::tidy(did_triad) %>%
  filter(term %in% c("post:treat_CH"))
write_csv(did_triad_tidy, file.path(out_dir, "did_triad_share_results.csv"))

fig_did_triad_share <- did_triad_tidy %>%
  mutate(label = "CH × post") %>%
  ggplot(aes(x = label, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "DiD: triad share (CH vs HK+SG)",
    x = NULL, y = "Estimate"
  ) +
  theme_minimal(base_size = 12)
ggsave(file.path(fig_dir, "did_triad_share_CH_HKSG.png"), fig_did_triad_share, width = 7, height = 4, dpi = 200)

###########################
# 5. Event-study (Sun–Abraham)
###########################
triad_es <- triad_panel %>%
  filter(time_qtr >= as.yearqtr("2018 Q1")) %>%
  mutate(t_id = as.integer(factor(time_qtr)))

es_triad <- feols(
  triad_share ~ sunab(g_triad, t_id) | center + t_id,
  cluster = ~ center,
  data = triad_es
)

es_triad_tidy_ci <- broom::tidy(es_triad, conf.int = TRUE) %>%
  filter(str_detect(term, "^t_id::")) %>%
  mutate(event_time = suppressWarnings(as.integer(stringr::str_match(term, "::(-?\\d+)$")[, 2]))) %>%
  drop_na(event_time)

p_es_triad <- es_triad_tidy_ci %>%
  ggplot(aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(
    title = "Event-study: CH vs HK+SG (share within triad)",
    x     = "Event time (quarters, 0 = 2022Q2)",
    y     = "Difference in triad share (CH \u2212 HK+SG)"
  ) +
  theme_minimal(base_size = 14)
ggsave(filename = "fig/fig_es_triad_CH_vs_HKSG_CI.png", plot = p_es_triad, width = 8, height = 5, dpi = 300, bg = "white")

###########################
# 6. Pre/post means (triad)
###########################
prepost_triad <- panel %>%
  filter(id %in% c("CH", "HK", "SG")) %>%
  mutate(group = if_else(id == "CH", "CH", "HK+SG"), post = quarter >= q_treat) %>%
  group_by(group, post) %>%
  summarise(mean_log = mean(log_value, na.rm = TRUE), mean_lvl = mean(value, na.rm = TRUE), .groups = "drop")
write_csv(prepost_triad, file.path(out_dir, "prepost_means_triad.csv"))

###########################
# 7. Save panel
###########################
write_csv(panel, file.path(out_dir, "panel_quarterly_CH_HK_SG.csv"))
write_csv(panel_indexed, file.path(out_dir, "panel_quarterly_indexed.csv"))

message("DiD outputs written to ./out and figures to ./fig")
