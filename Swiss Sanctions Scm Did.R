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
fixest::setFixest_notes(FALSE)

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
  LU = "data/bis_lu_quarterly.csv",
  SA = "data/Saudi_Arabia_cross_lia.csv",

  # FX controls (monthly)
  EXSZUS = "Raw_data/Exchange rate/Swiss_Francs_to_One_USd.csv",
  EXHKUS = "Raw_data/Exchange rate/HK_Dollars_to_One_USDollar.csv",
  EXSIUS = "Raw_data/Exchange rate/Singapore_Dollars_to_One_USDollar.csv",
  EXLUUS = "Raw_data/Luxembourg_usd.csv",
  EXSAUS = "Raw_data/Saudi_usd.csv",
  GDP_CH = "Raw_data/CLVMNACSAB1GQCH.csv"
)

ids_main   <- c("CH", "HK", "SG", "SA")         # include Saudi as a main unit we track
ids_donors <- c("GB", "NL", "IE", "LU")        # donors for Switzerland; LU added

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
read_fx_controls <- function(paths_fx, quarters_fallback = NULL) {
  fx_list <- list(
    read_monthly(paths_fx$EXSZUS, "ex_ch"),
    read_monthly(paths_fx$EXHKUS, "ex_hk"),
    read_monthly(paths_fx$EXSIUS, "ex_sg"),
    read_monthly(paths_fx$EXLUUS, "ex_lu"),
    read_monthly(paths_fx$EXSAUS, "ex_sa")
  ) %>% purrr::discard(is.null)
  if (length(fx_list) == 0 && !is.null(quarters_fallback)) {
    tibble(quarter = sort(unique(quarters_fallback)))
  } else {
    purrr::reduce(fx_list, full_join, by = "quarter")
  }
}
save_four_panel <- function(p1, p2, p3, p4, file, width = 12, height = 8, res = 200) {
  grDevices::png(filename = file, width = width, height = height, units = "in", res = res, bg = "white")
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))
  vp <- function(row, col) grid::viewport(layout.pos.row = row, layout.pos.col = col)
  if (!is.null(p1)) print(p1, vp = vp(1, 1))
  if (!is.null(p2)) print(p2, vp = vp(1, 2))
  if (!is.null(p3)) print(p3, vp = vp(2, 1))
  if (!is.null(p4)) print(p4, vp = vp(2, 2))
  grDevices::dev.off()
}

# Read quarterly GDP CH
read_gdp_ch <- function(path) {
  if (!file_exists(path)) return(NULL)
  df <- suppressMessages(read_csv(path, show_col_types = FALSE))
  date_col <- intersect(names(df), c("DATE", "Date", "date", "observation_date"))[1]
  val_col  <- setdiff(names(df), c("DATE", "Date", "date", "observation_date"))[1]
  if (is.na(date_col) || is.na(val_col)) return(NULL)
  df %>%
    transmute(
      quarter = as.yearqtr(as.Date(.data[[date_col]])),
      gdp_ch  = as.numeric(.data[[val_col]])
    ) %>%
    filter(!is.na(quarter), !is.na(gdp_ch))
}

# Pretrend joint test (Wald) utility
pretrend_test_one <- function(es_model, tidy_df) {
  pre_terms <- tidy_df %>% filter(event_time < 0) %>% pull(term)
  if (length(pre_terms) == 0) return(NA_real_)
  pat <- paste0("^(", paste(stringr::str_replace_all(pre_terms, "([\\+\\-])", "\\\\\\1"), collapse = "|"), ")$")
  w <- tryCatch(fixest::wald(es_model, keep = pat), error = function(e) NULL)
  if (is.null(w)) return(NA_real_)
  as.numeric(w$p.value)
}
safe_p <- function(x) if (length(x) == 0) NA_real_ else x

# Convert a raw BIS download (with header preamble) into a clean two-column CSV in ./data
convert_bis_raw <- function(raw_file, out_file) {
  if (!file_exists(raw_file)) return(FALSE)
  lines <- readr::read_lines(raw_file)
  idx <- which(stringr::str_detect(lines, "TIME_PERIOD"))
  if (length(idx) == 0) {
    message(glue("[warn] Could not find TIME_PERIOD in {raw_file}"))
    return(FALSE)
  }
  df <- suppressMessages(readr::read_csv(raw_file, skip = idx - 1, show_col_types = FALSE))
  time_col <- names(df)[stringr::str_detect(names(df), "TIME_PERIOD")][1]
  val_col  <- names(df)[stringr::str_detect(names(df), "OBS_VALUE")][1]
  if (is.na(time_col) || is.na(val_col)) {
    message(glue("[warn] Could not detect TIME_PERIOD/OBS_VALUE columns in {raw_file}"))
    return(FALSE)
  }
  cleaned <- df %>%
    transmute(TIME_PERIOD = .data[[time_col]], OBS_VALUE = as.numeric(.data[[val_col]])) %>%
    filter(!is.na(TIME_PERIOD), !is.na(OBS_VALUE))
  dir_create(path_dir(out_file))
  write_csv(cleaned, out_file)
  message(glue("[info] Parsed {nrow(cleaned)} rows from {raw_file} -> {out_file}"))
  TRUE
}

# Parse MAS/Swiss ancillary raw files (for descriptive robustness)
parse_ancillary_series <- function(path, value_col_clean, delimiter = ",") {
  if (!file_exists(path)) return(NULL)
  lines <- readr::read_lines(path)
  start <- which(stringr::str_detect(lines, "^End of Period"))[1]
  if (length(start) == 0 || is.na(start)) {
    start <- which(stringr::str_detect(lines, "^\"?Date\"?"))[1]
  }
  if (length(start) == 0 || is.na(start)) start <- 1
  df <- suppressWarnings(suppressMessages(
    readr::read_delim(path, delim = delimiter, skip = start - 1, show_col_types = FALSE, progress = FALSE)
  ))
  names(df) <- make.names(names(df))
  date_raw <- df[[1]]
  date_clean <- gsub("\\s*\\(.*?\\)", "", as.character(date_raw))
  date_parsed <- suppressWarnings(lubridate::parse_date_time(date_clean, orders = c("b Y", "Ym", "Y-m-d")))
  if (all(is.na(date_parsed))) return(NULL)
  val <- suppressWarnings(as.numeric(df[[value_col_clean]]))
  tibble(quarter = as.yearqtr(date_parsed), value = val) %>%
    drop_na(quarter, value) %>%
    group_by(quarter) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
}

# If newer raw BIS downloads exist, parse them into ./data before loading
raw_map <- list(
  CH = "Raw_data/bis_dp_search_export_SWIT_20251027-150523.csv",
  HK = "Raw_data/bis_dp_search_export_HK_20251027-152222.csv",
  SG = "Raw_data/bis_dp_search_export_SG_20251027-151930.csv",
  GB = "Raw_data/UK_20251027-212025.csv",
  LU = "Raw_data/Luxembourg_20251027-212259.csv",
  LU_alt = "Raw_data/Luxembourg——liability.csv",
  SA = "Raw_data/Saudi_Arabia_cross_lia.csv"
)

raw_to_out <- list(
  CH = paths$CH,
  HK = paths$HK,
  SG = paths$SG,
  GB = paths$GB,
  LU = paths$LU,
  SA = paths$SA
)

for (k in names(raw_to_out)) {
  raw_path <- raw_map[[k]]
  if (k == "LU") {
    ok <- convert_bis_raw(raw_path, raw_to_out[[k]])
    if (!ok) convert_bis_raw(raw_map$LU_alt, raw_to_out[[k]])
  } else {
    convert_bis_raw(raw_path, raw_to_out[[k]])
  }
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

###########################
# Auto-select donors closest to CH (pre-treatment mean & slope)
###########################
pre_treat_stats <- panel %>%
  filter(quarter < q_treat) %>%
  group_by(id) %>%
  summarise(
    mean_log = mean(log_value, na.rm = TRUE),
    slope    = if (n() >= 2) coef(lm(log_value ~ as.numeric(quarter)))[2] else NA_real_,
    .groups  = "drop"
  )

ch_ref <- pre_treat_stats %>% filter(id == "CH") %>% slice_head(n = 1)
ids_donors_auto <- pre_treat_stats %>%
  filter(!id %in% c("CH", "HK", "SG", "SA")) %>%  # keep donors distinct from treated/alternative centers
  mutate(
    dist = sqrt((mean_log - ch_ref$mean_log)^2 + (slope - ch_ref$slope)^2)
  ) %>%
  arrange(dist) %>%
  slice_head(n = 5) %>%
  drop_na(dist) %>%
  pull(id)
# ids_donors (original): c("GB", "NL", "IE", "LU")
if (length(ids_donors_auto) == 0) ids_donors_auto <- ids_donors

controls_q <- read_fx_controls(paths, quarters_fallback = panel$quarter)
if (nrow(controls_q) > 0 && ncol(controls_q) > 1) {
  fx_plot <- controls_q %>%
    pivot_longer(-quarter, names_to = "series", values_to = "value") %>%
    drop_na(value) %>%
    ggplot(aes(x = as.Date(quarter), y = value, color = series)) +
    geom_line(linewidth = 0.8) +
    labs(
      title = "FX controls (monthly → quarterly averages)",
      subtitle = "All raw FX files from Raw_data used; levels per USD",
      x = NULL, y = "FX (local per USD)", color = "Series"
    ) +
    theme_minimal(base_size = 11)
  ggsave(file.path(fig_dir, "fig_fx_controls.png"), fx_plot, width = 8, height = 4.5, dpi = 200)
}

# GDP overlay for CH
gdp_ch <- read_gdp_ch(paths$GDP_CH)
gdp_idx_tbl <- NULL
if (!is.null(gdp_ch) && nrow(gdp_ch) > 0) {
  gdp_ch <- gdp_ch %>% filter(quarter >= q_start, quarter <= q_end)
  liab_ch <- panel %>% filter(id == "CH") %>% select(quarter, liab_ch = value)
  gdp_liab <- liab_ch %>%
    left_join(gdp_ch, by = "quarter") %>%
    mutate(
      liab_idx = 100 * liab_ch / liab_ch[which.min(abs(quarter - index_base))],
      gdp_idx  = 100 * gdp_ch / gdp_ch[which.min(abs(quarter - index_base))]
    ) %>%
    drop_na(liab_idx, gdp_idx)

  if (nrow(gdp_liab) > 0) {
    fig_gdp_liab <- gdp_liab %>%
      pivot_longer(c(liab_idx, gdp_idx), names_to = "series", values_to = "index") %>%
      mutate(series = recode(series, liab_idx = "Liabilities (CH)", gdp_idx = "GDP (CH)")) %>%
      ggplot(aes(x = as.Date(quarter), y = index, color = series)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = as.Date(q_treat), linetype = "dashed") +
      labs(
        title = "Switzerland: liabilities vs GDP (index, 2019Q4=100)",
        subtitle = "Dashed = 2022Q2 sanctions",
        x = NULL, y = "Index (2019Q4 = 100)", color = NULL
      ) +
      theme_minimal(base_size = 12)
    ggsave(file.path(fig_dir, "fig_gdp_liab_CH.png"), fig_gdp_liab, width = 8.5, height = 5, dpi = 200)

    gdp_stats <- gdp_liab %>%
      mutate(post = quarter >= q_treat) %>%
      group_by(post) %>%
      summarise(
        mean_liab = mean(liab_idx),
        mean_gdp  = mean(gdp_idx),
        cor_ln    = cor(log(liab_ch), log(gdp_ch), use = "complete.obs"),
        .groups = "drop"
      )
    write_csv(gdp_stats, file.path(out_dir, "gdp_liab_summary_CH.csv"))
    gdp_idx_tbl <- gdp_liab %>% select(quarter, gdp_idx)
  }
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
       subtitle = "Vertical line = 2022Q2 (Switzerland adopts EU Russia sanctions)",
       x = NULL, y = "Index (2019Q4=100)", color = "Center") +
  theme_minimal(base_size = 12)
ggsave(file.path(fig_dir, "fig1_levels_index_CH_HK_SG.png"), fig1, width = 9, height = 5.2, dpi = 200)

###########################
# 4b. Triad shares: CH vs HK+SG
###########################

triad <- panel %>%
  filter(id %in% c("CH", "HK", "SG")) %>%
  transmute(center = id, time_qtr = quarter, value = value) %>%
  group_by(time_qtr) %>%
  mutate(
    tri_total = sum(value, na.rm = TRUE),
    share_tri = value / tri_total
  ) %>%
  ungroup()

triad_agg <- triad %>%
  mutate(
    group = case_when(
      center == "CH"              ~ "CH",
      center %in% c("HK", "SG")   ~ "HK+SG",
      TRUE                    ~ NA_character_
    )
  ) %>%
  filter(!is.na(group)) %>%
  group_by(group, time_qtr) %>%
  summarise(
    share_tri = sum(share_tri, na.rm = TRUE),
    .groups   = "drop"
  )

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

ggsave(
  file.path(fig_dir, "fig_triad_share_CH_vs_HKSG.png"),
  fig_triad, width = 9, height = 5.2, dpi = 200
)

###########################
# 4b-new. Triad-only DiD (CH vs HK+SG)
###########################
triad <- panel %>%
  filter(id %in% c("CH", "HK", "SG")) %>%
  group_by(quarter) %>%
  mutate(
    triad_total = sum(value, na.rm = TRUE),
    share       = value / triad_total
  ) %>%
  ungroup() %>%
  mutate(post = as.integer(quarter >= q_treat))

triad_group <- triad %>%
  mutate(group = if_else(id == "CH", "CH", "HKSG")) %>%
  group_by(group, quarter, post) %>%
  summarise(share = sum(share, na.rm = TRUE), .groups = "drop")

did_triad_share_CH <- feols(
  share ~ i(group, post, ref = "HKSG") | group + quarter,
  cluster = ~ group,
  data = triad_group
)

did_triad_share_HKSG <- feols(
  share ~ i(group, post, ref = "CH") | group + quarter,
  cluster = ~ group,
  data = triad_group
)

tidy_triad <- bind_rows(
  broom::tidy(did_triad_share_CH) %>%
    filter(stringr::str_detect(term, "^group::CH")) %>%
    mutate(term = "CH:post"),
  broom::tidy(did_triad_share_HKSG) %>%
    filter(stringr::str_detect(term, "^group::HKSG")) %>%
    mutate(term = "HKSG:post")
)

write_csv(
  tidy_triad,
  file.path(out_dir, "did_triad_share_CH_HKSG.csv")
)

fig_did_triad_share <- tidy_triad %>%
  mutate(label = recode(term, `CH:post` = "CH × post", `HKSG:post` = "HK+SG × post")) %>%
  ggplot(aes(x = label, y = estimate)) +
  geom_point() +
  geom_errorbar(
    aes(ymin = estimate - 1.96 * std.error,
        ymax = estimate + 1.96 * std.error),
    width = 0.15
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "DiD: triad share (CH vs HK+SG)",
    x = NULL, y = "Estimate"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(fig_dir, "did_triad_share_CH_HKSG.png"),
  fig_did_triad_share, width = 7, height = 4, dpi = 200
)

###########################
# 4c. Event-study: CH vs HK+SG (share within triad)
###########################

triad_es <- triad_agg %>%
  mutate(
    treated = (group == "CH"),
    time_id = as.integer(factor(time_qtr, levels = sort(unique(time_qtr))))
  )

ref_time_tri <- max(triad_es$time_id[triad_es$time_qtr < q_treat])

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
  file.path(fig_dir, "fig_es_triad_CH_vs_HKSG.png"),
  fig_es_triad, width = 8.5, height = 5.2, dpi = 200
)

###########################
# 4d. Triad-share DiD & ES (CH vs HK+SG, formal)
###########################

triad_panel <- panel %>%
  filter(id %in% c("CH", "HK", "SG")) %>%
  transmute(
    center   = id,
    time_qtr = quarter,
    liab     = value
  ) %>%
  group_by(time_qtr) %>%
  mutate(
    triad_total = sum(liab, na.rm = TRUE),
    triad_share = liab / triad_total
  ) %>%
  ungroup() %>%
  mutate(
    treat_CH = as.integer(center == "CH"),
    post     = as.integer(time_qtr >= q_treat)
  )

triad_panel <- triad_panel %>%
  arrange(time_qtr, center) %>%
  mutate(t_id = as.integer(factor(time_qtr)))

event_qtr <- q_treat
event_t_id <- triad_panel %>% filter(time_qtr == event_qtr) %>% pull(t_id) %>% unique()

triad_panel <- triad_panel %>%
  mutate(
    g_triad = if_else(center == "CH", event_t_id, 0L)
  )

triad_sample <- triad_panel %>% filter(time_qtr >= as.yearqtr("2018 Q1"))

did_triad <- feols(
  triad_share ~ post:treat_CH | center + time_qtr,
  data    = triad_sample,
  cluster = ~ center
)
did_triad_tidy <- broom::tidy(did_triad) %>%
  filter(term %in% c("post:treat_CH"))

write_csv(
  did_triad_tidy,
  file.path(out_dir, "did_triad_share_results.csv")
)
writeLines(
  capture.output(print(summary(did_triad))),
  file.path(out_dir, "did_triad_share_console.txt")
)
print(summary(did_triad))
# ATT interpretation: coefficient on treat_CH:post is in share units (~percentage points of triad share).

did_ch_donors_data <- panel %>%
  filter(id %in% c("CH", ids_donors_auto)) %>%
  mutate(
    treated = as.integer(id == "CH"),
    post    = as.integer(quarter >= q_treat)
  ) %>%
  { if (!is.null(gdp_idx_tbl)) left_join(., gdp_idx_tbl, by = "quarter") else mutate(., gdp_idx = NA_real_) } %>%
  mutate(gdp_idx = if_else(id == "CH", gdp_idx, 0))  # donors get 0 baseline so CH variation remains

did_ch_donors_group <- did_ch_donors_data %>%
  mutate(group = if_else(id == "CH", "CH", "Donor")) %>%
  group_by(group, quarter, post) %>%
  summarise(
    log_value = mean(log_value, na.rm = TRUE),
    gdp_idx   = mean(gdp_idx, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(group = factor(group, levels = c("Donor", "CH")))

did_ch_donors <- feols(
  log_value ~ i(group, post, ref = "Donor") + gdp_idx | group + quarter,
  cluster = ~ group,
  data = did_ch_donors_group
)

did_ch_donors_tidy <- broom::tidy(did_ch_donors) %>%
  filter(term == "group::CH:post")

# Robustness: fixed donor set (GB, NL, IE, LU)
did_ch_donors_data_manual <- panel %>%
  filter(id %in% c("CH", ids_donors)) %>%
  mutate(
    treated = as.integer(id == "CH"),
    post    = as.integer(quarter >= q_treat)
  ) %>%
  { if (!is.null(gdp_idx_tbl)) left_join(., gdp_idx_tbl, by = "quarter") else mutate(., gdp_idx = NA_real_) } %>%
  mutate(gdp_idx = if_else(id == "CH", gdp_idx, 0))

did_ch_donors_group_manual <- did_ch_donors_data_manual %>%
  mutate(group = if_else(id == "CH", "CH", "Donor")) %>%
  group_by(group, quarter, post) %>%
  summarise(
    log_value = mean(log_value, na.rm = TRUE),
    gdp_idx   = mean(gdp_idx, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(group = factor(group, levels = c("Donor", "CH")))

did_ch_donors_manual <- feols(
  log_value ~ i(group, post, ref = "Donor") + gdp_idx | group + quarter,
  cluster = ~ group,
  data = did_ch_donors_group_manual
)

did_ch_donors_manual_tidy <- broom::tidy(did_ch_donors_manual) %>%
  filter(term == "group::CH:post")

write_csv(
  did_ch_donors_tidy,
  file.path(out_dir, "did_log_CH_vs_donors_results.csv")
)
writeLines(
  capture.output(print(summary(did_ch_donors))),
  file.path(out_dir, "did_log_CH_vs_donors_console.txt")
)
write_csv(
  did_ch_donors_manual_tidy,
  file.path(out_dir, "did_log_CH_vs_manual_donors_results.csv")
)

key_coefs <- bind_rows(
  broom::tidy(did_triad) %>%
    filter(term == "post:treat_CH") %>%
    mutate(model = "Triad share CH vs HK+SG"),
  broom::tidy(did_ch_donors) %>%
    filter(term == "group::CH:post") %>%
    mutate(model = "Log liabilities CH vs auto donors"),
  did_ch_donors_manual_tidy %>%
    mutate(model = "Log liabilities CH vs fixed donors")
)

fig_did_coef <- key_coefs %>%
  ggplot(aes(x = model, y = estimate)) +
  geom_point() +
  geom_errorbar(
    aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    ),
    width = 0.2
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(
    title = "DiD estimates across specifications",
    x     = NULL,
    y     = "Estimate"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(fig_dir, "fig_did_coefficients.png"),
  fig_did_coef, width = 7, height = 4, dpi = 200
)

# pre-trend joint tests for DiD/event-studies
pretrend_test <- function(es_model, es_tidy_df, model_name) {
  pre_terms <- es_tidy_df %>% filter(event_time < 0) %>% pull(term)
  if (length(pre_terms) == 0) return(NULL)
  pat <- paste0("^(", paste(stringr::str_replace_all(pre_terms, "([\\+\\-])", "\\\\\\1"), collapse = "|"), ")$")
  w <- tryCatch(fixest::wald(es_model, keep = pat), error = function(e) NULL)
  if (is.null(w)) return(NULL)
  tibble(
    model = model_name,
    n_pre  = length(pre_terms),
    wald   = as.numeric(w$F[1]),
    p_value = as.numeric(w$p.value)
  )
}

es_triad <- feols(
  triad_share ~ sunab(g_triad, t_id) | center + t_id,
  cluster = ~ center,
  data = triad_panel %>% filter(time_qtr >= as.yearqtr("2018 Q1"))
)

es_triad_tidy_ci <- broom::tidy(es_triad, conf.int = TRUE) %>%
  filter(str_detect(term, "^t_id::")) %>%
  mutate(
    event_time = suppressWarnings(as.integer(stringr::str_match(term, "::(-?\\d+)$")[, 2]))
  ) %>%
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

ggsave(
  filename = "fig/fig_es_triad_CH_vs_HKSG_CI.png",
  plot     = p_es_triad,
  width    = 8,
  height   = 5,
  dpi      = 300,
  bg       = "white"
)
# Interpretation:
# es_triad Sun–Abraham coefficients measure CH triad share relative to HK+SG
# around the 2022Q2 sanction shock (event time 0); post-2022Q2 coefficients
# should be negative if CH loses triad share to HK+SG.
ggsave(
  filename = "fig/fig_es_triad_CH_vs_HKSG_CI.png",
  plot     = p_es_triad,
  width    = 8,
  height   = 5,
  dpi      = 300,
  bg       = "white"
)

###########################
# 4e. Global-share DiD: CH/HK+SG vs donor centres
###########################
triad_donors <- panel %>%
  filter(id %in% c("CH", "HK", "SG", ids_donors_auto)) %>%
  mutate(
    group = case_when(
      id == "CH" ~ "CH",
      id %in% c("HK", "SG") ~ "HKSG",
      id %in% ids_donors_auto ~ "Donor",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(group)) %>%
  group_by(quarter) %>%
  mutate(
    global_total = sum(value, na.rm = TRUE),
    global_share = value / global_total,
    post         = as.integer(quarter >= q_treat)
  ) %>%
  ungroup()

triad_donors_group <- triad_donors %>%
  group_by(group, quarter, post) %>%
  summarise(global_share = sum(global_share, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = factor(group, levels = c("Donor", "CH", "HKSG")))

did_global <- feols(
  global_share ~ i(group, post, ref = "Donor") | group + quarter,
  cluster = ~ group,
  data = triad_donors_group
)

tidy_global <- broom::tidy(did_global) %>%
  filter(stringr::str_detect(term, "^group::"))

write_csv(
  tidy_global,
  file.path(out_dir, "did_global_share_CH_HKSG_vs_donors.csv")
)

fig_did_global <- tidy_global %>%
  mutate(label = stringr::str_remove(term, "^group::")) %>%
  ggplot(aes(x = label, y = estimate)) +
  geom_point() +
  geom_errorbar(
    aes(ymin = estimate - 1.96 * std.error,
        ymax = estimate + 1.96 * std.error),
    width = 0.15
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "DiD: global share vs donor centres",
    subtitle = "Effects relative to Donor group",
    x = NULL, y = "Estimate"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(fig_dir, "did_global_share_CH_HKSG_vs_donors.png"),
  fig_did_global, width = 7, height = 4, dpi = 200
)

###########################
# 5. synthdid SCM
###########################
panel_scm <- panel %>% filter(quarter >= as.yearqtr("2017 Q1"))

donors_for_ch <- intersect(ids_donors_auto, unique(panel_scm$id))
units_for_ch  <- c(donors_for_ch, "CH")  # donors first, CH last

ch_mat <- panel_scm %>%
  filter(id %in% units_for_ch) %>%
  select(id, quarter, log_value) %>%
  pivot_wider(names_from = quarter, values_from = log_value) %>%
  arrange(factor(id, levels = units_for_ch))

if (nrow(ch_mat) < 2) stop("Need at least Switzerland + one donor with overlapping data.")

Y <- as.matrix(ch_mat[, -1])
rownames(Y) <- ch_mat$id
treated_row <- nrow(Y)

all_q <- as.yearqtr(colnames(ch_mat)[-1])
complete_cols <- colSums(!is.na(Y)) == nrow(Y)
Y <- Y[, complete_cols, drop = FALSE]
all_q <- all_q[complete_cols]
T0   <- sum(all_q < q_treat)
N0   <- min(length(ids_donors_auto), nrow(Y) - 1)   # donors = first rows, CH = last; safeguard if some donors missing
if (T0 <= 3) warning("Pre-period is short; SCM fit may be weak.")

set.seed(123)
if (nrow(Y) >= 2 && T0 >= 2) {
  ch_sd <- tryCatch(
    synthdid_estimate(Y, N0 = N0, T0 = T0),
    error = function(e) { message("[warn] synthdid failed: ", e$message); NULL }
  )
  ch_tau <- tryCatch(as.numeric(ch_sd), error = function(e) { message("[warn] coef() failed: ", e$message); NA_real_ })
  if (length(ch_tau) == 0) ch_tau <- NA_real_
  if (!is.na(ch_tau)) print(glue("synthdid ATT (post-mean log gap): {round(ch_tau, 4)}"))

  if (!is.null(ch_sd)) {
    tryCatch({
      png(file.path(fig_dir, "fig2_synthdid_paths_CH.png"), width = 900, height = 520)
      plot(ch_sd)
      invisible(dev.off())
    }, error = function(e) message("[warn] synthdid plot failed: ", e$message))
  }
} else {
  ch_sd <- NULL
  ch_tau <- NA_real_
  message("[warn] synthdid skipped: insufficient donors or pre-period (nrow(Y)<2 or T0<2)")
}

placebo_df <- purrr::map_dfr(rownames(Y), function(u) {
  donors_u <- setdiff(rownames(Y), u)

  # controls first, treated last
  Y_rot <- rbind(
    Y[donors_u, , drop = FALSE],
    Y[u, , drop = FALSE]
  )
  N0_rot <- nrow(Y_rot) - 1

  est <- tryCatch(synthdid_estimate(Y_rot, N0 = N0_rot, T0 = T0), error = function(e) NULL)

  tibble(
    unit = u,
    att  = tryCatch(as.numeric(est), error = function(e) NA_real_)
  )
}) %>% drop_na(att)
if (nrow(placebo_df) > 0) {
  write_csv(placebo_df, file.path(out_dir, "synthdid_placebo_att.csv"))
} else {
  message("[warn] placebo_df empty; skipping placebo export/plot")
}

tau_ch   <- tryCatch(as.numeric(ch_sd), error = function(e) NA_real_)
placebos <- placebo_df %>% filter(unit != "CH")
perm_p   <- if (!is.na(tau_ch) && nrow(placebos) > 0) mean(abs(placebos$att) >= abs(tau_ch)) else NA_real_
write_csv(tibble(tau_ch = tau_ch, perm_p = perm_p), file.path(out_dir, "synthdid_perm_pval.csv"))
if (!is.na(perm_p)) message(glue("Permutation p-value = {round(perm_p, 3)} (|tau_CH| vs placebos)"))

if (nrow(placebo_df) > 0) {
  fig3 <- placebo_df %>%
    mutate(is_CH = (unit == "CH")) %>%
    ggplot(aes(x = reorder(unit, att), y = att, fill = is_CH)) +
    geom_col() +
    coord_flip() +
    guides(fill = "none") +
    labs(title = "Placebo ATT distribution (log scale)", y = "Post-mean ATT (log)", x = NULL,
         subtitle = glue("N_placebo = {nrow(placebo_df)}; perm p = {round(perm_p,3)}")) +
    theme_minimal(base_size = 12)
  ggsave(file.path(fig_dir, "fig3_placebo_ATT.png"), fig3, width = 8, height = 6, dpi = 200)
}

wt <- attr(ch_sd, "weights")
omega <- if (!is.null(wt)) wt$omega else NULL
lambda <- if (!is.null(wt)) wt$lambda else NULL
donor_idx <- if (!is.null(omega)) seq_len(length(omega)) else integer(0)
donor_names <- if (length(donor_idx) > 0) rownames(Y)[donor_idx] else character(0)
fig_weights <- NULL
if (!is.null(omega) && length(donor_idx) > 0) {
  weights_df <- tibble(donor = donor_names, weight = as.numeric(omega))
  write_csv(weights_df, file.path(out_dir, "synthdid_donor_weights.csv"))
  fig_weights <- weights_df %>%
    ggplot(aes(x = reorder(donor, weight), y = weight)) +
    geom_col(fill = "gray40") +
    coord_flip() +
    labs(
      title = "SCM donor weights",
      x = NULL, y = "Weight"
    ) +
    theme_minimal(base_size = 12)
  ggsave(file.path(fig_dir, "fig_synthdid_weights.png"), fig_weights, width = 6, height = 4, dpi = 200)
}
if (!is.null(lambda)) {
  lambda_q <- all_q[seq_len(length(lambda))]
  write_csv(tibble(quarter = as.character(lambda_q), weight = as.numeric(lambda)), file.path(out_dir, "synthdid_time_weights.csv"))
}

Y_treat <- as.numeric(Y[treated_row, ])
Y_synth <- rep(NA_real_, ncol(Y))
if (!is.null(omega) && length(donor_idx) > 0) {
  Y_don <- Y[donor_idx, , drop = FALSE]
  Y_synth <- as.numeric(drop(t(omega) %*% Y_don))
}

gap_df <- tibble(
  quarter = all_q,
  CH      = Y_treat,
  synth   = Y_synth,
  gap     = Y_treat - Y_synth
)
write_csv(gap_df, file.path(out_dir, "synthdid_paths_gap.csv"))

if (nrow(gap_df) > 0) {
  gap_long <- gap_df %>%
    select(quarter, CH, synth) %>%
    pivot_longer(-quarter, names_to = "series", values_to = "log_value")

  fig2_paths <- gap_long %>%
    ggplot(aes(x = as.Date(quarter), y = log_value, color = series)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = as.Date(q_treat), linetype = "dashed") +
    labs(
      title = "Switzerland vs synthetic donors (log liabilities)",
      subtitle = "Dashed = 2022Q2; synthetic = synthdid weights",
      x = NULL, y = "Log liabilities", color = "Series"
    ) +
    theme_minimal(base_size = 12)

  fig2_gap <- gap_df %>%
    drop_na(gap) %>%
    ggplot(aes(x = as.Date(quarter), y = gap)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line(color = "firebrick", linewidth = 1) +
    geom_vline(xintercept = as.Date(q_treat), linetype = "dashed") +
    labs(
      title = "Gap: CH minus synthetic (log)",
      subtitle = "Negative values = CH below synthetic counterfactual",
      x = NULL, y = "Log gap"
    ) +
    theme_minimal(base_size = 12)

  ggsave(file.path(fig_dir, "fig2b_synthdid_paths_CH_custom.png"), fig2_paths, width = 9, height = 5.2, dpi = 200)
  ggsave(file.path(fig_dir, "fig2c_synthdid_gap_CH.png"), fig2_gap, width = 9, height = 4.5, dpi = 200)
}

# 2x2 SCM panel: path, gap, placebo, weights
if (exists("fig2_paths") && exists("fig2_gap") && exists("fig3") && !is.null(fig_weights)) {
  save_four_panel(
    fig2_paths + theme(legend.position = "bottom"),
    fig2_gap,
    fig3 + theme(legend.position = "none"),
    fig_weights,
    file = file.path(fig_dir, "fig2d_scm_panel.png"),
    width = 12, height = 8, res = 200
  )
}
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

  est <- tryCatch(synthdid_estimate(Y_sub, N0 = nrow(Y_sub) - 1, T0 = T0), error = function(e) NULL)

  tibble(
    dropped = dropped,
    att     = tryCatch(as.numeric(est), error = function(e) NA_real_)
  )
}) %>% drop_na(att)
if (nrow(loo) > 0) {
  write_csv(loo, file.path(out_dir, "synthdid_leave_one_out.csv"))

  fig3b <- loo %>% ggplot(aes(x = reorder(dropped, att), y = att)) +
    geom_point() + coord_flip() +
    labs(title = "Leave-one-out ATT (drop each donor)", x = "Dropped donor", y = "ATT (log)") +
    theme_minimal(base_size = 12)
  ggsave(file.path(fig_dir, "fig3b_synthdid_leave_one_out.png"), fig3b, width = 8, height = 6, dpi = 200)
} else {
  message("[warn] leave-one-out empty; skipping plot/export")
}

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
pre_p_es <- safe_p(pretrend_test_one(es, es_tidy))
if (length(pre_p_es) > 0 && !is.na(pre_p_es)) {
  fig6 <- fig6 + annotate("text", x = min(es_tidy$event_time, na.rm = TRUE), y = max(es_tidy$estimate, na.rm = TRUE),
                          label = glue("Pretrend p = {scales::number(pre_p_es, accuracy = 0.001)}"),
                          hjust = 0, vjust = 1, size = 3.2)
}
ggsave(file.path(fig_dir, "fig6_event_study_CH.png"), fig6, width = 8.5, height = 5.2, dpi = 200)

panel_es_donors <- panel %>% filter(id %in% c("CH", intersect(ids_donors_auto, unique(id)))) %>%
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
pre_p_es_d <- safe_p(pretrend_test_one(es_d, es_d_tidy))
if (length(pre_p_es_d) > 0 && !is.na(pre_p_es_d)) {
  fig6b <- fig6b + annotate("text", x = min(es_d_tidy$event_time, na.rm = TRUE), y = max(es_d_tidy$estimate, na.rm = TRUE),
                            label = glue("Pretrend p = {scales::number(pre_p_es_d, accuracy = 0.001)}"),
                            hjust = 0, vjust = 1, size = 3.2)
}
ggsave(file.path(fig_dir, "fig6b_event_study_CH_donors_only.png"), fig6b, width = 8.5, height = 5.2, dpi = 200)

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

# Note: coefficients here are noisy and CIs typically include zero; treat as exploratory robustness.
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
pre_p_es_hksg <- safe_p(pretrend_test_one(es_hksg, es_hksg_tidy))
if (length(pre_p_es_hksg) > 0 && !is.na(pre_p_es_hksg)) {
  fig_HKSG <- fig_HKSG + annotate("text", x = min(es_hksg_tidy$event_time, na.rm = TRUE), y = max(es_hksg_tidy$estimate, na.rm = TRUE),
                                  label = glue("Pretrend p = {scales::number(pre_p_es_hksg, accuracy = 0.001)}"),
                                  hjust = 0, vjust = 1, size = 3.2)
}

ggsave(
  file.path(fig_dir, "fig_event_HKSG_share.png"),
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
      file.path(fig_dir, "fig_event_SA_vs_donors.png"),
      fig_sa, width = 8.5, height = 5.2, dpi = 200
    )
    TRUE
  }, error = function(e) { message("[warn] SA event-study skipped: ", e$message); FALSE })
} else {
  message("[warn] SA event-study skipped: insufficient data")
}

###########################
# 6d. Descriptive stats tables
###########################
desc_stats <- panel %>%
  mutate(post = quarter >= q_treat) %>%
  group_by(id, post) %>%
  summarise(
    mean_log = mean(log_value),
    sd_log   = sd(log_value),
    mean_lvl = mean(value),
    .groups  = "drop"
  )

write_csv(
  desc_stats,
  file.path(out_dir, "descriptive_stats_by_center_pre_post.csv")
)

###########################
# 6e. DiD placebo panel (donor units as pseudo-treated)
###########################
placebo_units <- intersect(ids_donors_auto, unique(panel$id))

did_placebo_df <- purrr::map_dfr(placebo_units, function(u) {
  dat <- panel %>%
    filter(id %in% c(u, ids_donors_auto[ids_donors_auto != u])) %>%
    mutate(
      treated = (id == u),
      post    = as.integer(quarter >= q_treat)
    )

  est <- tryCatch(
    feols(log_value ~ post:treated | id + quarter,
          cluster = ~ id,
          data = dat),
    error = function(e) NULL
  )

  if (is.null(est)) {
    return(tibble(unit = u, estimate = NA_real_, std.error = NA_real_))
  }

  broom::tidy(est) %>%
    filter(str_detect(term, "post:treated")) %>%
    transmute(unit = u, estimate, std.error)
}) %>%
  drop_na(estimate)

write_csv(
  did_placebo_df,
  file.path(out_dir, "did_placebo_log_results.csv")
)

ch_row <- did_ch_donors_tidy %>%
  filter(stringr::str_detect(term, "group::CH")) %>%
  slice_head(n = 1) %>%
  transmute(unit = "CH", estimate, std.error)

placebo_plot_df <- bind_rows(
  did_placebo_df %>% mutate(is_CH = FALSE),
  ch_row %>% mutate(is_CH = TRUE)
) %>%
  drop_na(estimate)

if (nrow(placebo_plot_df) > 0) {
  fig_did_placebo <- placebo_plot_df %>%
    ggplot(aes(x = reorder(unit, estimate), y = estimate, color = is_CH, shape = is_CH)) +
    geom_point(size = 3) +
    geom_errorbar(
      aes(ymin = estimate - 1.96 * std.error,
          ymax = estimate + 1.96 * std.error),
      width = 0.15,
      na.rm = TRUE
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c(`FALSE` = "black", `TRUE` = "red")) +
    scale_shape_manual(values = c(`FALSE` = 17, `TRUE` = 19)) +
    coord_flip() +
    labs(
      title = "DiD placebo: treated×post (CH and donor centres)",
      subtitle = "Red = actual CH effect; black triangles = pseudo-treated donors",
      x     = "Pseudo-treated unit",
      y     = "Estimate"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.title = element_blank())

  ggsave(
    file.path(fig_dir, "fig_did_placebo_CH_and_donors.png"),
    fig_did_placebo, width = 7, height = 4, dpi = 200
  )
  ggsave(
    file.path(fig_dir, "fig_did_placebo_donors.png"),
    fig_did_placebo, width = 7, height = 4, dpi = 200
  )
} else {
  message("[warn] placebo plot skipped: no placebo observations and CH ATT available")
}

###########################
# 6f. Pre-trend joint tests
###########################
pretrend_rows <- tibble(
  model = c("ES: CH vs all (log)", "ES: CH vs donors (log)", "ES: HK+SG vs donors (share)"),
  p_value = c(pre_p_es, pre_p_es_d, pre_p_es_hksg)
) %>% drop_na(p_value)

if (nrow(pretrend_rows) > 0) {
  write_csv(pretrend_rows, file.path(out_dir, "pretrend_tests.csv"))
  pretrend_plot <- pretrend_rows %>%
    mutate(model = forcats::fct_reorder(model, p_value)) %>%
    ggplot(aes(x = model, y = p_value)) +
    geom_col(fill = "gray40") +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "firebrick") +
    coord_flip() +
    labs(
      title = "Pre-trend joint tests (all pre-event coefficients = 0)",
      y = "p-value", x = NULL,
      subtitle = "Dashed line = 5% threshold"
    ) +
    theme_minimal(base_size = 12)
  ggsave(file.path(fig_dir, "fig_pretrend_tests.png"), pretrend_plot, width = 7, height = 4, dpi = 200)
}

###########################
# 6g. Placebo timing robustness (fake treatment at 2019Q4)
###########################
placebo_treat_q <- as.yearqtr("2019 Q4")
did_ch_placebo <- did_ch_donors_data %>%
  mutate(
    group = if_else(id == "CH", "CH", "Donor"),
    post_placebo = as.integer(quarter >= placebo_treat_q)
  ) %>%
  group_by(group, quarter, post_placebo) %>%
  summarise(log_value = mean(log_value, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = factor(group, levels = c("Donor", "CH")))

did_placebo_est <- feols(
  log_value ~ i(group, post_placebo, ref = "Donor") | group + quarter,
  cluster = ~ group,
  data = did_ch_placebo
)
did_placebo_tidy <- broom::tidy(did_placebo_est) %>%
  filter(term == "group::CH:post_placebo")

write_csv(did_placebo_tidy, file.path(out_dir, "did_log_CH_vs_donors_placebo_timing.csv"))

fig_did_placebo_time <- did_placebo_tidy %>%
  mutate(label = "Placebo 2019Q4") %>%
  { df <- .; est_val <- df$estimate[1]; se_val <- df$std.error[1];
    y_min <- min(est_val - 0.2, -1); y_max <- max(0.05, est_val + 0.2);
    ggplot(df, aes(x = label, y = estimate)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.1) +
      annotate("text", x = 1, y = est_val + 0.12, label = glue("est = {round(est_val,3)}"), size = 3.2) +
      labs(
        title = "Placebo timing: CH vs donors (log liabilities)",
        subtitle = "Fake treatment at 2019Q4; expect ~0 if no pre-trend",
        x = NULL, y = "Estimate (log points)"
      ) +
      coord_cartesian(ylim = c(y_min, y_max)) +
      theme_minimal(base_size = 12)
  }
ggsave(file.path(fig_dir, "fig_did_placebo_timing_CH.png"), fig_did_placebo_time, width = 6, height = 4, dpi = 200)

###########################
# 6h. Ancillary raw-data descriptives (use all Raw_data)
###########################
anc_sg_liab <- parse_ancillary_series("Raw_data/SG Banking System_ Liabilities.csv", "Liabilities")
anc_sg_nonres <- parse_ancillary_series("Raw_data/SG Commercial Banks_ Loans and Advances to Non-Residents by Industry.csv",
                                        "Loans.and.Advances.Including.Bills.Financing")
anc_sg_res <- parse_ancillary_series("Raw_data/SG Commercial Banks_ Loans and Advances to Residents by Industry.csv",
                                     "Loans.and.Advances.Including.Bills.Financing")
anc_ch_deposits <- parse_ancillary_series("Raw_data/SWIT Customer deposits excluding tied pension provision.csv",
                                          "Value", delimiter = ";")

anc_list <- list(
  sg_liabilities = anc_sg_liab,
  sg_loans_nonres = anc_sg_nonres,
  sg_loans_res = anc_sg_res,
  ch_deposits = anc_ch_deposits
)

purrr::iwalk(anc_list, function(df, nm) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  write_csv(df, file.path(out_dir, glue("anc_{nm}.csv")))
  p <- df %>%
    ggplot(aes(x = as.Date(quarter), y = value)) +
    geom_line(color = "steelblue", linewidth = 0.9) +
    geom_vline(xintercept = as.Date(q_treat), linetype = "dashed", color = "firebrick") +
    labs(
      title = glue("Ancillary series: {nm}"),
      subtitle = "Quarterly mean of raw series; dashed = 2022Q2",
      x = NULL, y = "Level"
    ) +
    theme_minimal(base_size = 11)
  ggsave(file.path(fig_dir, glue("fig_{nm}.png")), p, width = 8, height = 4.5, dpi = 200)
})

###########################
# 7. Save panel
###########################
write_csv(panel, file.path(out_dir, "panel_quarterly_CH_HK_SG_and_donors.csv"))
write_csv(panel_indexed, file.path(out_dir, "panel_quarterly_indexed.csv"))

new_outputs <- c(
  "out/did_triad_share_CH_HKSG.csv",
  "fig/did_triad_share_CH_HKSG.png",
  "out/did_global_share_CH_HKSG_vs_donors.csv",
  "fig/did_global_share_CH_HKSG_vs_donors.png",
  "fig/fig_did_placebo_CH_and_donors.png"
)

# Collate main results for quick reference
main_results <- tibble::tibble(
  model = c(
    "Triad share DiD (CH vs HK+SG)",
    "Log liabilities DiD (CH vs auto donors, +GDP)",
    "Log liabilities DiD (CH vs fixed donors, +GDP)",
    "SCM ATT (log gap)",
    "SCM perm p-value"
  ),
  estimate = c(
    if (nrow(did_triad_tidy) > 0) did_triad_tidy$estimate[1] else NA_real_,
    if (nrow(did_ch_donors_tidy) > 0) did_ch_donors_tidy$estimate[1] else NA_real_,
    if (nrow(did_ch_donors_manual_tidy) > 0) did_ch_donors_manual_tidy$estimate[1] else NA_real_,
    if (!is.null(ch_tau)) ch_tau else NA_real_,
    if (!is.null(perm_p)) perm_p else NA_real_
  ),
  std_error = c(
    if (nrow(did_triad_tidy) > 0) did_triad_tidy$std.error[1] else NA_real_,
    if (nrow(did_ch_donors_tidy) > 0) did_ch_donors_tidy$std.error[1] else NA_real_,
    if (nrow(did_ch_donors_manual_tidy) > 0) did_ch_donors_manual_tidy$std.error[1] else NA_real_,
    NA_real_,
    NA_real_
  ),
  p_value = c(
    if (nrow(did_triad_tidy) > 0) did_triad_tidy$p.value[1] else NA_real_,
    if (nrow(did_ch_donors_tidy) > 0) did_ch_donors_tidy$p.value[1] else NA_real_,
    if (nrow(did_ch_donors_manual_tidy) > 0) did_ch_donors_manual_tidy$p.value[1] else NA_real_,
    NA_real_,
    if (!is.null(perm_p)) perm_p else NA_real_
  )
) %>%
  mutate(
    t_value = if_else(!is.na(std_error) & std_error != 0, estimate / std_error, NA_real_),
    sig = case_when(
      !is.na(p_value) & p_value < 0.001 ~ "***",
      !is.na(p_value) & p_value < 0.01  ~ "**",
      !is.na(p_value) & p_value < 0.05  ~ "*",
      !is.na(p_value) & p_value < 0.1   ~ ".",
      TRUE ~ ""
    ),
    note = c(
      "CH share vs HK+SG (triad, pct-pt)",
      "CH vs auto donors, log liabilities, +GDP",
      "CH vs fixed donors, log liabilities, +GDP",
      "SCM post-mean log gap",
      "Permutation p-value (SCM)"
    )[seq_len(n())]
  )
write_csv(main_results, file.path(out_dir, "main_results_summary.csv"))

# Export main results to LaTeX and HTML for write-up
fmt_num <- function(x) ifelse(is.na(x), "--", sprintf("%.3f", x))
latex_lines <- c(
  "\\begin{tabular}{lrrrrr}",
  "\\toprule",
  "Model & Estimate & Std. Error & t value & p-value & Sig.\\\\",
  "\\midrule",
  paste0(main_results$model, " & ",
         fmt_num(main_results$estimate), " & ",
         fmt_num(main_results$std_error), " & ",
         fmt_num(main_results$t_value), " & ",
         fmt_num(main_results$p_value), " & ",
         main_results$sig, "\\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(latex_lines, file.path(out_dir, "main_results_table.tex"))

html_rows <- apply(main_results, 1, function(r) {
  est_class <- if (!is.na(as.numeric(r["estimate"])) && as.numeric(r["estimate"]) < 0) "neg" else "pos"
  paste0(
    "<tr>",
    "<td>", r["model"], "<br/><span class='note'>", r["note"], "</span></td>",
    "<td class='", est_class, "'>", fmt_num(as.numeric(r["estimate"])), "</td>",
    "<td>", fmt_num(as.numeric(r["std_error"])), "</td>",
    "<td>", fmt_num(as.numeric(r["t_value"])), "</td>",
    "<td>", fmt_num(as.numeric(r["p_value"])), " ", r["sig"], "</td>",
    "</tr>"
  )
})
html_doc <- c(
  "<html><head><style>",
  "body { font-family: Arial, sans-serif; padding: 24px; }",
  "h2 { margin-bottom: 6px; }",
  "table { border-collapse: collapse; width: 90%; font-size: 16px; }",
  "th, td { border: 1px solid #333; padding: 8px 10px; }",
  "th { background: #f2f2f2; text-align: left; }",
  "tr:nth-child(even) { background: #fafafa; }",
  ".neg { color: #b00020; font-weight: 600; }",
  ".pos { color: #004d1a; font-weight: 600; }",
  ".note { color: #555; font-size: 12px; }",
  "</style></head><body>",
  "<h2>Main Results (DiD + SCM)</h2>",
  "<table>",
  "<thead><tr><th>Model</th><th>Estimate</th><th>Std. Error</th><th>t value</th><th>p-value</th></tr></thead>",
  "<tbody>",
  html_rows,
  "</tbody></table>",
  "<p style='margin-top:8px; font-size:12px;'>Sig. codes: *** <0.001, ** <0.01, * <0.05, . <0.1</p>",
  "</body></html>"
)
writeLines(html_doc, file.path(out_dir, "main_results_table.html"))

# Controls vs no-controls table (CH vs donors log outcome)
did_ch_donors_nocntl <- feols(
  log_value ~ i(group, post, ref = "Donor") | group + quarter,
  cluster = ~ group,
  data = did_ch_donors_group %>% select(-gdp_idx)
)
did_ch_donors_nocntl_tidy <- broom::tidy(did_ch_donors_nocntl) %>%
  filter(term == "group::CH:post") %>%
  mutate(model = "No GDP control")
did_ch_donors_withcntl_tidy <- did_ch_donors_tidy %>%
  mutate(model = "With GDP control")
controls_compare <- bind_rows(did_ch_donors_nocntl_tidy, did_ch_donors_withcntl_tidy) %>%
  mutate(
    ci_low = estimate - 1.96 * std.error,
    ci_high = estimate + 1.96 * std.error
  ) %>%
  select(model, estimate, std.error, p.value, ci_low, ci_high)
write_csv(controls_compare, file.path(out_dir, "did_log_CH_donors_controls_compare.csv"))

controls_html_rows <- paste0(
  "<tr><td>", controls_compare$model, "</td>",
  "<td>", fmt_num(controls_compare$estimate), "</td>",
  "<td>", fmt_num(controls_compare$std.error), "</td>",
  "<td>", fmt_num(controls_compare$p.value), "</td>",
  "<td>", fmt_num(controls_compare$ci_low), "</td>",
  "<td>", fmt_num(controls_compare$ci_high), "</td></tr>"
)
controls_html <- c(
  "<table border=\"1\" cellspacing=\"0\" cellpadding=\"4\">",
  "<thead><tr><th>Model</th><th>Estimate</th><th>Std. Error</th><th>p-value</th><th>CI low</th><th>CI high</th></tr></thead>",
  "<tbody>",
  controls_html_rows,
  "</tbody></table>"
)
writeLines(controls_html, file.path(out_dir, "did_log_CH_donors_controls_compare.html"))

# -------------------------
# INTERPRETIVE NOTES (comments only)
# -------------------------
# SCM: CH log liabilities roughly -13% vs synthetic after 2022Q2; perm p ≈ 0.67 (suggestive, not definitive).
# Triad share: DiD ATT (CH vs HK+SG) in triad share is negative (percentage-point drop for CH); ES shows CH losing share while HK+SG gain post-2022.
# HK+SG vs other donors: estimates are noisy with CIs often covering zero; evidence for broader global share gains is weaker than triad-share result.

message("New outputs written: ", paste(new_outputs, collapse = ", "))
message("\nDone. Check the ./out/ folder for figures and CSVs.\n")
