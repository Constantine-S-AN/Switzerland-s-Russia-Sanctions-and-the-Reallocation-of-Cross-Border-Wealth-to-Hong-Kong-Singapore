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
  "Synth", "synthdid", "fixest", "broom", "fwildclusterboot"
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
  GDP_CH = "Raw_data/CLVMNACSAB1GQCH.csv",
  VIX    = "Raw_data/VIX.csv"
)

ids_main   <- c("CH", "HK", "SG", "SA")         # include Saudi as a main unit we track
ids_donors <- c("GB", "NL", "IE", "ES", "FI", "BE", "SE", "NO", "DK", "IT")        # expanded donors; HK/SG excluded by design

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
read_vix <- function(path) {
  if (!file_exists(path)) return(NULL)
  df <- suppressMessages(read_csv(path, show_col_types = FALSE))
  date_col <- intersect(names(df), c("DATE", "Date", "date", "observation_date"))[1]
  val_col  <- setdiff(names(df), c("DATE", "Date", "date", "observation_date"))[1]
  if (is.na(date_col) || is.na(val_col)) return(NULL)
  df %>%
    transmute(
      quarter = as.yearqtr(as.Date(.data[[date_col]])),
      vix = as.numeric(.data[[val_col]])
    ) %>%
    group_by(quarter) %>%
    summarise(vix = mean(vix, na.rm = TRUE), .groups = "drop")
}

# Annual GDP growth files (per country) → quarterly step series
read_gdp_growth_dir <- function(dir_path) {
  if (!dir_exists(dir_path)) return(NULL)
  files <- dir_ls(dir_path, glob = "*.csv")
  if (length(files) == 0) return(NULL)

  recode_id <- function(fname) {
    base <- tools::file_path_sans_ext(path_file(fname))
    base <- sub("^GDP_", "", base)
    key  <- toupper(gsub("_", "", base))
    recode(key,
           SWITZERLAND = "CH",
           HONGKONGSARCHINA = "HK",
           SINGAPORE = "SG",
           UNITEDKINGDOM = "GB",
           NETHERLANDS = "NL",
           IRELAND = "IE",
           LUXEMBOURG = "LU",
           SAUDIARABIA = "SA",
           .default = NA_character_)
  }

  out <- purrr::map_dfr(files, function(f) {
    id <- recode_id(f)
    if (is.na(id)) return(NULL)
    df <- suppressMessages(read_csv(f, show_col_types = FALSE))
    if (!all(c("year", "GDP_growth") %in% names(df))) return(NULL)
    df %>%
      transmute(year = as.integer(year), growth = as.numeric(GDP_growth)) %>%
      drop_na(year, growth) %>%
      rowwise() %>%
      mutate(tmp = list(tibble(
        id = id,
        quarter = as.yearqtr(paste0(year, " Q", 1:4)),
        gdp_growth = growth
      ))) %>%
      ungroup() %>%
      select(tmp) %>%
      tidyr::unnest(tmp)
  })
  if (nrow(out) == 0) return(NULL)
  out
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

# Wild cluster bootstrap helper (Rademacher), safe wrapper
wild_boot <- function(model, param, cluster, B = 999, seed = 123) {
  if (!requireNamespace("fwildclusterboot", quietly = TRUE)) {
    message("[info] fwildclusterboot not available; skipping wild bootstrap for ", param)
    return(NULL)
  }
  set.seed(seed)
  res <- tryCatch(
    fwildclusterboot::boottest(
      model,
      param = param,
      clustid = cluster,
      B = B,
      type = "rademacher",
      conf_int = TRUE
    ),
    error = function(e) NULL
  )
  if (is.null(res)) return(NULL)
  tibble(
    term   = param,
    boot_p = res$p_val,
    ci_low = res$ci_low,
    ci_high = res$ci_high,
    B = B
  )
}

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
  SA = "Raw_data/Saudi_Arabia_cross_lia.csv",
  ES = "Raw_data/spain_bis.csv",
  FI = "Raw_data/Finland_bis.csv",
  BE = "Raw_data/Belgium_bis.csv",
  SE = "Raw_data/Sweden_bis.csv",
  NO = "Raw_data/Norway_bis.csv",
  DK = "Raw_data/Denmark_bis.csv",
  IT = "Raw_data/Italy_bis.csv"
)

raw_to_out <- list(
  CH = paths$CH,
  HK = paths$HK,
  SG = paths$SG,
  GB = paths$GB,
  LU = paths$LU,
  SA = paths$SA,
  ES = "data/bis_es_quarterly.csv",
  FI = "data/bis_fi_quarterly.csv",
  BE = "data/bis_be_quarterly.csv",
  SE = "data/bis_se_quarterly.csv",
  NO = "data/bis_no_quarterly.csv",
  DK = "data/bis_dk_quarterly.csv",
  IT = "data/bis_it_quarterly.csv"
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
vix_q <- read_vix(paths$VIX)
gdp_growth_q <- read_gdp_growth_dir("Raw_data/GDP growth-selected")
if (!is.null(gdp_growth_q)) {
  gdp_growth_q <- gdp_growth_q %>%
    filter(quarter >= q_start, quarter <= q_end)
  write_csv(gdp_growth_q, file.path(out_dir, "gdp_growth_quarterly_all.csv"))

  # Plot GDP growth (quarterly step) for available centers
  gdp_ids <- intersect(
    unique(gdp_growth_q$id),
    c("CH", "HK", "SG", "GB", "NL", "IE", "ES", "FI", "BE", "SE", "NO", "DK", "IT")
  )
  if (length(gdp_ids) > 0) {
    gdp_growth_plot <- gdp_growth_q %>%
      filter(id %in% gdp_ids) %>%
      group_by(id) %>%
      arrange(quarter, .by_group = TRUE) %>%
      mutate(growth_ma4 = zoo::rollmean(gdp_growth, 4, fill = NA, align = "right")) %>%
      ungroup()

    fig_gdp_growth <- gdp_growth_plot %>%
      ggplot(aes(x = as.Date(quarter), color = id)) +
      geom_line(aes(y = growth_ma4), linewidth = 1) +
      geom_point(aes(y = growth_ma4), size = 1.5, alpha = 0.7) +
      geom_vline(xintercept = as.Date(q_treat), linetype = "dashed") +
      labs(
        title = "GDP growth (4-quarter moving average)",
        subtitle = "Dashed = 2022Q2 sanctions; smoothed within each country",
        x = NULL, y = "GDP growth (%)", color = "Center"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")

    ggsave(file.path(fig_dir, "fig_gdp_growth_quarterly.png"), fig_gdp_growth, width = 9, height = 5.2, dpi = 200)

    gdp_growth_prepost <- gdp_growth_q %>%
      mutate(post = quarter >= q_treat) %>%
      group_by(id, post) %>%
      summarise(mean_growth = mean(gdp_growth, na.rm = TRUE), .groups = "drop")
    write_csv(gdp_growth_prepost, file.path(out_dir, "gdp_growth_prepost_by_id.csv"))
  }
}

###########################
# 4. Figure: levels/index
###########################
fig1 <- panel_indexed %>%
  filter(id %in% c("CH", "HK", "SG")) %>%
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
did_triad_boot <- wild_boot(did_triad, "post:treat_CH", cluster = "center")

write_csv(
  did_triad_tidy,
  file.path(out_dir, "did_triad_share_results.csv")
)
if (!is.null(did_triad_boot)) {
  write_csv(
    did_triad_boot %>% mutate(model = "Triad share CH vs HK+SG"),
    file.path(out_dir, "did_triad_share_wildboot.csv")
  )
}
writeLines(
  capture.output(print(summary(did_triad))),
  file.path(out_dir, "did_triad_share_console.txt")
)
print(summary(did_triad))
# ATT interpretation: coefficient on treat_CH:post is in share units (~percentage points of triad share).

###########################
# 4e. Event-study: CH vs HK+SG (Sun-Abraham on triad share)
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

###########################
# 5. Synthetic control (log liabilities, donors = GB/NL/IE/ES/FI/BE/SE/NO/DK/IT)
###########################
donor_set <- intersect(ids_donors, unique(panel$id))
if (length(donor_set) < 2) stop("Need at least two donor countries for SCM.")
units_scm <- c(donor_set, "CH")

panel_scm <- panel %>%
  filter(id %in% units_scm, quarter >= as.yearqtr("2018 Q1")) %>%
  select(id, quarter, log_value)

scm_mat <- panel_scm %>%
  pivot_wider(names_from = quarter, values_from = log_value) %>%
  arrange(factor(id, levels = units_scm))

Y <- as.matrix(scm_mat[, -1])
rownames(Y) <- scm_mat$id
complete_cols <- colSums(!is.na(Y)) == nrow(Y)
Y <- Y[, complete_cols, drop = FALSE]
quarters_scm <- as.yearqtr(colnames(scm_mat)[-1])[complete_cols]
T0 <- sum(quarters_scm < q_treat)
if (nrow(Y) < 2 || T0 < 2) stop("Insufficient donors or pre-period for SCM.")
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
sd_se <- if (!is.null(sd_est)) tryCatch(as.numeric(synthdid_se(sd_est, method = "jackknife")), error = function(e) NA_real_) else NA_real_
sd_p <- if (!is.na(sd_tau) && !is.na(sd_se) && sd_se != 0) 2 * pnorm(-abs(sd_tau / sd_se)) else NA_real_

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
write_csv(tibble(att = sd_tau, se = sd_se, p_value = sd_p), file.path(out_dir, "synthdid_att.csv"))

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
# 5b. Classic Synth (fixed donor set, log outcome)
###########################
synth_df <- panel %>%
  filter(id %in% units_scm, quarter %in% quarters_scm) %>%
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
  predictors = "log_value",
  predictors.op = "mean",
  special.predictors = list(
    list("log_value", tail(pre_t, 4), "mean"),
    list("log_value", tail(pre_t, 8), "mean")
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
  time_vec     <- quarters_scm

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
# 6. Pre/post means within triad only
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
write_csv(panel, file.path(out_dir, "panel_quarterly_CH_HK_SG_and_donors.csv"))
write_csv(panel_indexed, file.path(out_dir, "panel_quarterly_indexed.csv"))

new_outputs <- c(
  "out/did_triad_share_CH_HKSG.csv",
  "fig/did_triad_share_CH_HKSG.png",
  "fig/fig_es_triad_CH_vs_HKSG_CI.png",
  "fig/fig_synthdid_paths_CH.png",
  "fig/fig_synth_classic_paths.png"
)

main_results <- tibble::tibble(
  model = c(
    "Triad share DiD (CH vs HK+SG)",
    "Triad share ES (Sun-Abraham)",
    "synthdid ATT (log gap)",
    "Classic Synth gap (mean post)"
  ),
  estimate = c(
    if (nrow(did_triad_tidy) > 0) did_triad_tidy$estimate[1] else NA_real_,
    NA_real_,
    sd_tau,
    if (exists("sc_gap")) mean(sc_gap$gap[sc_gap$quarter >= q_treat], na.rm = TRUE) else NA_real_
  ),
  std_error = c(
    if (nrow(did_triad_tidy) > 0) did_triad_tidy$std.error[1] else NA_real_,
    NA_real_,
    sd_se,
    NA_real_
  ),
  p_value = c(
    if (nrow(did_triad_tidy) > 0) did_triad_tidy$p.value[1] else NA_real_,
    NA_real_,
    sd_p,
    NA_real_
  )
)
write_csv(main_results, file.path(out_dir, "main_results_summary.csv"))

message("New outputs written: ", paste(new_outputs, collapse = ", "))
message("\nDone. Check the ./out/ folder for figures and CSVs.\n")
