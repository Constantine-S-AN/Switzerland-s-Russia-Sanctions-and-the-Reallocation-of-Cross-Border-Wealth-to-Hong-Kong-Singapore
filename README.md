* explicitly documents **`Dataset/`**, **`bis_CH.csv`**, and **`Rplots.pdf`**
* clarifies what **`fig_py/`** and **`out_py/`** are for
* slightly reorganizes things so someone new can understand the project end-to-end

````markdown
# Switzerland Sanctions → Wealth Reallocation (CH ↓ vs HK/SG ↑)

This repo asks whether Switzerland’s adoption of EU Russia sanctions around **2022Q2** led to a *relative* decline in cross-border bank liabilities booked in Switzerland, with corresponding gains in **Hong Kong** and **Singapore**, after accounting for global conditions.

- **Main outcome:** cross-border liabilities to non-banks (BIS LBS).
- **Main comparison:** Switzerland (CH) vs Hong Kong + Singapore (HK/SG) and vs European donor centres.
- **Main designs:**  
  - Synthetic Control (SCM) for CH vs donors  
  - DiD + event-study models (CH vs HK/SG and CH vs donors, with FX/GDP controls)

---

## Research question & hypotheses

- **Q:** After Switzerland joined EU Russia sanctions (late Feb 2022), did foreign client money *reallocate* from Swiss banks toward Hong Kong and Singapore?
- **H₀:** CH, HK, and SG move in parallel after 2022 once global factors are controlled for.  
- **H₁:** CH liabilities fall and HK/SG liabilities rise **relative to pre-trend and peer centres** after 2022.

---

## Quick start

From the repo root:

```bash
# R: main pipeline (SCM + DiD/ES + figures/tables)
R_LIBS_USER="$(pwd)/r_libs" R_LIBS="$(pwd)/r_libs" Rscript "Swiss Sanctions Scm Did.R"

# Optional: Python robustness checks (extra DiD/ES, plots, tables)
python analysis_py.py
````

* The R script will install any missing packages into `./r_libs` on the first run.
* Main R outputs go to **`fig/`** (PNGs) and **`out/`** (CSVs/HTML/TeX).
* Python robustness outputs go to **`fig_py/`** and **`out_py/`**.
* Tested on **R 4.5.x** and a recent Python 3.x.

---

## Data

All data needed to reproduce the current results are included in the repo.

* **BIS LBS liabilities (non-banks), quarterly** — CH, HK, SG, GB, NL, IE, LU, SA

  * Cleaned, analysis-ready series live in **`data/`**.
  * Original raw BIS CSVs live in **`Raw_data/`** and are auto-parsed if you drop in newer vintages (columns `TIME_PERIOD` / `OBS_VALUE`).
* **FX controls (monthly → quarterly averages)**

  * `Raw_data/Exchange rate/*.csv` (CHF, HKD, SGD, LU, SA vs USD).
* **GDP (Switzerland), quarterly**

  * `Raw_data/CLVMNACSAB1GQCH.csv` (Eurostat/FRED, chained 2010 EUR), used to build a GDP index (`gdp_idx`, 2019Q4 = 100).
* **Ancillary series**

  * SG liabilities/loans, Swiss deposit series, Saudi Arabia liabilities, etc., stored alongside the main BIS series.
* **Study window / treatment:**

  * Time span: **2015Q1–2025Q2**
  * Treatment quarter: **2022Q2**
  * Index base: **2019Q4 = 100**

### Dataset snapshots

* **`Dataset/`** contains *frozen snapshots* of key cleaned/aggregated datasets (e.g., panel-level CSVs) used for analysis and slides.
  You can inspect these directly without running the scripts; they broadly mirror what the R pipeline writes into `out/`.

---

## Key variables

Core variables in the panel datasets:

* **Outcomes**

  * `value` — level of cross-border liabilities
  * `log_value` — log liabilities
  * `index` — liabilities indexed to 2019Q4 = 100
  * `triad_share` — CH/HK/SG share of the CH–HK–SG “triad”
  * `global_share` — share of total liabilities across all centres in the sample

* **Treatment / time**

  * `treated` — 1 for CH, 0 otherwise
  * `post` — 1 for quarters ≥ 2022Q2 (post-sanctions)
  * Event-time dummies for event-study plots
  * `post_placebo` — placebo “treatment” from 2019Q4 for timing placebos

* **Controls**

  * FX series: `ex_ch`, `ex_hk`, `ex_sg`, `ex_lu`, `ex_sa`
  * CH GDP index: `gdp_idx` (2019Q4 = 100)

* **SCM weights / paths**

  * `out/synthdid_donor_weights.csv` — donor weights for SCM
  * `out/synthdid_time_weights.csv` — time weights for SCM
  * `out/synthdid_paths_gap.csv` — treated vs synthetic paths and gaps

---

## Code & repository layout

### Main code

* **`Swiss Sanctions Scm Did.R`**
  End-to-end R pipeline:

  * Reads/cleans BIS, FX, and GDP series
  * Builds CH/HK/SG + donors panels
  * Runs Synthetic Control (synthdid) for CH vs donors
  * Runs DiD and event-study specifications (CH vs HK/SG, CH vs donors)
  * Produces plots and tables into `fig/` and `out/`

* **`analysis_py.py`**
  Python robustness script:

  * Re-implements selected DiD and event-study models (triad/global/log outcomes, placebos)
  * Writes additional diagnostic plots and tables into `fig_py/` and `out_py/`

* **`r_libs/`** (auto-created)
  Local R library directory used when you run the pipeline with `R_LIBS_USER="$(pwd)/r_libs"`; not required to be version-controlled.

### Top-level folders

* **`Dataset/`** — snapshot CSVs / panels used for quick inspection and replication (does not have to be regenerated each time).
* **`Raw_data/`** — original BIS/FX/GDP and related raw downloads. Safe to overwrite with newer BIS vintages if you want updated results.
* **`data/`** — cleaned, long-format quarterly series used as inputs to the R and Python scripts.
* **`fig/`** — main R figures (PNG) used in the slide deck / report.
* **`out/`** — main R tables and diagnostics (CSV/HTML/TeX).
* **`fig_py/`** — Python robustness figures (PNG).
* **`out_py/`** — Python robustness tables/diagnostics (CSV, etc.).

### Misc files

* **`bis_CH.csv`** — quick single-series export for Switzerland used during early prototyping; kept as a simple sanity-check time series.
* **`Rplots.pdf`** — auto-generated multi-plot PDF from an interactive R session; not used in the final analysis, but left as an example of exploratory plotting.
* **`.gitignore`** — standard ignore rules (e.g., for local libs, temp files).

---

## Outputs (high level)

The list below summarizes the most important outputs. File names may be useful for mapping to specific slides or tables.

### 1. SCM (CH vs donors)

* `fig/fig2_synthdid_paths_CH.png` — treated vs synthetic CH liabilities over time
* `fig/fig2c_synthdid_gap_CH.png` — gap (CH − synthetic) over time
* `fig/fig3_placebo_ATT.png` — distribution of placebo ATTs across donor centres
* `fig/fig3b_synthdid_leave_one_out.png` — leave-one-out SCM robustness
* Weights and paths:

  * `out/synthdid_donor_weights.csv`
  * `out/synthdid_time_weights.csv`
  * `out/synthdid_paths_gap.csv`

### 2. Triad DiD / ES (CH vs HK+SG)

* `fig/fig_triad_share_CH_vs_HKSG.png` — descriptive CH vs HK+SG triad share plot
* `fig/fig_es_triad_CH_vs_HKSG.png` — simple event-study
* `fig/fig_es_triad_CH_vs_HKSG_CI.png` — Sun–Abraham ES with 95% CIs
* `out/did_triad_share_results.csv` — DiD estimates (including ATT on `treated × post`)

### 3. CH vs donors DiD (log outcomes, +GDP)

* `fig/fig_did_coefficients.png` — coefficient plots for CH vs donors (log liabilities)
* `out/did_log_CH_vs_donors_results.csv` — “automatic” donor set
* `out/did_log_CH_vs_manual_donors_results.csv` — fixed/manual donor set
* `fig/fig_did_placebo_timing_CH.png` — placebo-timing check (2019Q4 “fake” treatment)

### 4. Event-study plots

* `fig/fig6_event_study_CH.png` — CH ES with full controls
* `fig/fig6b_event_study_CH_donors_only.png` — CH ES with donors only
* `fig/fig_event_HKSG_share.png` — HK+SG vs donors ES
* `fig/fig_event_SA_vs_donors.png` — Saudi Arabia vs donors ES
* `out/pretrend_tests.csv` — numeric pre-trend tests; p-values are also annotated in the plots

### 5. GDP overlay

* `fig/fig_gdp_liab_CH.png` — CH liabilities vs CH GDP index
* `out/gdp_liab_summary_CH.csv` — summary statistics for the overlay

### 6. Tables / panels

* `out/main_results_table.html` / `.tex` — main regression summary table
* `out/did_log_CH_donors_controls_compare.html` / `.csv` — DiD with vs without controls
* `out/panel_quarterly_CH_HK_SG_and_donors.csv` — main panel used in regressions
* `out/panel_quarterly_indexed.csv` — indexed version (2019Q4=100)

Python robustness scripts may produce additional outputs in `fig_py/` and `out_py/` with similar naming conventions.

---

## Headline findings (current run)

The numbers below summarize the results at the current commit. Using different BIS vintages or alternative donor sets may shift magnitudes slightly.

* **SCM (CH vs donors)**
  Post-2022Q2 **log ATT ≈ −7%** (CH below synthetic). Permutation p-value ≈ 0.5, so suggestive but not statistically decisive.

* **Triad share DiD (CH vs HK+SG)**
  CH’s share of the CH–HK–SG triad falls by roughly **−3.4 percentage points** in the post-period. Event-study coefficients for CH after 2022 are consistently negative.

* **CH vs donors DiD (log, +GDP controls)**
  The `CH × post` coefficient is negative and remains so under different donor sets. Including GDP as a control does not remove the negative post-sanctions effect.

* **Placebo checks**
  Donor-centre placebos and placebo timing (treating 2019Q4 as “treatment”) show much smaller effects than the actual CH post-2022 pattern.

Taken together, the evidence points toward a **relative weakening of CH liabilities** after sanctions, with **HK/SG picking up some share**, though results are moderate in size and not always statistically sharp.

---

## Repo layout (summary)

* `Dataset/` — frozen CSVs / panels (quick inspection, no rerun needed)
* `Raw_data/` — raw BIS/FX/GDP & ancillary series
* `data/` — cleaned analysis-ready time series
* `fig/`, `out/` — main R plots and tables
* `fig_py/`, `out_py/` — Python robustness plots and tables
* `Swiss Sanctions Scm Did.R` — main R pipeline
* `analysis_py.py` — Python robustness
* `bis_CH.csv`, `Rplots.pdf` — legacy / exploratory artefacts
* `r_libs/` — local R library folder (auto-created, usually git-ignored)

---

## Troubleshooting

* **PSD VCOV / fixest warnings:**
  Some variance-covariance warnings (PSD, etc.) are expected and benign given clustered/robust options.

* **ggplot linewidth deprecation warnings:**
  Harmless; they come from upstream package changes and do not affect results.

* **Blank or missing plots:**

  * Make sure `fig/` (and `fig_py/` if using Python) exist; the R script creates directories as needed.
  * If things look stale, delete `fig/` and `out/` (and `fig_py/`, `out_py/`), then rerun the scripts.
