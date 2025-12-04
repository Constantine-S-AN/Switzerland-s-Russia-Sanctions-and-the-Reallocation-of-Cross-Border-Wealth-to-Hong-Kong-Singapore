# Switzerland Sanctions → Wealth Reallocation (CH ↓ vs HK/SG ↑)

This repo analyzes whether Switzerland’s adoption of EU Russia sanctions (2022Q2) led to a reallocation of cross‑border liabilities away from Switzerland toward Hong Kong/Singapore. Primary designs: Synthetic Control (SCM) for CH vs donors, and DiD/Event-studies for CH vs HK+SG and CH vs donors (with GDP/FX controls).

## Quick start
```sh
# From repo root; installs packages into ./r_libs if missing
R_LIBS_USER="$(pwd)/r_libs" R_LIBS="$(pwd)/r_libs" Rscript "Swiss Sanctions Scm Did.R"

# Optional Python robustness
python analysis_py.py
```
Outputs regenerate into `fig/` (PNGs) and `out/` (CSVs). Tested on R 4.5.x.

## Data
- **BIS LBS liabilities (non‑banks), quarterly** — CH, HK, SG, GB, NL, IE, LU, SA. Cleaned in `data/`. Raw BIS in `Raw_data/` (auto-parsed if newer; columns TIME_PERIOD/OBS_VALUE).
- **FX controls (monthly → quarterly averages)** — `Raw_data/Exchange rate/*.csv` (CHF, HKD, SGD, LU, SA vs USD).
- **GDP (CH), quarterly** — `Raw_data/CLVMNACSAB1GQCH.csv` (Eurostat/FRED, chained 2010 EUR).
- **Ancillary** — SG liabilities/loans, Swiss deposits, Saudi series.
- **Study window** — 2015Q1–2025Q2. **Treatment** — 2022Q2. **Index base** — 2019Q4.

## Key variables
- **Outcomes:** `value` (liabilities), `log_value`, `index` (2019Q4=100), `triad_share` (CH/HK/SG share of triad), `global_share` (share of total across all centers).
- **Treatment/time:** `treated` (CH=1), `post` (≥2022Q2), event‑time dummies for ES; placebo timing `post_placebo` (2019Q4).
- **Controls:** FX series (`ex_ch`, `ex_hk`, `ex_sg`, `ex_lu`, `ex_sa`); CH GDP index (`gdp_idx`, 2019Q4=100).
- **SCM weights:** `out/synthdid_donor_weights.csv`, `out/synthdid_time_weights.csv`; paths/gaps `out/synthdid_paths_gap.csv`.

## Code
- `Swiss Sanctions Scm Did.R` — full R pipeline (SCM, DiD/ES, placebos, GDP/FX controls, plots/tables).
- `analysis_py.py` — Python DiD robustness (triad/global/log outcomes, placebos).
- `r_libs/` — local R library (auto-created).

## Outputs (high level)
- **SCM:** `fig/fig2_synthdid_paths_CH.png`, `fig/fig2c_synthdid_gap_CH.png`, `fig/fig3_placebo_ATT.png`, `fig/fig3b_synthdid_leave_one_out.png`, weights/time weights CSVs.
- **Triad DiD/ES (CH vs HK+SG):** `fig/fig_triad_share_CH_vs_HKSG.png`, `fig/fig_es_triad_CH_vs_HKSG.png`, `fig/fig_es_triad_CH_vs_HKSG_CI.png`, coefficients `out/did_triad_share_results.csv`.
- **CH vs donors DiD (log, +GDP):** `fig/fig_did_coefficients.png`, auto/fixed donors `out/did_log_CH_vs_donors_results.csv`, `out/did_log_CH_vs_manual_donors_results.csv`; placebo timing `fig/fig_did_placebo_timing_CH.png`.
- **Event studies:** `fig/fig6_event_study_CH.png`, `fig/fig6b_event_study_CH_donors_only.png`, HK+SG vs donors `fig/fig_event_HKSG_share.png`, SA vs donors `fig/fig_event_SA_vs_donors.png`. Pretrend p‑values annotated and in `out/pretrend_tests.csv`.
- **GDP overlay:** `fig/fig_gdp_liab_CH.png`, `out/gdp_liab_summary_CH.csv`.
- **Tables:** main results `out/main_results_table.html/.tex`, controls vs no‑controls `out/did_log_CH_donors_controls_compare.html/.csv`, panels `out/panel_quarterly_CH_HK_SG_and_donors.csv`, `out/panel_quarterly_indexed.csv`.

## Headline findings (current run)
- **SCM (CH vs donors):** post‑2022Q2 ATT ≈ –7% log gap; permutation p ≈ 0.5 (suggestive, not definitive).
- **Triad share DiD (CH vs HK+SG):** CH share drops (~ –3.4 p.p. point estimate); ES post coefficients negative.
- **CH vs donors DiD (log, +GDP):** negative CH×post; robust to fixed donor set. GDP control included.
- **Placebos:** donor placebos and placebo timing (2019Q4) show much smaller effects than CH post‑2022.

## Repo layout
- `data/` cleaned BIS; `Raw_data/` raw BIS/FX/GDP; `fig/` plots; `out/` tables; `fig_py/`, `out_py/` for Python outputs.

## Troubleshooting
- PSD VCOV warnings are benign; ggplot linewidth deprecation is harmless.
- If plots look blank, ensure `fig/` exists (the script creates it) and rerun with the commands above.
- To regenerate clean: delete `fig/` and `out/`, rerun scripts.
