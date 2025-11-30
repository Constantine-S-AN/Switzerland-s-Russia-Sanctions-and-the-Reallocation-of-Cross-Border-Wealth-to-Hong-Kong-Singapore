# Switzerland Sanctions → Reallocation to HK/SG

Synthetic Control is the primary design (CH vs donor centres), with supporting triad-share DiD / event-studies (CH vs HK+SG).

## Repository layout
- `Swiss Sanctions Scm Did.R` — main end-to-end script.
- `data/` — cleaned BIS quarterly liabilities:
  - `bis_ch_quarterly.csv`, `bis_hk_quarterly.csv`, `bis_sg_quarterly.csv`
  - `bis_uk_quarterly.csv`, `bis_nl_quarterly.csv`, `bis_ie_quarterly.csv`
  - `bis_lu_quarterly.csv` (Luxembourg donor, already present)
  - (Optional) Saudi Arabia: raw file `Raw_data/Saudi_Arabia_cross_lia.csv` exists; place a copy into `data/` as `Saudi_Arabia_cross_lia.csv` to enable the Saudi ES.
- `Raw_data/` — raw downloads (BIS, FX, etc.); FX controls read from `Raw_data/Exchange rate/*.csv`. Raw BIS for Luxembourg (`Raw_data/Luxembourg——liability.csv`) and Saudi (`Raw_data/Saudi_Arabia_cross_lia.csv`) are here if you need to re-clean or re-export.
- `fig/` — generated figures (PNG).
- `out/` — generated tables/diagnostics (CSV).
- `r_libs/` — local R library (auto-created).

## How to run
```sh
# From repo root (uses local library; installs to r_libs as needed)
R_LIBS_USER="$(pwd)/r_libs" R_LIBS="$(pwd)/r_libs" Rscript "Swiss Sanctions Scm Did.R"
```
Outputs will go to `fig/` (plots) and `out/` (CSVs). If `Luxembourg——liability.csv` or `Saudi_Arabia_cross_lia.csv` are absent, Luxembourg and/or Saudi analyses are skipped with warnings.

## Data expected
- BIS LBS quarterly liabilities (non-banks):
  - CH, HK, SG, GB, NL, IE, LU (present in `data/`)
  - SA optional: raw in `Raw_data/Saudi_Arabia_cross_lia.csv`; copy to `data/` to enable
  - Columns: either `TIME_PERIOD`/`OBS_VALUE` or `quarter`/`value` or `date`/`value`.
- FX controls (monthly → quarterly averages):
  - `Raw_data/Exchange rate/Swiss_Francs_to_One_USd.csv`
  - `Raw_data/Exchange rate/HK_Dollars_to_One_USDollar.csv`
  - `Raw_data/Exchange rate/Singapore_Dollars_to_One_USDollar.csv`
- Study window: 2015Q1–2025Q2; treatment: 2022Q2; index base: 2019Q4.

## Key analyses in the script
1) **Levels/index plot (CH/HK/SG)**  
   - `fig/fig1_levels_index_CH_HK_SG.png`  
   - Vertical line at 2022Q2.

2) **Triad shares (CH vs HK+SG)**  
   - Descriptive share plot: `fig/fig_triad_share_CH_vs_HKSG.png`  
   - Event-study (simple ES): `fig/fig_es_triad_CH_vs_HKSG.png`  
   - Sun–Abraham ES with CIs (CH treated at 2022Q2): `fig/fig_es_triad_CH_vs_HKSG_CI.png`  
   - Formal DiD ATT (2018Q1+): `did_triad` (printed in console). ATT is the interaction `treat_CH:post` in triad share units (percentage points).

3) **Synthetic Control (primary identification)**  
   - Donors: GB, NL, IE (CH treated), sample trimmed to 2017Q1+ pre/2022Q2 post.  
   - Main path plot: `fig/fig2_synthdid_paths_CH.png`  
   - Placebo distribution: `fig/fig3_placebo_ATT.png`, data `out/synthdid_placebo_att.csv`  
   - Permutation p-val: `out/synthdid_perm_pval.csv`  
   - RMSPE pre/post: `out/synthdid_rmspe.csv`  
   - Leave-one-out: `fig/fig3b_synthdid_leave_one_out.png`, `out/synthdid_leave_one_out.csv`.

4) **CH event-studies (supporting)**  
   - Full controls: `fig/fig6_event_study_CH.png`  
   - Donors-only: `fig/fig6b_event_study_CH_donors_only.png`.

5) **HK+SG vs donor centres (exploratory/robustness)**  
   - Share ES: `fig/fig_event_HKSG_share.png`  
   - Note: coefficients are noisy; CIs often include zero.

6) **Saudi vs donors (optional)**  
   - Skipped unless `data/Saudi_Arabia_cross_lia.csv` is present.

## Generated outputs
### Figures (PNG, in `fig/`)
- `fig1_levels_index_CH_HK_SG.png`
- `fig2_synthdid_paths_CH.png`
- `fig3_placebo_ATT.png`
- `fig3b_synthdid_leave_one_out.png`
- `fig6_event_study_CH.png`
- `fig6b_event_study_CH_donors_only.png`
- `fig_triad_share_CH_vs_HKSG.png`
- `fig_es_triad_CH_vs_HKSG.png`
- `fig_es_triad_CH_vs_HKSG_CI.png` (Sun–Abraham ES with 95% CIs)
- `fig_event_HKSG_share.png`
- (SA plot only if SA data present)

### Tables/diagnostics (CSV, in `out/`)
- `panel_quarterly_CH_HK_SG_and_donors.csv`
- `panel_quarterly_indexed.csv`
- `synthdid_placebo_att.csv`
- `synthdid_perm_pval.csv`
- `synthdid_rmspe.csv`
- `synthdid_leave_one_out.csv`

## Model notes
- SCM ATT (CH): ~ -13% post-2022Q2; perm p ~ 0.67 (suggestive, not conclusive).
- Triad DiD ATT (CH vs HK+SG share): ~ -2.5 p.p. (treat_CH:post) on 2018Q1+ sample.
- Triad ES (Sun–Abraham): event time 0 = 2022Q2; post coefficients should be negative if CH loses triad share.
- HK+SG vs other donor centres: imprecise; treat as exploratory.

## Troubleshooting
- Missing LU/SA files: SA ES skipped, LU not used as donor; add `data/Luxembourg——liability.csv` and `data/Saudi_Arabia_cross_lia.csv` to enable.
- Blank triad CI plot: ensure `fig/` exists (script creates it), and that `tidyverse`, `fixest`, `broom` load; rerun the script.
- PSD VCOV warnings from `fixest` are benign; ggplot linewidth deprecation is harmless.

## Re-running clean
```sh
rm -rf out fig
R_LIBS_USER="$(pwd)/r_libs" R_LIBS="$(pwd)/r_libs" Rscript "Swiss Sanctions Scm Did.R"
```
Outputs regenerate into `fig/` and `out/`.

## Pitch / talking points
- Primary ID: SCM for CH vs donor centres (GB, NL, IE) post-2022Q2 → CH liabilities about 13% below synthetic; p-value ~ 0.67 (suggestive).
- Triad share: CH share drops vs HK+SG; DiD ATT ~ -2.5 p.p.; ES shows negative post-2022Q2 coefficients.
- HK+SG vs broader donor centres: results noisy; patterns weaker.
