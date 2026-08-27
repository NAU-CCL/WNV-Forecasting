# WNV-Forecasting — Code Guide

This document is the detailed companion to `README.md`. It explains, script by script, what the code does, which data files it reads, what it writes, and how the pieces fit together, for reproducibility.

The code accompanies the manuscript *Evaluating the roles of weather and bird dynamics in accurately forecasting West Nile virus infection in mosquitoes and humans* (Oshinubi et al.). Preprint: **[link to be added when the preprint is posted](PREPRINT_URL_PLACEHOLDER)**. §11.2–11.4 map every figure in the main text and the supplement to the script and section that produces it.

**Contents**

1. [What the project does](#1-what-the-project-does)
2. [Pipeline](#2-pipeline)
3. [Running the code](#3-running-the-code)
4. [The `datasets/` folder](#4-the-datasets-folder)
5. [How the three surveillance targets are built from the data](#5-how-the-three-surveillance-targets-are-built-from-the-data)
6. [The four mechanistic model scripts](#6-the-four-mechanistic-model-scripts)
7. [`Baseline_model.R`](#7-baseline_modelr)
8. [`Fit_WIS.R`](#8-fit_wisr)
9. [`Ensemble_RelWIS.R`](#9-ensemble_relwisr)
10. [The `outputs/` folder](#10-the-outputs-folder)
11. [Figures produced by the scripts](#11-figures-produced-by-the-scripts)
    - 11.1 [Figure families](#111-figure-families)
    - 11.2 [Main-text figures → code](#112-main-text-figures--code)
    - 11.3 [Supplementary figures → code](#113-supplementary-figures--code)
    - 11.4 [Supplementary tables and numbers quoted in the text](#114-supplementary-tables-and-numbers-quoted-in-the-text)
12. [Known quirks and things to watch for](#12-known-quirks-and-things-to-watch-for)
13. [Glossary of recurring variable names](#13-glossary-of-recurring-variable-names)

---

## 1. What the project does

The repository fits and evaluates weekly forecasts of West Nile virus (WNV) activity in Maricopa County, Arizona, for the years 2006–2019 and 2021 (2020 is excluded from the study, see §12). Three surveillance targets are tracked each week:

| Target (name used in code) | Meaning | Observation vector |
|---|---|---|
| `total_abundance` | Total *Culex* mosquitoes caught in the county trap network that week | `X_obs1` |
| `infectious_per_1000` | Estimated infected mosquitoes per 1,000 tested (IM1000) | `X_obs2` |
| `human_cases` | **Cumulative** human WNV cases | `X0_obs` |

Four mechanistic ordinary-differential-equation (ODE) models of WNV transmission are fit to those targets with an Ensemble Kalman Filter (EnKF). The models differ along two axes: whether an avian (bird) reservoir is represented explicitly, and whether mosquito recruitment is forced by daily temperature and precipitation ("weather"/"climate"). For each year, every model produces (i) an in-sample fit and (ii) rolling 1- and 2-week-ahead probabilistic forecasts issued at 46 weekly forecast origins. Forecast quality is scored with the Weighted Interval Score (WIS) and compared, on a log relative-WIS scale, against a statistical baseline (`epipredict`'s CDC-style baseline) and against five ensembles of the four models.

## 2. Pipeline

```
datasets/*.csv
     │
     ├──► FullModel.R          ─┐
     ├──► FullModel_NoW.R       │  EnKF fit + rolling forecasts, one script per model
     ├──► Mosq_Human.R          │  (each loops over Year = 2006..2019, 2021)
     ├──► Mosq_Human_NoW.R     ─┘
     │        │
     │        ├── save_ensemble_full_global_<model>_<Year>.rds   (posterior ensemble, every week)  ──► Fit_WIS.R
     │        ├── results_<model>_<Year>.rds                     (forecast quantiles, 46 origins)  ──► Ensemble_RelWIS.R
     │        └── wis_all_<model>_<Year>.rds                     (forecast WIS per week)           ──► Ensemble_RelWIS.R
     │
     └──► Baseline_model.R
              ├── results_BaselineModel_<Year>.rds   (same format as results_<model>)
              └── wis_all_BaselineModel_<Year>.rds   (same format as wis_all_<model>)              ──► Ensemble_RelWIS.R

Fit_WIS.R          ──► fit_results_<model>_<Year>.rds, fit_wis_all_<model>_<Year>.rds, fit-quality figures
Ensemble_RelWIS.R  ──► ensemble_results_alldata_<Year>.rds, linear_pool_results_<Year>.rds,
                       all_wis_longE_<Year>.rds, all_rel_longE_<Year>.rds, relative-WIS figures
```

`<model>` is one of `FullModel`, `FullModel_NoClimate`, `Mosq+Human+Climate`, `Mosq+Human+NoClimate` (note the `+` characters in file names).

**Run order.** The four model scripts and `Baseline_model.R` are independent of one another and can be run in any order (or in parallel). `Fit_WIS.R` needs the `save_ensemble_full_global_*` files from all four model scripts. `Ensemble_RelWIS.R` needs `results_*` and `wis_all_*` from all four model scripts **and** from `Baseline_model.R`.

## 3. Running the code

**Working directory.** Every script reads inputs as `./<file>.csv` and writes outputs as bare file names. There is no `setwd()`. Before running, set the R working directory to a folder that contains the CSVs (i.e. copy or symlink the contents of `datasets/` there, or `setwd("datasets")` and let outputs land there). The saved `.rds` files are later re-read by bare name, so the same working directory must be used throughout. The multi-year panel figures are written to `./figures/`, which the scripts create.

**Packages.** Across the scripts: `tidyverse` (dplyr, tidyr, purrr, ggplot2, readr, tibble, lubridate), `MASS` (`ginv`, `mvrnorm`), `epipredict` and `epiprocess` (CMU Delphi; `quantile_pred`, `weighted_interval_score`, `cdc_baseline_forecaster`, `as_epi_df`), `distributional`, `gridExtra`, `grid`, `scales`, `nloptr` (ensemble weight optimisation), `patchwork` (figure stacking), `withr`, `checkmate`, `cli`, `RColorBrewer`. `readxl`, `mgcv` and `ciTools` are loaded by the model scripts but not used. `Baseline_model.R` installs `epiprocess`/`epipredict` from GitHub with `pak::pkg_install()` (lines 8 and 10) and `Ensemble_RelWIS.R` calls `install.packages("patchwork")` (line 3) every time they are run — comment those lines out once the packages are installed.

**Selecting years and models.** Each model script and the baseline/evaluation scripts start with

```r
years_to_run <- c(2006:2019, 2021)
for (Year in years_to_run) { ... }
```

Change `years_to_run` to run a subset (e.g. `c(2014)`). The `if (Year == 2021) {...} else if (Year == 2020) {...} ...` chain that follows selects the right CSV files and week padding for that year (§5). To run a different model you run a different script; there is no model switch inside a script.

**Runtime.** Each year of a mechanistic model propagates `N = 8000` ensemble members through a daily Euler scheme for 50 assimilation weeks and then generates 1,000-member two-week forecasts at 46 origins. 

**Recommended way to run.** The scripts are long and were written to be run interactively section by section in RStudio. Each script is self-contained in the sense that anything it needs from another script is passed through a saved `.rds` file rather than through the R session (the hand-off files are listed in §10.1). That fixes the order in which the figure sections can be run:

1. the four model scripts and `Baseline_model.R` (any order) — the year loops, then their figure sections; `FullModel.R`'s 2014 three-panel section writes `final_fit_2014_FullModel.rds` and `final_combined_2014_FullModel.rds`, and `Baseline_model.R`'s last section writes `baseline_joinWIS.rds`;
2. `Fit_WIS.R` — its year loops, then the boxplot section, which writes `median_plot_fit.rds`;
3. `Ensemble_RelWIS.R` — its year loop, then the summary sections, which write `summary_stats_log_relWIS.rds`;
4. the optional analysis blocks at the end of `Mosq_Human.R` and `Mosq_Human_NoW.R`, which read `summary_stats_log_relWIS.rds`.

Within a script, always run the year loop before the sections that follow it.

## 4. The `datasets/` folder

All files are for Maricopa County. Daily weather series are county-wide means of PRISM grid cells; the header of each weather file is the literal R expression that produced it (`rowMeans(combined_temp, na.rm = TRUE)` etc.).

### 4.1 Daily temperature (°C) and precipitation

| File | Rows | Covers | Column(s) | Used for years |
|---|---|---|---|---|
| `county_TEMP_2006-2012.csv` | 2557 | 1 Jan 2006 – 31 Dec 2012 | `rowMeans.combined_temp..na.rm...TRUE.`, `Year` | 2006–2012 (`filter(Year == <yr>)`) |
| `county_PRCP_2006-2012.csv` | 2557 | same | `rowMeans.combined_p..na.rm...TRUE.`, `Year` | 2006–2012 |
| `County_TEMP_2013.csv` / `County_PRCP_2013.csv` | 365 | 2013 | `row_means` | 2013 |
| `county_temp.csv` / `county_prcp.csv` | 1096 | 2014–2016 | `rowMeans(...)` | 2014 (rows 1:365), 2015 (366:730), 2016 (731:1096) |
| `County_TEMP_2017.csv` / `County_PRCP_2017.csv` | 730 | 2017 (+ extra) | `row_means` | 2017 (rows 1:365) |
| `county_temp_2018-2019.csv` / `county_prcp_2018-2019.csv` | 730 | 2018–2019 | `rowMeans(...)` | 2018 (1:365), 2019 (366:730) |
| `county_temp_2020-2024.csv` | 1827 | 2020–2024 | `rowMeans(...)` | 2020 (1:366), 2021 (367:731) |
| `county_prcp_2020.csv` | 366 | 2020 | `rowMeans(...)` | 2020 |
| `county_prcp_2021-2024.csv` | 1461 | 2021–2024 | `rowMeans(...)` | 2021 (1:365) |

In the scripts the selected daily slices become `inputTem_i` (temperature) and `inputP_i` (precipitation), each 365 or 366 values long. Only the two *weather-forced* models use them; the `NoW`/`NoClimate`/`NoWeather` scripts, the baseline, and the evaluation scripts still load them but never use them.

### 4.2 Mosquito surveillance

| File | Rows | Covers | Key columns | Used for years |
|---|---|---|---|---|
| `mosq_pools_data.csv` | 618 | weekly, 2006–2017 | `LabWeek`, `Tot_Mosq_Abund`, `N_Pools`, `Tot_Mosq_Sampled`, `Mean_Mosq_Per_Pool`, `N_Pos_Pools`, `Prop_Pos_Pools`, `MIR`, `I_M`, `Inf_Mosq_Per_1000`, `Year`, `date`, `Date` | 2006–2017 (`filter(Year == <yr>)`) |
| `mosq_pools_data_2018-2019.csv` | 61 | 2018 (and a few 2019 rows) | pool-level columns (`Species`, `Trap.ID`, `LabDate`, `Pathogen`, `Females`, `MPP`, `Result`) **plus** the same weekly aggregate columns | 2018 (`filter(Year == 2018)`) |
| `temp_csv_2018.csv` | 21 | 2018 lab weeks 2–22 | `LabWeek`, `Tot_Mosq_Abund` | 2018 — early-season abundance that is prepended to the 2018 series |
| `mosq_pools_data_2019.csv` | 50 | 2019 | same layout as the 2018–2019 file | 2019 |
| `mosq_pools_agg_2020.csv`, `mosq_pools_agg_2021.csv` | 52 | 2020, 2021 | `Tot_Mosq_Abund` only | 2020, 2021 (abundance) |
| `mosq_pools_data_2020.csv`, `mosq_pools_data_2021.csv` | 52 | 2020, 2021 | `Inf_Mosq_Per_1000` only | 2020, 2021 (infection) |

Column meanings: `Tot_Mosq_Abund` is the weekly count of female *Culex* collected; `N_Pools`/`Tot_Mosq_Sampled` are pools tested and mosquitoes in them; `N_Pos_Pools` is WNV-positive pools; `MIR` is the minimum infection rate per 1,000 (`N_Pos_Pools / Tot_Mosq_Sampled * 1000`); `I_M` is the estimated infection prevalence (proportion) and `Inf_Mosq_Per_1000 = I_M * 1000` is the IM1000 target used by the models.

### 4.3 Human cases and population

| File | Rows | Covers | Columns | Used for years |
|---|---|---|---|---|
| `WNV_humans_summary3_2006-2017.csv` | 635 | weekly, 2006–2017 | `YEAR`, `week_start` (m/d/yy), `cases` | 2006–2017 |
| `WNV_humans_summary3_2018-2019.csv` | 105 | 2018–2019 | same | 2018, 2019 |
| `WNV_humans_summary3.csv` | 105 | 2020–2021 | same | 2020, 2021 |
| `maricopa_population_2006-2021.csv` | 16 | annual | `year`, `county`, `population`, `margin_of_error` | all — gives the human population `Nh` used to initialise `Sh` |

`cases` is weekly incidence; the scripts convert it to a cumulative count (§5).

## 5. How the three surveillance targets are built from the data

The first ~550 lines of every script are the same year-selection block. For the chosen `Year` it produces:

* `inputTem_i`, `inputP_i` — daily temperature and precipitation (see §4.1).
* `X_obs` → `X_obs1` — weekly `Tot_Mosq_Abund`.
* `X2_obs` → `X_obs2` — weekly `Inf_Mosq_Per_1000`.
* `X3_obs` → `X0_obs` — weekly human `cases` for the year, then `cumsum()` so the target is **cumulative cases**. (In `Baseline_model.R` the weekly incidence is kept as `X0_obs` for forecasting and the cumulative series is `X4_obs`; see §7.)
* `training_start_date` = 1 January; `all_weeks <- seq(training_start_date, by = "weeks", length.out = 52)`; `observed_dates <- all_weeks`.
* `forecast_1week_dates <- all_weeks[6:51]` and `forecast_2week_dates <- all_weeks[7:52]` — the 46 target dates for 1- and 2-week-ahead forecasts.

Because the raw mosquito files do not always contain exactly 52 lab weeks, each year branch zero-pads the series to a 52-week calendar. The padding differs by year and is hard-coded, e.g. `c(X_obs, 0)` for 2011/2013/2014/2015, `c(X_obs, 0, 0)` for 2007/2017, `c(0, X_obs, 0)` for 2019, and for 2018 `c(0, X_obs0, X_obs, 0)` where `X_obs0` comes from `temp_csv_2018.csv` (infection is padded with `c(rep(0, 22), X2_obs, 0)`). Human cases usually drop the first row of the year (`X3_obs[-1, ]`), except 2006 and 2012 where it is kept, 2018 which keeps rows 1:52, and 2019 where a stray first `1` is set to `0`. The multi-year figure sections at the end of the scripts re-implement this loader as `load_year_data(Year)` and add a `pad52()` helper that forces exactly 52 values.

**Timeline conventions used everywhere.** Weeks are indexed 1…52 from 1 January. The EnKF assimilates weeks 1…50 (`total_time_points <- 50`). Forecast iteration `i` (1…46) is issued after assimilating week `i + 4`; its 1-week-ahead target is week `i + 5` and its 2-week-ahead target is week `i + 6`. Hence `actual_idx <- i + 5 + horizon - 1` in all WIS code.

## 6. The four mechanistic model scripts

`FullModel.R`, `FullModel_NoW.R`, `Mosq_Human.R` and `Mosq_Human_NoW.R` share one skeleton; the differences are the compartments, the mosquito recruitment term, and the set of estimated parameters. This section describes `FullModel.R` in full and then the differences.

### 6.1 Model 1 — `FullModel.R` (mosquito + bird + human, weather-forced)

**Compartments** (rows 1–8 of the ensemble state): `Sm`, `Im` (susceptible / infectious mosquitoes), `Sb`, `Ib`, `Rb` (susceptible / infectious / recovered birds), `Sh`, `Eh`, `Ih` (susceptible / exposed / infected humans; `Ih` accumulates and is compared with cumulative cases).

**Time-varying (OU) parameters** (rows 9–10, stored as logs): `Vm_t`, the mosquito recruitment scaling; `r_t`, a transmission/contact scaling applied to every infection term.

**Static parameters** (rows 11–22 in this order, one value per ensemble member, estimated by the filter): `Tmi`, `Tma` (lower / upper temperature limits for mosquito recruitment), `alpha`, `phi` (precipitation logistic), `Tmb` (bird→mosquito transmission), `d` (day-of-year centre of the bird birth pulse), `Tbm` (mosquito→bird transmission), `gamma` (bird recovery), `tau` (width of the bird birth pulse), `Vb` (its amplitude), `Tmh` (mosquito→human transmission), `psi` (human incubation rate). The same order is used in `param_names` and by `extract_tmin_tmax()` in `Mosq_Human.R`, which reads `Tmi`/`Tma` from rows 11–12 of the saved ensemble array.

**Fixed constants:** `N <- 8000` ensemble members; `mu <- 0.05` (daily mosquito death); `mu1 <- 1/120` (daily bird death); `dt <- 1` day; `brim` — a daily importation of infectious mosquitoes of 0.01/day between day `i_year[Year]` and day 180 (`i_year` is a per-year start day: 100 for 2006–2011, 2014, 2018; 25 for 2012–2013; 50 for 2015–2017, 2019–2021).

**Model equations** (`WNV_model()`, forward Euler with a daily step, run for the 7 days of a week; `T` and `P` are that day's temperature and precipitation, `t` the day of year):

```
λ_mb = r_t · Tmb · Ib · Sm / Nb          (bird → mosquito;  0 if Nb = 0)
λ_bm = r_t · Tbm · Im · Sb / Nb          (mosquito → bird)
λ_mh = r_t · Tmh · Im · Sh / Nh          (mosquito → human; 0 if Nh = 0)

dSm = Vm_t · [ −(T − Tmi)(T − Tma) ] · 1 / (1 + exp(alpha − phi·P))  − λ_mb − mu·Sm
dIm = λ_mb − mu·Im + brim(t)
dSb = 10·Vb · exp( −(t − d)² / (2·tau) ) − λ_bm − mu1·Sb
dIb = λ_bm − (mu1 + gamma)·Ib
dRb = gamma·Ib − mu1·Rb
dSh = −λ_mh
dEh =  λ_mh − psi·Eh
dIh =  psi·Eh
```

All states are floored at 0 after each step. Weather enters only through mosquito recruitment: the quadratic term is positive between `Tmi` and `Tma` and the logistic term increases with precipitation. During fitting, `inputTem()` replaces any daily temperature outside `[Tmi, Tma]` (or NA) by `Tmi`, which zeroes recruitment that day; during forecasting the 7-day temperature vector is passed straight through without this clamp, so the quadratic can go negative and recruitment is simply floored at zero by the `pmax()` step. Bird births are a Gaussian pulse in day-of-year. There is no temperature-dependent extrinsic incubation or mortality.

**Ornstein–Uhlenbeck propagation** (`OUproc_func`). `Vm_t` and `r_t` evolve daily on the log scale:

```
log x(t+1) = log x(t)·e^(−λ) + μ·(1 − e^(−λ)) + σ·sqrt(1 − e^(−2λ))·Z,   Z ~ N(0,1)
```

with `ou_lambda = 1/14` for both. For `Vm_t`, `μ = log(1)` and `σ = sigma_vm_year[Year]` (1.10 or 1.50 depending on the year; 2.00 for 2021). For `r_t` the process switches regime at a per-year week `obs_year[Year]` (18 for 2006–2011, 2014, 2018; 5 for 2012–2013; 10 for 2015–2017, 2019–2021): before that week `σ = 0.002`, `μ = log(0.001)` (transmission effectively off); from that week `σ = 1.50`, `μ = log(0.05)`. The regime in force each week is recorded in `sigma_t_history`.

**Initial ensemble.** `Sm = 7`, `Im = Sb = Ib = Rb = Eh = Ih = 0`, `Sh = Nh` (county population for the year). `Vm_t ~ U(0.0001, 2)`, `r_t ~ U(0.0001, 0.001)`. Static-parameter priors (uniform): `Tmi` 17–20, `Tma` 42–48, `alpha` 0.7–1.8, `phi` 0.95–1.8, `Tmb` 0.14–0.55, `d` 180–200, `Tbm` 0.14–0.55, `gamma` 0.1–0.2, `tau` 300–420, `Vb` 0.1–0.9, `Tmh` 0.014–0.055, `psi` 0.05–0.14.

**EnKF loop** (`for (iteration in 1:num_iterations)`, `num_iterations <- 46`). Iteration 1 assimilates weeks 1–5; each later iteration assimilates one more week (`obs_index = iteration + 4`), so by iteration 46 weeks 1–50 have been assimilated. For each assimilated week:

1. *Propagate.* Draw 7 daily OU steps for `Vm_t` and `r_t` per member (saved in `save_vm_daily_global`, `save_rt_daily_global`), write the last value back into rows 9–10, and push every member through `WNV_model()` for the week. Any `Im` value below 1 and any `Ih` value below 1 is then set to 0 (only those two entries, not the whole member).
2. *Observe.* The observation operator maps the state to `(Sm/10, Im/10, Ih)`. The data are converted to the same scale: `Sm_obs = (1 − X_obs2/1000)·X_obs1/10`, `Im_obs = (X_obs2/1000)·X_obs1/10`, `Ih_obs = X0_obs` (cumulative cases).
3. *Observation error.* `R <- diag(c(5.0, 0.001, 0.05))` is scaled by the observed value: `R_temp[1,1] = 5·Sm_obs`, `R_temp[2,2] = max(0.001·Im_obs, 0.001)`, `R_temp[3,3] = max(0.05·Ih_obs, 0.05)`.
4. *Update.* Stochastic EnKF with perturbed observations: `K = Pxy · ginv(Py + R_temp)`; `x_i ← x_i + K (y − h(x_i) + ε_i)`, `ε_i ~ N(0, R_temp)`. States and static parameters are floored at `1e-6`; no covariance inflation or localisation.
5. *Store.* The full analysed ensemble goes into `save_ensemble_full_global[ , , obs_index]` (dimension `22 × 8000 × 50`), plus per-target summaries (`predicted_global` = `Sm + Im`, `predicted2_global` = `Im/(Sm+Im)·1000`, `predicted3_global` = `Ih`, and the individual compartments).

After the analysis step of each iteration the script issues the **forecast**: 1,000 randomly chosen members are propagated for 14 further days (OU draws for `Vm_t`, `r_t`; observed daily weather for the two target weeks), giving an `8 × 2 × 1000` array. Members are kept only if their mosquito total is positive and `Im/(Sm+Im) < 1` (`valid_idx`); this filter is applied to every target, not just IM1000. Quantiles at 23 levels (`0.01, 0.025, 0.05, 0.10, 0.15, …, 0.90, 0.95, 0.975, 0.99`) are then taken for total abundance (`Sm + Im`), infectious per 1,000 (`Im/(Sm+Im)·1000`), cumulative human cases (`Ih`), plus `Sm` alone and `Im` alone, and stored as `results[[iteration]]`.

**Forecast WIS.** After the loop, `calculate_wis_all_targets()` scores every forecast with `epipredict::weighted_interval_score()` (23 quantile levels, `na_handling = "impute"`) and saves `wis_all_FullModel_<Year>.rds`. Note this is *forecast* WIS; the *fit* WIS is computed separately in `Fit_WIS.R`.

**Saved objects per year:** `save_ensemble_full_global_FullModel_<Year>.rds`, `wis_all_FullModel_<Year>.rds`, `results_FullModel_<Year>.rds` (structures in §10).

**Figures** (written at iteration 46 for each year; see §11 for the full list): OU trajectory ribbons (`A1`, `A2`), compartment ribbons (`A3`, `A7`–`A11`), fit-vs-observation plots (`A4`–`A6`), the `r_t` regime plot (`A26`), parameter histograms (`Param_Hist_*`), daily OU plots, 1- and 2-week forecast interval plots `Q1`–`Q6`, and, after the year loop, fan charts, 15-year panel PDFs (`build_panel()`), a selected-years panel (2007, 2014, 2021), a 2016 "schematic" figure, a 2014 three-panel figure and the temperature/precipitation response-function figure (`PRCP_TEMP_RESPONSE_2016.pdf`).

### 6.2 What changes in the other three scripts

| | `FullModel.R` | `FullModel_NoW.R` | `Mosq_Human.R` | `Mosq_Human_NoW.R` |
|---|---|---|---|---|
| README model number | 1 | 2 | 3 | 4 |
| Compartments | Sm Im Sb Ib Rb Sh Eh Ih | Sm Im Sb Ib Rb Sh Eh Ih | Sm Im Sh Eh Ih | Sm Im Sh Eh Ih |
| Weather forcing of `dSm` | yes | **no**: `dSm = Vm_t − λ_mb − mu·Sm` | yes (same term as Model 1) | **no** |
| Bird → mosquito infection | `r_t·Tmb·Ib·Sm/Nb` | same | **`r_t·f_t·Sm`** | **`r_t·f_t·Sm`** |
| OU parameters | `Vm_t`, `r_t` | `Vm_t`, `r_t` | `Vm_t`, `r_t`, **`f_t`** | `Vm_t`, `r_t`, **`f_t`** |
| Static parameters | 12 (rows 11–22) | 8: Tmb d Tbm gamma tau Vb Tmh psi (rows 11–18) | 6: Tmi Tma alpha phi Tmh psi (rows 9–14) | 2: Tmh psi (rows 9–10) |
| Ensemble rows | 22 | 18 | 14 | 10 |
| Row holding `Ih` | 8 | 8 | 5 | 5 |
| `ou_sigma_vm` | per-year table | 3.10 fixed | per-year table (2020 = 3.10) | 3.10 fixed |
| Initial `Vm_t` prior | U(0.0001, 2) | U(0.0001, 7) | U(0.0001, 7) | U(0.0001, 7) |
| Output prefix `<model>` | `FullModel` | `FullModel_NoClimate` | `Mosq+Human+Climate` | `Mosq+Human+NoClimate` |
| Tag in per-iteration PNG names | `iteration0FULL_` | `iteration0FULL0_` | `iteration_` | `iteration0_` |
| Label used in OU PNG names | — | `FullModel_withoutweather` | `Mosq+Human+Weather` | `Mosq+Human+NoWeather` |

In the two `Mosq_Human` scripts there is no bird population, so the force of infection on mosquitoes is a latent daily quantity `f_t` (the "bird-to-mosquito force of infection estimated as a time-varying parameter" in the README). `f_t` is initialised `U(0.0001, 0.001)` and propagated with exactly the same OU settings and regime switch as `r_t`. In the two `_NoW` scripts, `Vm_t` becomes the absolute daily mosquito recruitment rate rather than a scaling of a weather term, so it is given a wider prior and a larger OU σ. Ensemble size, iteration count, observation noise, Kalman update and forecast procedure are identical in all four scripts. The `NoW` scripts still read the weather CSVs in the year block but never pass them to `WNV_model()`.

**Extra analysis sections that exist only in some scripts.** `Mosq_Human.R` ends with `extract_tmin_tmax()`, which reads the posterior `Tmi`/`Tma` from the ensemble files of both weather models to summarise the estimated temperature window for mosquito growth, and a Spearman correlation between monsoon precipitation and the IM1000 relative WIS. `Mosq_Human_NoW.R` ends with several console-only checks used for the manuscript: ranking years by peak abundance ("boom years"), by June–September precipitation, and by total human case burden; raw WIS by year across the four models; central tendency / variability of relative WIS; and the direction (under/over-prediction) of abundance forecast errors in 2019 and in the dry years 2007, 2009, 2011. Several of these start by reading `summary_stats_log_relWIS.rds`, which `Ensemble_RelWIS.R` writes, so run that script first.

## 7. `Baseline_model.R`

Purpose: produce the reference forecasts against which every model and ensemble is compared, in exactly the same file format as the mechanistic models.

**Model.** `epipredict::cdc_baseline_forecaster()` — the CDC FluSight-style baseline. The point forecast is the last observed value; the predictive distribution is obtained by adding to it the (symmetrised) empirical distribution of past week-to-week changes, propagated over the horizon by simulation (`nsims = 1e5`) and summarised at the 23 quantile levels. No covariates, trend or seasonality; weather data are loaded but unused.

**Configuration** (lines ~565–583): `quantile_levels` (the 23 levels), `min_train_weeks <- 5`, `forecast_horizons <- c(1, 2)`, `n_output_trajectories <- 100L` (sample paths kept per origin), `output_dirpath <- "wnv-forecasts/"` (created but unused).

**Rolling forecasts.** `prepare_vector_to_edf()` wraps a 52-value series into an `epi_df` (geo `"AZ"`, column `weekly_count`). `run_rolling_forecast()` then loops over origins `all_weeks[5:50]` (46 origins, matching the mechanistic scripts), trains on all data up to the origin (expanding window), fixes the RNG seed per origin, fits the baseline and records the quantile forecasts (`forecasts`) and 100 sample trajectories (`samples`, captured by `trace()`-ing `epipredict:::propagate_samples`). This is done for total abundance, IM1000 and **weekly** human cases. The weekly case forecasts are then converted to cumulative-case forecasts by summing the quantile vectors across successive origins (lines ~1310–1337), so that `human_cases_q_*` is on the same cumulative scale as the models.

Note that this is a quantile-by-quantile running sum of *forecast* increments starting at the first origin; it does not add the observed cases from weeks 1–5, and past weeks enter through their forecasts rather than their observations. The result is scored against the true cumulative series `X4_obs`, so the baseline's human-case WIS should be read with that approximation in mind.

**Custom format.** `convert_baseline_to_custom_format()` reshapes the three `epipredict` outputs into the list-of-iterations format used by all model scripts (one element per origin with `total_abundance_q_1/2`, `infectious_per_1000_q_1/2`, `human_cases_q_1/2`, plus `reference_date`), which is saved as `results_BaselineModel_<Year>.rds`. `evaluate_forecasts()`, `get_forecast_errors()`, `plot_forecast_example()` and `plot_all_forecasts()` provide MAE/RMSE/coverage summaries and diagnostic plots that are printed but not saved.

**WIS.** The same `calculate_wis_all_targets()` used in the model scripts scores the baseline forecasts against `X_obs1`, `X_obs2` and the cumulative cases `X4_obs`, and saves `wis_all_BaselineModel_<Year>.rds`.

**After the year loop** the script aggregates the 15 `wis_all_BaselineModel_*.rds` files into a month-by-year heatmap (`heatmap_baseline_WIS_by_month.pdf`), year-level and month-level boxplots (`baseline_median_WIS_boxplot.*`, `baseline_WIS_by_month.*`) and 5×3 multi-year forecast panels (`figures/panel_BaselineModel_<target>_<forecast_1wk|forecast_2wk|fansight>.pdf`). A final section joins the four models' `wis_all_*` files to the baseline WIS iteration by iteration to compute relative WIS by season and month; on the way it saves the baseline table `baseline_joinWIS.rds` (columns `year, horizon, target, iteration, WIS_baseline`), which `Ensemble_RelWIS.R` reads in its last block.

## 8. `Fit_WIS.R`

Purpose: score the **in-sample fits** (not the forecasts) of the four mechanistic models.

The file contains four near-identical blocks, one per model, each with its own year loop. For every year and model it:

1. reads `save_ensemble_full_global_<model>_<Year>.rds` (dimension `n_state × 8000 × 50`);
2. for each of the 50 assimilated weeks computes, across the 8000 members, `total_abundance = Sm + Im` (rows 1 + 2), `infectious_per_1000 = Im / (Sm + Im) · 1000`, and `human_cases = Ih` (row 8 for the two Full models, row 5 for the two Mosq+Human models — this row index is the only code difference between the blocks besides file names);
3. takes the 23 quantiles of each and saves them as `fit_results_<model>_<Year>.rds` (a list of 50 weeks, each `list(total_abundance_q, infectious_per_1000_q, human_cases_q)`);
4. scores each week's quantile set against the observation for that week with `calculate_wis_per_year()` (`epipredict::weighted_interval_score`, `na_handling = "impute"`) and saves `fit_wis_all_<model>_<Year>.rds`, a tibble with columns `week, target, actual, median_forecast, WIS` (≤ 150 rows: 3 targets × 50 weeks).

The remainder of the file (after line ~2545) loads all `fit_wis_all_*` files for `years <- c(2006:2019, 2021)` and produces:

* `median_h2_fit.png/.pdf` — boxplots of each year's median fit WIS by model and target (`plot_wis_boxplots()`; the "mean" version is drawn but not saved). The ggplot object is also saved as `median_plot_fit.rds` for `Ensemble_RelWIS.R`;
* `median_plot_final_fit_all_targets_2014.png/.pdf` — the above stacked (with `patchwork::wrap_elements`) on the 2014 three-panel fit figure read from `final_fit_2014_FullModel.rds`, which `FullModel.R` writes;
* `median_month_fit.png/.pdf` — month-of-year boxplots by model (`plot_wis_by_month()`);
* `heatmap_WIS_{total_abundance,infectious_per_1000,human_cases}.pdf` — 15-year heatmaps of median (absolute) fit WIS by month and model;
* console-only "claim" checks used for the manuscript (median WIS by model, spring vs rest-of-year abundance WIS, IM1000 WIS by year with and without zero weeks);
* `trap_counts_supplementary_figure.png/.pdf` and `trap_counts.csv` — the size of the county trap network 2006–2024 (hard-coded counts in the script).

Display names are recoded in this section: `FullModel_NoClimate → FullModel_NoWeather`, `Mosq+Human+Climate → Mosq+Human+Weather`, `Mosq+Human+NoClimate → Mosq+Human+NoWeather` (the heatmap section uses `…_WithoutWeather` instead).

## 9. `Ensemble_RelWIS.R`

Purpose: build five ensembles from the four model forecasts, score them, and express every model's forecast WIS relative to the baseline.

**Loading** (`load_all_model_quantiles(year, obs_list)`). Reads `results_<model>_<Year>.rds` for the four models and, for each of the six target × horizon combinations defined in `TARGET_CONFIG`, stacks the quantile vectors into `q_array[46 iterations, 4 models, 23 quantiles]` together with the 46 matching observations (`actuals`).

**Ensemble builders** (each produces a `46 × 23` quantile matrix; `build_ens_wis()` returns it as `list(q_matrix, weights)` so the fitted weights can be kept):

| Function | Ensemble name in outputs | Method |
|---|---|---|
| `build_ens_avg()` | `Ensemble_model_1` | quantile-wise mean of the four models (Vincentisation) |
| `build_ens_median()` | `Ensemble_model_2` | quantile-wise median |
| `build_ens_reg()` | `Ensemble_model_3` | unconstrained OLS of the observations on the four models' medians (`lm(actual ~ .)`); the fitted intercept and weights are then applied to every quantile level and negative values clipped at 0 |
| `build_ens_wis()` | `Ensemble_model_4` | weights on the simplex (≥ 0, sum = 1) chosen with `nloptr` (COBYLA) to minimise the total WIS of the weighted quantile average over the year |
| `build_linear_pool()` | `Ensemble_model_5` | equal-weight linear pool: each model's quantiles are turned into an approximate CDF on a 500-point grid, the CDFs are averaged, and the mixture CDF is inverted at the 23 levels |

`run_ensembles_all_data()` applies the first four to every target × horizon and saves `ensemble_results_alldata_<Year>.rds`; `run_linear_pool()` saves `linear_pool_results_<Year>.rds`. "alldata" means the regression and WIS-optimal weights are fitted on all 46 forecasts of the same year (in-sample weighting, no hold-out), separately for each target and horizon; the fitted weights are stored in `weights_by_target`.

**Relative WIS.** Still inside the year loop, the script reads the four models' `wis_all_*`, the baseline's `wis_all_BaselineModel_*`, and the two ensemble result files, and stacks them into one long tibble (`extract_wis()`, `extract_wis_ensemble()`) with columns `time, model, target, horizon, WIS`, where `model` ∈ {`Full Model`, `Mosq+Human+Climate`, `Mosq+Human+NoClimate`, `FullModel_NoClimate`, `Baseline`, `Ensemble_model_1…5`} and `target` ∈ {`Total abundance`, `Infectious mosq per 1000`, `Human cases`}. This is saved as `all_wis_longE_<Year>.rds`. Joining each non-baseline row to the baseline WIS for the same week/target/horizon gives `rel_wis = WIS / wis_baseline`; non-finite ratios (e.g. baseline WIS = 0) are dropped and the result is saved as `all_rel_longE_<Year>.rds`. The ratio is stored raw; the **log** is taken in the summary and plotting code that follows, so a value below 0 in the figures means "better than baseline".

**Multi-year figures** (after the loop, `years <- c(2006:2019, 2021)`), using an Okabe–Ito colour map `model_colors`:

* `mean_h1.png`, `mean_h2.png`, `median_h1.pdf`, `median_h2.pdf` — boxplots of yearly mean/median log relative WIS by model and target for 1- and 2-week horizons (`plot_boxplots()`), drawn from `summary_stats_log_5`, a five-model subset (`Ensemble_model_4` plus the four individual models). `median_h1_full.pdf`, `median_h2_full.pdf` are the same plots drawn from the unfiltered `summary_stats_log` (all nine non-baseline models). The unfiltered table is also saved as `summary_stats_log_relWIS.rds` for the `Mosq_Human*` extras.
* `median_plot_final_forecast_all_targets_2014.png/.pdf` — the 2-week boxplot stacked (with `wrap_elements`) on the 2014 fan/forecast figure read from `final_combined_2014_FullModel.rds` (written by `FullModel.R`); the script also reads `median_plot_fit.rds` from `Fit_WIS.R` to display the fit and forecast boxplots together.
* `log_median_rel_wis_by_month_horizon{1,2}.pdf`, `log_mean_rel_wis_by_month_horizon{1,2}.pdf` — month-of-year boxplots (`plot_boxplots_by_month()`) from the five-model subset `individual_points_log_5`; `log_median_rel_wis_by_month_horizon{1,2}_full.pdf` are the unfiltered versions.
* `heatmap_relWIS_{total_abundance,infectious_mosq,human_cases}_h{1,2}.pdf` — 15-year heatmaps of median log relative WIS by month for all nine non-baseline models (blue = better than baseline, red = worse; capped at ±7).
* `heatmap_2014_relWIS_horizon{1,2}.pdf` — the same for 2014 only, five models, capped at ±4.5.

Console-only summaries include the fraction of year × target × horizon combinations in which each individual model ranks in the top half, two ways of counting how often models beat the baseline on human cases, and a final "ensemble 4 exploration" that rebuilds `Ensemble_model_4`'s weekly WIS from the `all_wis_longE_*` files, joins it to the baseline table `baseline_joinWIS.rds` (written by `Baseline_model.R`) and prints its monthly median log relative WIS.

## 10. The `outputs/` folder

All files are one-per-year, and every family covers the same 15 study years (2006–2019 and 2021), giving 330 files in total.

| File pattern | Written by | Read by | Structure | Years present |
|---|---|---|---|---|
| `results_<model>_<Year>.rds` | the four model scripts | `Ensemble_RelWIS.R`; panel sections of the model scripts | unnamed list of 46 iterations; each a named list of 12: `vm_forecast_q`, `rt_forecast_q` (23 × 2 matrices, columns = horizon 1, 2) and the 23-quantile vectors `total_abundance_q_1/2`, `infectious_per_1000_q_1/2`, `human_cases_q_1/2`, `infectious_mosq_q_1/2` (`Im`), `abundance_q_1/2` (`Sm`); quantile names `"1%" … "99%"` | all four models: 2006–2019, 2021 |
| `results_BaselineModel_<Year>.rds` | `Baseline_model.R` | `Ensemble_RelWIS.R` (indirectly, via its WIS file); baseline panels | list of 46; each `list(reference_date, total_abundance_q_1/2, infectious_per_1000_q_1/2, human_cases_q_1/2)` | 2006–2019, 2021 |
| `wis_all_BaselineModel_<Year>.rds` | `Baseline_model.R` | `Ensemble_RelWIS.R`, baseline summary sections | named list of 6 tibbles `abundance_1wk, abundance_2wk, infected_1wk, infected_2wk, cases_1wk, cases_2wk`, each 46 rows × `iteration, horizon, target, actual, median_forecast, WIS` | 2006–2019, 2021 |
| `wis_all_<model>_<Year>.rds` | the four model scripts | `Ensemble_RelWIS.R`, `Baseline_model.R` (last section), `Mosq_Human_NoW.R` extras | same structure as the baseline WIS file | 2006–2019, 2021 |
| `save_ensemble_full_global_<model>_<Year>.rds` | the four model scripts | `Fit_WIS.R`; fit panels in the model scripts; `extract_tmin_tmax()` in `Mosq_Human.R` | numeric array `n_state × 8000 × 50` (`n_state` = 22 / 18 / 14 / 10, see §6.2) | **not in the repository** — too large (≈ 30–70 MB each); regenerate by running the model scripts |
| `fit_results_<model>_<Year>.rds` | `Fit_WIS.R` | `Fit_WIS.R` | list of 50 weeks; each `list(total_abundance_q, infectious_per_1000_q, human_cases_q)` of 23 quantiles | all four models: 2006–2019, 2021 |
| `fit_wis_all_<model>_<Year>.rds` | `Fit_WIS.R` | `Fit_WIS.R` aggregation | tibble `week, target, actual, median_forecast, WIS` (150 rows) | same as `fit_results_` |
| `ensemble_results_alldata_<Year>.rds` | `Ensemble_RelWIS.R` | `Ensemble_RelWIS.R` | `list(year, wis_EnsAvg, wis_EnsMedian, wis_EnsReg, wis_EnsWIS, weights_by_target)`; each `wis_*` is a list of 6 tibbles (`abundance_1wk … cases_2wk`) with columns `iteration, forecast_date, horizon, target, actual, median_forecast, WIS`; `weights_by_target` holds the six length-4 weight vectors of the WIS-optimised ensemble | 2006–2019, 2021 |
| `linear_pool_results_<Year>.rds` | `Ensemble_RelWIS.R` | `Ensemble_RelWIS.R` | `list(year, wis_LinearPool)` with the same 6-tibble layout | 2006–2019, 2021 |
| `all_wis_longE_<Year>.rds` | `Ensemble_RelWIS.R` | `Ensemble_RelWIS.R` (final section) | tibble `time, model, target, horizon, WIS`; 10 models × 3 targets × 2 horizons × 46 weeks = 2760 rows | 2006–2019, 2021 |
| `all_rel_longE_<Year>.rds` | `Ensemble_RelWIS.R` | all multi-year summaries in `Ensemble_RelWIS.R` | tibble `time, model, target, horizon, WIS, wis_baseline, rel_wis` (9 models, non-finite ratios removed) | 2006–2019, 2021 |

Note the naming convention: files with the `results_`/`wis_all_` prefix are **forecast** objects; files with the `fit_` prefix are **in-sample fit** objects; `longE` files are the long-format tables used for the relative-WIS figures.

### 10.1 Hand-off files between scripts

Besides the per-year results above, a few small `.rds` files carry objects from one script to another so that no script relies on another script's R session. They are written to the working directory and are not checked in.

| File | Written by | Read by | Content |
|---|---|---|---|
| `final_fit_2014_FullModel.rds` | `FullModel.R` (2014 three-panel section) | `Fit_WIS.R` | `arrangeGrob` of the 2014 fit panels + legend |
| `final_combined_2014_FullModel.rds` | `FullModel.R` (same section) | `Ensemble_RelWIS.R` | `arrangeGrob` of the 2014 fan / 2-week forecast panels + legends |
| `median_plot_fit.rds` | `Fit_WIS.R` (boxplot section) | `Ensemble_RelWIS.R` | ggplot of median fit WIS by model and target |
| `summary_stats_log_relWIS.rds` | `Ensemble_RelWIS.R` (multi-year summary) | `Mosq_Human.R`, `Mosq_Human_NoW.R` | tibble `year, model, target, horizon, mean_rel_wis, median_rel_wis` (log scale, all nine models, display names recoded) |
| `baseline_joinWIS.rds` | `Baseline_model.R` (last section) | `Ensemble_RelWIS.R` (last block) | tibble `year, horizon, target, iteration, WIS_baseline` |

### 10.2 What can be reproduced from the checked-in files

With the `results_*` and `wis_all_*` files for all five models present, `Ensemble_RelWIS.R` and the multi-year sections of `Baseline_model.R` can be re-run end to end from `outputs/` alone, and `Fit_WIS.R`'s aggregation section can be re-run from the `fit_wis_all_*` files. The only outputs that require re-running the four model scripts are those that need the full posterior ensemble (`save_ensemble_full_global_*`, omitted because of size): the per-year loops in `Fit_WIS.R` that build `fit_results_*` / `fit_wis_all_*`, the `fit` panels and the 2016/2014 figures in the model scripts, and `extract_tmin_tmax()` in `Mosq_Human.R`.

## 11. Figures produced by the scripts

The scripts write figures to the working directory (and `./figures/`); none are checked in. §11.1 lists the figure families by file-name pattern; §11.2–11.4 map the figures and tables of the manuscript to the code that generates them.

### 11.1 Figure families

| Pattern | Script(s) | Content |
|---|---|---|
| `A1…A11_<tag>46_<Year>.png` (Full models) or `A1…A9_<tag>46_<Year>.png` (Mosq+Human models, no bird plots), plus `A26_…` (`FullModel.R`) / `A36_…` (other three) | model scripts (iteration 46) | posterior ribbons of `Vm_t`, `r_t`, each compartment, fitted vs observed targets, and the `r_t` OU regime over time |
| `Param_Hist_<tag>46_<Year>.png` | model scripts (iteration 46) | histograms of the posterior static parameters (12 / 8 / 6 / 2 panels) |
| `vm_daily_OU_*`, `rt_daily_OU_*`, `ft_daily_OU_*`, `vm_rt(_ft)_daily_OU_*_<Year>.png` | model scripts | daily OU trajectories |
| `Q1…Q6_<tag>46_<Year>.png`, `Q1…Q6_BASELINE_*` | model scripts, baseline | 1-week (odd) and 2-week (even) forecast interval plots for cases, IM1000, abundance |
| `f_abundance_<model>.png`, `f_infectious_per_1000_<model>.png`, `f_Humancases_<model>.png` | model scripts | FluSight-style fan charts for the last year in the loop |
| `figures/NEWpanel_FullModel_<target>_<type>.pdf`, `figures/panel_<model>_<target>_<type>.pdf` | model scripts, baseline | 5 × 3 year panels; `<type>` ∈ `fit`, `forecast_1wk`, `forecast_2wk`, `fansight` |
| `figures/panel_FullModel_selected_years_fit_and_forecast.pdf`, `schematic_2016_fansight.png`, `plot_2014_fit_3panel_FullModel.png`, `plot_2014_fansight_fc2wk_2x3_FullModel.png`, `PRCP_TEMP_RESPONSE_2016.pdf` | `FullModel.R` | manuscript figures (2007/2014/2021 panel, 2016 schematic, 2014 three-panel, temperature/precipitation response functions) |
| `heatmap_baseline_WIS_by_month.pdf`, `baseline_*_WIS_boxplot.*`, `baseline_WIS_by_month.*`, `figures/panel_BaselineModel_*.pdf` | `Baseline_model.R` | baseline WIS summaries and multi-year forecast panels |
| `median_h2_fit.*`, `median_month_fit.*`, `heatmap_WIS_<target>.pdf`, `trap_counts_supplementary_figure.*` | `Fit_WIS.R` | fit-quality summaries |
| `mean_h*.png`, `median_h*.pdf`, `log_*_rel_wis_by_month_horizon*.pdf`, `heatmap_relWIS_*_h*.pdf`, `heatmap_2014_relWIS_horizon*.pdf`, `median_plot_final_forecast_all_targets_2014.*` | `Ensemble_RelWIS.R` | relative-WIS summaries |

### 11.2 Main-text figures → code

Figure numbers follow the manuscript PDF (`WNV_Forecast.pdf`). Line numbers refer to the current scripts and are approximate. Every figure below needs the year loops of the relevant scripts to have been run first (§3), because they read the per-year `.rds` files.

| Figure | File in the manuscript | Generated by | How |
|---|---|---|---|
| **Figure 1** — model schematic and pipeline | `figs/model_scheme_FINAL.pdf` | not code-generated | Drawn by hand (vector graphics). The schematic-style fan chart in `FullModel.R` (`schematic_2016_fansight.png`, lines ~3785–4130) is a related illustration but is not used in the final figure. |
| **Figure 2** — (top) median in-sample fit WIS by model and target across years; (bottom) 2014 fit for the Full Model, three targets | `figs/median_plot_final_fit_all_targets_2014.pdf` | `Fit_WIS.R` + `FullModel.R` | Bottom row: `FullModel.R` "2014 Three-panel layout" section (lines ~4136–4665) → `plot_2014_fit_3panel_FullModel.png` and the hand-off file `final_fit_2014_FullModel.rds`. Top row: `Fit_WIS.R` boxplot section (`plot_wis_boxplots()`, lines ~2547–2684) → `median_h2_fit.pdf`. The two are stacked with `patchwork` at `Fit_WIS.R` lines ~2688–2705 → `median_plot_final_fit_all_targets_2014.pdf`. Inputs: `fit_wis_all_<model>_<Year>.rds` (all four models, 15 years), `save_ensemble_full_global_FullModel_2014.rds`. |
| **Figure 3** — (top) median log relative WIS of 2-week-ahead forecasts by model and target; (bottom) 2014 Full Model fan charts and 2-week-ahead forecasts | `figs/median_plot_final_forecast_all_targets_2014.pdf` | `Ensemble_RelWIS.R` + `FullModel.R` | Bottom rows: `FullModel.R` 2014 section (lines ~4666–4736) → `plot_2014_fansight_fc2wk_2x3_FullModel.png` and `final_combined_2014_FullModel.rds`. Top row: `Ensemble_RelWIS.R` `plot_boxplots(summary_stats_log_5, "median", horizon_num = 2)` (lines ~1430–1520) → `median_h2.pdf`; five-model subset (Ensemble 4 + four individual models). Stacked at `Ensemble_RelWIS.R` lines ~1530–1548 → `median_plot_final_forecast_all_targets_2014.pdf`. Inputs: `all_rel_longE_<Year>.rds`, `results_FullModel_2014.rds`, `save_ensemble_full_global_FullModel_2014.rds`. |
| **Figure 4** — seasonal (by calendar month) log relative WIS of 2-week-ahead forecasts by model and target | `figs/log_median_rel_wis_by_month_horizon2.pdf` | `Ensemble_RelWIS.R` | "Box plots by months" section: `plot_boxplots_by_month(individual_points_log_5, "median", horizon_num = 2)` (lines ~1550–1675) → `log_median_rel_wis_by_month_horizon2.pdf`. Five-model subset. Input: `all_rel_longE_<Year>.rds`. |
| **Figure 5** — 2014 heatmap of median log relative WIS by model, month and target, 2-week horizon | `figs/heatmap_2014_relWIS_horizon2.pdf` | `Ensemble_RelWIS.R` | "Heatmap, 2014 only" section (lines ~1840–1990): `heatmap_2014`, loop over `h in c(1, 2)` → `heatmap_2014_relWIS_horizon2.pdf` (the `horizon1` file is produced at the same time but not used). Five-model subset, colour scale capped at ±4.5. Input: `all_rel_longE_2014.rds`. |

### 11.3 Supplementary figures → code

Numbers are the order of the `figure` environments in `supplement.tex` (S1–S80). The supplement's own file names are given so a figure can be matched regardless of numbering. All 15-year panels are 5 × 3 grids built by `build_panel(target, plot_type)` in the script named; they need that script's `results_<model>_<Year>.rds` (forecast panels) or `save_ensemble_full_global_<model>_<Year>.rds` (fit panels) for all 15 years, plus the observation CSVs.

**Fit panels (S1–S12).** `plot_type = "fit"`: ribbons of the EnKF posterior at every assimilated week versus observations.

| Figures | Files | Script → output |
|---|---|---|
| S1, S5, S9 | `NEWpanel_FullModel_{total_abundance, infectious_per_1000, human_cases}_fit.pdf` | `FullModel.R` `build_panel()` (lines ~3485–3560; driver loop `for (tgt in targets) for (pt in plot_types)`) → `figures/NEWpanel_FullModel_<target>_fit.pdf` |
| S2, S6, S10 | `panel_FullModel_NoClimate_<target>_fit.pdf` | `FullModel_NoW.R` `build_panel()` (lines ~3319–3400) → `figures/panel_FullModel_NoClimate_<target>_fit.pdf` |
| S3, S7, S12 | `panel_Mosq+Human+Climate_<target>_fit.pdf` | `Mosq_Human.R` `build_panel()` (lines ~3318–3395) → `figures/panel_Mosq+Human+Climate_<target>_fit.pdf` |
| S4, S8, S11 | `panel_Mosq+Human+NoClimate_<target>_fit.pdf` | `Mosq_Human_NoW.R` `build_panel()` (lines ~3284–3370) → `figures/panel_Mosq+Human+NoClimate_<target>_fit.pdf` |

**Forecast panels (S13–S48).** Same `build_panel()` calls with `plot_type = "forecast_1wk"` (interval bars for every 1-week-ahead forecast), `"forecast_2wk"` (2-week-ahead) and `"fansight"` (overlapping 2-week fans issued every fourth origin, `plot_every_n = 4`).

| Figures | Model | Script → output pattern |
|---|---|---|
| S13–S21 | Full Model | `FullModel.R` → `figures/NEWpanel_FullModel_<target>_{forecast_1wk, forecast_2wk, fansight}.pdf` |
| S22–S30 | Full Model without weather | `FullModel_NoW.R` → `figures/panel_FullModel_NoClimate_<target>_{…}.pdf` |
| S31–S39 | Mosquito + Human with weather | `Mosq_Human.R` → `figures/panel_Mosq+Human+Climate_<target>_{…}.pdf` |
| S40–S48 | Mosquito + Human without weather | `Mosq_Human_NoW.R` → `figures/panel_Mosq+Human+NoClimate_<target>_{…}.pdf` (note: the S40 `\includegraphics` in `supplement.tex` currently points at the `FullModel_NoClimate` 1-week abundance file; the intended file is `panel_Mosq+Human+NoClimate_total_abundance_forecast_1wk.pdf`) |

Within each block of nine the order is: total abundance 1-wk, 2-wk, fan; IM1000 1-wk, 2-wk, fan; human cases fan, 1-wk, 2-wk.

**Model diagnostics, response functions and baseline (S49–S61).**

| Figure | File | Script → how |
|---|---|---|
| S49 — temperature and precipitation response functions (2016 weather) | `PRCP_TEMP_RESPONSE_2016.pdf` | `FullModel.R`, final section "Supplementary Figure S49" (appended after the 2014 three-panel section). Reads the 2016 daily series (`county_temp.csv` / `county_prcp.csv`, rows 731:1096), applies the clipping rule of `inputTem()` with `Tmi = 21.1`, `Tma = 46`, and plots `−(T − Tmi)(T − Tma)` and `1/(1 + exp(alpha − phi·P))` (`alpha = 1.48`, `phi = 1.37`) against temperature / precipitation (top row) and against day of year (bottom row); four panels combined with `patchwork` → `PRCP_TEMP_RESPONSE_2016.pdf`. Runs standalone; needs only the two CSVs. |
| S50 — WIS / relative-WIS explainer | `WIS_explainer_diagram.pdf` | not code-generated (prepared in PowerPoint) |
| S51 — simulated susceptible birds, Full Model, 2014 | `A3_iteration0FULL_46_2014.png` | `FullModel.R`, in-loop diagnostic plots saved at `iteration == 46` (`ggsave(paste0("A3_iteration0FULL_", …))`, line ~1466; plot object `S10`, row 3 `Sb` of `save_ensemble_full_global`). Run with `years_to_run <- 2014`. |
| S52 — OU diffusion regime σ over time, 2014 | `A26_iteration0FULL_46_2014.png` | `FullModel.R`, same block, line ~1476 (plot `p2` of `sigma_t_history`) |
| S53 — histograms of the 12 static parameters, Full Model, 2014 | `Param_Hist_iteration0FULL_46.png` | `FullModel.R`, line ~1478 (`png(…); par(mfrow = c(3, 4)); hist(static_global[i, ])`). The current code adds the year to the name: `Param_Hist_iteration0FULL_46_2014.png`. |
| S54 — daily OU trajectories of `Vm_t` and `r_t`, 2014 | `vm_rt_daily_OU_2014.png` | `FullModel.R`, daily-OU plots saved at `iteration == 46` (lines ~1198–1217) from `save_vm_daily_global` / `save_rt_daily_global` |
| S55 — baseline median WIS by year, both horizons | `baseline_median_WIS_boxplot.pdf` | `Baseline_model.R` "Boxplot for year" section (lines ~2495–2715): `plot_baseline_boxplots_free(…, "median")` → `baseline_median_WIS_boxplot.pdf`. Input: `wis_all_BaselineModel_<Year>.rds` |
| S56 — baseline WIS by calendar month | `baseline_WIS_by_month.pdf` | `Baseline_model.R` monthly section (lines ~2720–2935): `plot_baseline_by_month()` → `baseline_WIS_by_month.pdf` |
| S57, S59 — median log relative WIS by model and target, all nine models, 1-wk / 2-wk | `median_h1_full.pdf`, `median_h2_full.pdf` | `Ensemble_RelWIS.R` `plot_boxplots(summary_stats_log, "median", horizon_num = 1 / 2)` on the **unfiltered** table (lines ~1524–1527). Input: `all_rel_longE_<Year>.rds` |
| S58, S60 — the same by calendar month | `log_median_rel_wis_by_month_horizon1_full.pdf`, `…horizon2_full.pdf` | `Ensemble_RelWIS.R` `plot_boxplots_by_month(individual_points_log, "median", 1 / 2)` on the unfiltered table (lines ~1678–1681) |
| S61 — baseline median WIS heatmap by month and year | `heatmap_baseline_WIS_by_month.pdf` | `Baseline_model.R` "Baseline WIS heatmap" section (lines ~2315–2493) → `heatmap_baseline_WIS_by_month.pdf` |

**Fit-WIS and relative-WIS heatmaps, trap counts (S62–S71).**

| Figure | File | Script → how |
|---|---|---|
| S62, S63, S64 — median in-sample fit WIS by model, month and year (human cases, abundance, IM1000) | `heatmap_WIS_human_cases.pdf`, `heatmap_WIS_total_abundance.pdf`, `heatmap_WIS_infectious_per_1000.pdf` | `Fit_WIS.R` heatmap section (`plot_heatmap_by_year()`, lines ~2845–3001; loop over `targets_list`). Input: `fit_wis_all_<model>_<Year>.rds` |
| S65–S70 — median log relative WIS heatmaps by model, month and year; human cases / IM1000 / abundance × 1-wk / 2-wk | `heatmap_relWIS_{human_cases, infectious_mosq, total_abundance}_{h1, h2}.pdf` | `Ensemble_RelWIS.R` "Heatmaps, all years" section (`plot_heatmap()`, lines ~1690–1835; nested loop over targets and horizons). All nine non-baseline models; colour scale capped at ±7. Input: `all_rel_longE_<Year>.rds` |
| S71 — number of operational traps 2006–2024 | `trap_counts_supplementary_figure.pdf` | `Fit_WIS.R` last section (lines ~3126–3234): hard-coded `trap_data` tibble → `trap_counts_supplementary_figure.pdf` and `trap_counts.csv` |

**Baseline forecast panels (S72–S80).** `Baseline_model.R` `build_panel()` (lines ~3433–3520; `plot_type` ∈ `forecast_1wk`, `forecast_2wk`, `fansight`) → `figures/panel_BaselineModel_<target>_<type>.pdf`, in the order abundance 1-wk, 2-wk, fan (S72–S74); IM1000 1-wk, 2-wk, fan (S75–S77); human cases fan, 1-wk, 2-wk (S78–S80). Input: `results_BaselineModel_<Year>.rds`.

### 11.4 Supplementary tables and numbers quoted in the text

The supplement's tables are typeset by hand, but their contents come from the code:

| Table / quantity | Where it comes from |
|---|---|
| Table S1 (`tab:models`) — model configurations, numbers of state variables and parameters | the state vectors in §6.2 (rows 1–8 / 1–5, static rows, OU rows) |
| Table S2 (`tab:statevars`) — state variables of the Full Model | `WNV_model()` in `FullModel.R` (§6.1); `X_M, Y_M, X_B, Y_B, Z_B, X_H, W_H, Y_H` are `Sm, Im, Sb, Ib, Rb, Sh, Eh, Ih` in the code |
| Table S3 (`tab:params`) — priors for ensemble initialisation | the "State variables" / "Static parameters" blocks of each model script (`runif()` calls; `FullModel.R` lines ~860–915 and the equivalents listed in §6.2) |
| Table S4 (`tab:sigma_vm`) — year-specific σ for `Vm_t` | `sigma_vm_year` (`FullModel.R` lines ~615–635 and `Mosq_Human.R`); the fixed value 3.10 is `ou_sigma_vm` in `FullModel_NoW.R` and `Mosq_Human_NoW.R` |
| Estimated `Tmin` / `Tmax` (posterior medians quoted in Results) | `Mosq_Human.R` `extract_tmin_tmax()` block (lines ~3400–3490), reads `save_ensemble_full_global_FullModel_<Year>.rds` and `…Mosq+Human+Climate_<Year>.rds` |
| Spearman correlation between monsoon precipitation and IM1000 relative WIS (Supplementary Results, `supp:precip_correlation`) | monsoon totals: `Mosq_Human_NoW.R` June–September precipitation block (lines ~3532–3670, `precip_by_year`); correlation: `Mosq_Human.R` `cor_check` block (lines ~3490–3555), which reads `summary_stats_log_relWIS.rds` |
| Ensemble performance by target — median relative WIS, ranks and year-to-year SD of the nine configurations (`supp:ensemble_bytarget`) | `Mosq_Human_NoW.R` "central tendency and variability" blocks (`central_tendency`, `variability`, `central_tendency_full`, `ranked_full`, `variability_full`, lines ~3845–3990), reading `summary_stats_log_relWIS.rds` |
| Share of year × target × horizon combinations in which each model ranks in the top half; share of years each model beats the baseline for human cases | `Ensemble_RelWIS.R` `rank_among_4` / `top_half_4`, `rank_among_9` / `top_half_9`, `option_A` / `option_B` (lines ~1330–1410), console output only |
| Fit-quality statements (median fit WIS by model, spring vs rest-of-year abundance, IM1000 fit by year) | `Fit_WIS.R` `claim1`–`claim3_median_active` (lines ~3002–3125), console output only |
| Boom years, precipitation ranking, case-burden ranking, direction of abundance forecast errors (2019 and dry years) | `Mosq_Human_NoW.R` blocks at lines ~3376–3530, ~3673–3780 and ~3990–4090, console output only |

## 12. Known quirks and things to watch for

None of these affect the published results, but they matter if you re-run or extend the scripts.

* **2020 is not a study year.** Every year block has a 2020 branch, but `years_to_run` excludes it and the branch is incomplete (`inputTem_i` is overwritten and `inputP_i` never set). The 2020 input CSVs are kept in `datasets/` only so that branch still parses; no 2020 results are included in `outputs/`.
* **Scripts talk to each other only through files.** `Fit_WIS.R`, `Ensemble_RelWIS.R` and the analysis blocks at the end of the two `Mosq_Human` scripts read hand-off `.rds` files written by other scripts (§10.1). If one of them stops with "cannot open file", run the producing script (or section) first — see the run order in §3.
* **Inside a script, run the year loop before anything after it.** `Baseline_model.R`'s `Q1–Q6` section, for example, defines `num_iterations` and `iteration` from the freshly written `results_BaselineModel_<Year>.rds` immediately after the year loop, and all the multi-year sections read the per-year `.rds` files the loop produces.
* **Package installs inside scripts** (`pak::pkg_install` at the top of `Baseline_model.R`, `install.packages("patchwork")` at the top of `Ensemble_RelWIS.R`) hit the network every time they are run.
* **Model display names differ by section.** The same model appears as `FullModel_NoClimate` (file names), `FullModel_NoWeather` (boxplots, `summary_stats_log_relWIS.rds`), `FullModel_WithoutWeather` (heatmaps) and `FullModel_withoutweather` (OU plots); `Full Model` (with a space) is the name used inside the `longE` files. Likewise `Mosq+Human+Climate` ↔ `Mosq+Human+Weather`. Check the `recode()` step of whichever section you are in before filtering on a model name.
* **Five-model subsets.** The main relative-WIS boxplots in `Ensemble_RelWIS.R` are drawn from `summary_stats_log_5` / `individual_points_log_5` (Ensemble 4 plus the four individual models); the `_full` files and the all-year heatmaps use all nine non-baseline models. The 2014 heatmap uses the five-model subset only.
* **Forecast fallback.** In the model scripts, if no forecast member has a positive mosquito total, a warning is printed and the previous iteration's quantiles are reused silently.
* **Baseline human-case forecasts** are cumulative sums of weekly-incidence forecast quantiles across origins (§7), not forecasts of the cumulative series itself.


## 13. Glossary of recurring variable names

| Name | Meaning |
|---|---|
| `Year`, `years_to_run` | current study year and the vector looped over |
| `inputTem_i`, `inputP_i` | daily temperature (°C) and precipitation for `Year` |
| `X_obs1`, `X_obs2`, `X0_obs` | weekly observations: total abundance, IM1000, cumulative human cases (`X0_obs` is weekly incidence inside `Baseline_model.R`; cumulative there is `X4_obs`) |
| `all_weeks`, `observed_dates` | the 52 weekly dates starting 1 January |
| `forecast_1week_dates`, `forecast_2week_dates` | target dates of the 46 one- and two-week-ahead forecasts (weeks 6–51 and 7–52) |
| `N` | ensemble size (8000) |
| `num_iterations` | number of forecast origins (46) |
| `total_time_points` | number of assimilated weeks (50) |
| `ensemble` | current `n_state × N` EnKF state matrix |
| `save_ensemble_full_global` | archive of the analysed ensemble at every week, `n_state × N × 50` |
| `Vm_t`, `r_t`, `f_t` | OU-driven daily mosquito recruitment, transmission scaling, and (Mosq+Human models) latent force of infection on mosquitoes |
| `OUproc_func`, `ou_mu_*`, `ou_lambda_*`, `ou_sigma_*` | the OU propagator and its mean, reversion rate and volatility |
| `obs_year[Year]`, `sigma_t_history` | week at which `r_t` switches from the "off" to the "on" OU regime, and the σ in force each week |
| `i_year[Year]`, `brim` | start day and daily rate of infectious-mosquito importation |
| `R`, `R_temp` | base and observation-scaled observation-error covariance |
| `results` | list of 46 forecast-quantile sets (the "custom format") |
| `wis_all` | list of six WIS tibbles (targets × horizons) |
| `quantile_levels` / `QUANTILE_LEVELS`, `probs` | the 23 quantile levels |
| `q_array` | `46 × 4 × 23` stack of model forecast quantiles used to build ensembles |
| `rel_wis` | `WIS / wis_baseline` for the same week, target and horizon (logged in figures) |
| `model_colors` | Okabe–Ito palette keyed by display model name |
