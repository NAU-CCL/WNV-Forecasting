# WNV-Forecasting

We developed and compared four mechanistic Ordinary Differential Equation (ODE) model configurations of WNV transmission that differ systematically in whether they incorporate weather forcing and whether they explicitly represent an avian reservoir. We fit our models using Bayesian data assimilation, specifically the Ensemble Kalman Filter (EnKF).

## Repository structure

### Model fitting/forecasting scripts

Each script fits and forecasts one of the four model configurations using the EnKF and Ornstein-Uhlenbeck (OU) parameter propagation, generating both in-sample fits and rolling 1- and 2-week-ahead probabilistic forecasts for all three surveillance targets (total mosquito abundance, mosquito infection prevalence, and cumulative human WNV cases) across 2006–2019 and 2021.

| File | Description |
|---|---|
| `FullModel.R` | Model 1: mosquito + bird + human, **with** weather forcing. |
| `FullModel_NoW.R` | Model 2: mosquito + bird + human, **without** weather forcing. Mosquito growth follows the time-varying rate $\nu_M(t)$ alone. |
| `Mosq_Human.R` | Model 3: mosquito + human only, **with** weather forcing. Omits the avian compartment; bird-to-mosquito force of infection is estimated as a time-varying parameter $f(t)$. |
| `Mosq_Human_NoW.R` | Model 4: mosquito + human only, **without** weather forcing. The most parsimonious configuration. |

### Evaluation scripts

| File | Description |
|---|---|
| `Fit_WIS.R` | Computes the Weighted Interval Score (WIS) for in-sample model fits (not forecasts) of all four model configurations, across all 15 study years and all three surveillance targets. |
| `Ensemble_RelWIS.R` | Constructs all five ensemble forecasts (quantile averaging, quantile median, regression-based weighting, WIS-optimized weighting, and the linear pool) from the four individual model forecasts, and computes the log-scale relative WIS ($\log(\mathrm{WIS}_{\mathrm{model}} / \mathrm{WIS}_{\mathrm{baseline}})$) for each ensemble method against the historical baseline. |

### Folders

| Folder | Description |
|---|---|
| `datasets/` | Raw and processed input data: weekly mosquito trap counts, mosquito pool testing results (for IM1000), human WNV case counts, and daily temperature/precipitation from PRISM. |
| `outputs/` | Saved model outputs, including posterior ensemble arrays, fit and forecast WIS objects, and ensemble forecast results used to generate the manuscript's figures and tables. |
