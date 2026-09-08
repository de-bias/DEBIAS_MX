# Workflow overview

This guide describes the measurement and modelling stages. See [figure-reproducibility.md](figure-reproducibility.md) for individual figures, settings, export paths and known gaps.

Paths are relative to the repository root. 

## Regenerating plots from saved results

For plotting, start with the Python notebooks in [manuscript/figures/code](../manuscript/figures/code/). They read supplied municipal bias data, model summaries and SHAP outputs. The expensive model-fitting and tuning stages are not required for this route. Use that directory as the working directory and follow the instructions in the figure guide.


## Upstream workflow

| Stage | Purpose | Code and inputs | Outputs |
|---|---|---|---|
| 1. Municipal input preparation | Supply municipal counts, covariates and boundaries. | Inputs in `data/raw` are already aggregated; these are not raw device-level or tile-level traces. The wider covariate tables are supplied in `data/processed/explain_bias`. | Starting data for measurement and modelling. |
| 2. Coverage-bias measurement | Join census and MPD counts; calculate bias and spatial lags. | [measure_bias_full.qmd](../code/01_measure_bias/measure_bias_full.qmd), using census population, an active-population CSV and municipal boundaries. | `results/measure_bias/<source>/Active_population_bias_nonspatial.csv` and spatial variants such as `Active_population_bias_queen.csv` and `Active_population_bias_knn.csv`. |
| 3. Prepared modelling inputs | Supply bias, spatial lags, covariates and variable labels for each validation workflow. | `data/processed/explain_bias/blocked_cv`, `holdout` and selected `legacy_inputs` files. | Inputs read directly by the modelling QMDs and some figure notebooks. |
| 4. Original block-CV models | Fit models and summarise performance and SHAP contributions. | [explain_bias_blocked_cv_gridsearch.qmd](../code/02_explain_bias/explain_bias_blocked_cv_gridsearch.qmd). Select source, bias specification and matching output folder. | `results/explain_bias/blocked_cv/Final_Results_BlockedCV_GS5184_<source>_<specification>`. |
| 5. Original holdout models | Fit models using the holdout validation workflow. | [explain_bias_holdout_gridsearch_pvalue.qmd](../code/02_explain_bias/explain_bias_holdout_gridsearch_pvalue.qmd). Select source, specification and output folder. | `results/explain_bias/holdout/Final_Results_HoldOut_GS5184_<source>_<specification>`. |
| 6. Alternative random-search models | Run the alternative block-CV tuning workflow. | [explain_bias_blocked_cv_randomsearch.qmd](../code/02_explain_bias/explain_bias_blocked_cv_randomsearch.qmd). | Block-CV folders with `RS1000` in their names.|
| 7. Population and households sensitivity | Refit after excluding `Total_Population` and `Total_houses`, reusing each original specification's saved hyperparameters. | [explain_bias_blocked_cv_gridsearch_no_pop_house.qmd](../code/02_explain_bias/explain_bias_blocked_cv_gridsearch_no_pop_house.qmd). Run separately with `selected_run` set to `TTS_Non`, `TTS_Knn`, `P05_Non` and `P05_Knn`. | Four run folders under `results/explain_bias/blocked_cv/sensitivity_no_pop_house`. |
| 8. Larger-municipality sensitivity | Compare source-specific full samples with municipalities at least five times the assumed tile area. | [sensitivity_large_municipalities.ipynb](../manuscript/figures/code/sensitivity_large_municipalities.ipynb). Reads supplied bias/count files and boundaries; threshold currently 100 square kilometres. | `manuscript/figures/data/sensitivity_large_municipalities_metrics.csv`. |
| 9. Plotting and assembly | Draw panels and assemble manuscript figures/tables. | Python notebooks listed in [figure-reproducibility.md](figure-reproducibility.md). | Panels under `manuscript/figures/plots`; manually assembled assets under `manuscript/figures/visualisations`, `manuscript/supplementary-figures` and `manuscript/tables`. |

## Settings and result names

- `P05` denotes the multi-app model runs; `TTS` denotes the Facebook model runs.
- `Non` denotes covariate-only models, `Knn` the k-nearest-neighbour spatial lag and `Que` the queen-contiguity lag.
- `GS5184` and `RS1000` identify the grid-search and random-search workflows, respectively.
- Run the measurement QMD with its working directory in `code/01_measure_bias`; run the modelling QMDs from `code/02_explain_bias`. Their relative file paths and shared style-file imports depend on these locations.
- In the measurement QMD, change both the active-population input and `Outputs` for each source/window. It is currently configured for `phone_00_05`; it does not automatically run all alternatives.
- In the original modelling QMDs, review `Path03` (bias input) and `Outputs` together, plus the corresponding covariate, label and boundary paths. A single run does not generate all source/specification combinations. Use a fresh R session between configurations.
- The sensitivity QMD maps `selected_run` to the matching bias input, original `Optimal_parameters.csv` and sensitivity output folder automatically. All four original parameter files are prerequisites for rerunning this sensitivity analysis.

Useful saved block-CV outputs within each run folder are:

| File | Purpose |
|---|---|
| `Final_Blocked_CV_Performance.csv` | Aggregated model performance for reporting. |
| `Optimal_parameters.csv` | Saved tuning parameters; also used by the population/households sensitivity workflow. |
| `01.Feature Importance/Feature_Importance_CV_Avg.csv` | Average feature importance for bar charts. |
| `03.Shap Dependence/Shap_Dependence_CV_Avg.csv` | SHAP data used by the manuscript map/dependence notebook. |
| `Predictions/` | Training/test predictions by fold. |

Holdout importance panels read `01.Feature Importance/Feature_Importance.csv` from the corresponding holdout run folder.
