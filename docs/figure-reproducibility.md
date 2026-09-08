# Figure reproducibility guide

This guide maps manuscript figures and supplementary items to the notebooks that generate their source panels or numerical results. Final layouts and several tables were assembled manually from these outputs; the notebooks do not build the complete manuscript figures automatically.

Paths below are relative to the repository root unless stated otherwise. See [workflow.md](workflow.md) for upstream processing and model fitting. This guide is based on code inspection, not a fresh execution of all workflows.

## Running the plotting notebooks

1. Open the notebooks in [manuscript/figures/code](../manuscript/figures/code/) with the kernel working directory set to that folder. Relative paths such as `../../../results` depend on this.
2. Run the imports and setup cells, then select the dataset and specification described below. Use a fresh kernel when switching workflows to avoid carrying over variables.
3. Use the saved CSV results supplied in the repository. Model refitting is not needed just to draw these panels.
4. Check the export cell before running it. Several `plt.savefig(...)` calls are commented out: those panels display in the notebook but are not saved unless the export is enabled. Create any missing output directories first. Existing output files with the same name will be overwritten.
5. Assemble the exported panels manually into the final figure layout, including panel letters and any labels omitted by the notebook.

The notebooks import NumPy, pandas, Matplotlib, SciPy, GeoPandas, jenkspy, statsmodels and, in several notebooks, prophet. Spatial plots also import mapclassify, libpysal, esda and splot; the correlation notebook imports seaborn. Install packages used in the import cells even where an import is not needed by the selected panel. The configured plotting font is DejaVu Sans. Package versions are not pinned in these notebooks.

## Main manuscript figures

| Manuscript item | Source notebook | Inputs, settings and exports |
|---|---|---|
| Figure 1: analytical workflow (`fig:overview`) | No figure-generation notebook identified. | Conceptual diagram summarising the measurement, spatial analysis, modelling and interpretation stages. Final layout is manual. |
| Figure 2: coverage comparison, scaling, ranks and Lorenz curves (`fig:distribution`) | [figure-1.ipynb](../manuscript/figures/code/figure-1.ipynb) | Reads `Active_population_bias_nonspatial.csv` from `results/measure_bias/facebook_tts` and `phone_00_08`. Panels correspond to coverage comparison, population scaling, rank distribution and Lorenz curves. Export calls for `coverage-fb-mp.pdf`, `scaling-coverage-pop.pdf`, `size-rank-fb-mp.pdf` and `lorenz-fb-mp.pdf` under `manuscript/figures/plots` are commented out. |
| Figure 3: bias maps, LISA maps and Moran plots (`fig:spatial`) | [figure-2.ipynb](../manuscript/figures/code/figure-2.ipynb) | Reads non-spatial bias from `results/measure_bias/phone_00_05` and `facebook_tts`; queen-lag inputs from `data/processed/explain_bias/blocked_cv`; municipal boundaries from `data/raw/spatial/municipalities_measure_bias`; and state boundaries from `data/raw/spatial/gadm/States SHP`. Bias-map and Moran-plot exports are commented out. LISA exports are active: `plots/lisa-mp.pdf` and `plots/lisa-fb.pdf` relative to `manuscript/figures`. |
| Figure 4: relative model-performance changes (`fig:performance`) | [figure-3.ipynb](../manuscript/figures/code/figure-3.ipynb) | Set `data = 'mp'` or `'fb'` and `model = 'block-cv'` for the block-CV panels. Reads the corresponding summary in `manuscript/figures/data/model-performance`. Writes `plots/bcv-performance/bcv_performance_mp.png` or `bcv_performance_fb.png`. The notebook also accepts `model = 'rho-70-30'` for holdout summaries, but output names do not distinguish validation strategies; preserve or rename exports when switching. |
| Figure 5: SHAP maps and dependence insets (`fig5`) | [figure-4.ipynb](../manuscript/figures/code/figure-4.ipynb) | Set `data = 'mp'` or `'fb'`. Reads `03.Shap Dependence/Shap_Dependence_CV_Avg.csv` from the corresponding `Final_Results_BlockedCV_GS5184_P05_Knn` or `...TTS_Knn` folder. Select each required `feature`, then rerun the downstream map and dependence cells. PNGs are saved under `plots/SHAP-map`, `SHAP-map-lisa`, `SHAP-map-lisa-feature` and `SHAP-dependence`. Combine the appropriate maps and insets manually. |

In `figure-4.ipynb`, `features = np.unique(shap['Feature'])` supplies available feature names and the current selection is `feature = features[6]`. This is a single-feature selection, not a loop over the six most important predictors. Select the names required by the final figure explicitly; array position does not represent SHAP rank.

## Supplementary figures and tables

Numbers below follow the figure/table environments in the inspected SI, not earlier reviewer numbering.

| Supplementary item | Source and settings | Output or assembly |
|---|---|---|
| Table 1: covariates (`tab:covariates`) | Descriptive table based on the covariate definitions and sources. No automatic table generator identified. | Editable artifact: `manuscript/supplementary-figures/covariates.docx`; manuscript image: `covariates.png`. |
| Table 2: model performance (`tab:performance`) | Grid-search block-CV and holdout QMD results; see [workflow.md](workflow.md). Use both sources and `Non`, `Que` and `Knn` specifications. | Manually assembled `manuscript/supplementary-figures/performance-metrics.docx` and `.png`. |
| Figure 1: pairwise covariate-bias correlations (`fig:correlations`) | [figure-pairwise-correlations.ipynb](../manuscript/figures/code/figure-pairwise-correlations.ipynb). Reads `Census_data.csv` and both sources' non-spatial bias files in `data/processed/explain_bias/legacy_inputs`. | Writes `manuscript/figures/plots/correlations/correlations_nolabels.pdf`. Both sources are calculated together; the top-level `data` setting does not select separate panels in the current calculation. Final labels/layout are manual. |
| Figure 2: spatial statistics (`tab:spatial-stats`) | [supplementary-table-moranI.ipynb](../manuscript/figures/code/supplementary-table-moranI.ipynb); set `schema = 'queen'` and then `'knn'`. Reads the corresponding prepared bias files in `data/processed/explain_bias/blocked_cv`. | Prints fitted Moran-plot slopes, standard errors and regression p-values; no table export. Editable artifact: `manuscript/supplementary-figures/moranI.docx`. See the statistical-definition caveat below. Although its label begins `tab:`, the inspected SI places this item in a figure environment. |
| Figure 3: original feature importance (`fig:importance`) | [figure-importance.ipynb](../manuscript/figures/code/figure-importance.ipynb). Use the original-data loading cell and the eight combinations described below. | Original export call is commented out; its filename pattern is `plots/importance/importance_<model>_<data>_<spatialw>.png`. Assemble panels manually. |
| Figure 4: Facebook aggregation-order comparison (`fig:tts-stt`) | [supplementary-figure-facebook.ipynb](../manuscript/figures/code/supplementary-figure-facebook.ipynb). Reads non-spatial bias/count results for `facebook_tts` and `facebook_stt`. | Writes `manuscript/figures/plots/tts-stt.pdf`; final SI image is `figure-tts-stt.png`. This notebook compares already aggregated counts; it does not implement tile aggregation. |
| Table 3: hyperparameters (`tab:hyperparameters`) | Saved parameter outputs from the original modelling workflows, including `Optimal_parameters.csv` in block-CV run folders. | Manually assembled `manuscript/supplementary-figures/hyperparameters.docx` and `.png`. |
| Sensitivity: performance without population/households (number pending) | `Final_Blocked_CV_Performance.csv` in each of the four run folders under `results/explain_bias/blocked_cv/sensitivity_no_pop_house`. | Manually assembled `manuscript/supplementary-figures/performance-metrics-sensitivity.docx` and `.png`. |
| Sensitivity: importance without population/households (number pending) | [figure-importance.ipynb](../manuscript/figures/code/figure-importance.ipynb), using the sensitivity loading and plotting cells. | Four PNGs with prefix `importance_sensitivity_no_pop_house_` under `manuscript/figures/plots/importance`; manually assembled as `manuscript/supplementary-figures/figure-importance-sensitivity-r1.png`. |
| Sensitivity: larger municipalities (number pending) | [sensitivity_large_municipalities.ipynb](../manuscript/figures/code/sensitivity_large_municipalities.ipynb). Current settings: `TILE_AREA_KM2 = 20.0`, `THRESHOLD_MULTIPLIER = 5` (100 square kilometres). Uses source-specific full and restricted samples. | Writes `manuscript/figures/data/sensitivity_large_municipalities_metrics.csv`. Table assembled as `manuscript/supplementary-figures/large-municipalities-sensitivity.docx` and `.png`. |

## Feature-importance panel selection

The original and sensitivity data-loading cells both assign `df`. Do not use **Run All** to generate original panels: the sensitivity loading cell will replace the original data before plotting.

For the original eight panels:

1. Run imports, font and parameter cells.
2. Choose `model = 'bcv'` (block CV) or `'rho'` (holdout), `data = 'mp'` or `'fb'`, and `spatialw = 'Non'` or `'Knn'`.
3. Run only the loading cell marked `# original run`, then the original plotting cell. Enable its export if a file is required. Skip both sensitivity cells.
4. Repeat for all eight combinations. The SI caption groups block-CV panels as A-D and holdout panels as E-H; follow the assembled figure for the order within each group.

For the four sensitivity panels, use `model = 'bcv'` and all combinations of `data = 'mp'`/`'fb'` and `spatialw = 'Non'`/`'Knn'`. Run the loading cell marked `# sensitivity analysis with no pop or no hh`, then the sensitivity plotting cell. Its export is active. This loader always uses sensitivity block-CV outputs, irrespective of the `model` variable.

Both plotting cells currently contain fixed x-axis ticks. Check their suitability for the selected source and match the final figure's axes rather than assuming one tick range suits all panels.

