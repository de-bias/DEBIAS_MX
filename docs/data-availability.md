# Data and Code Availability Statement

The data and code supporting the paper "One country, multiple portraits: representativeness in GPS-based mobility data is source-specific and spatially dependent" are available in this GitHub repository.

The repository includes municipal-level aggregated input data, spatial boundary files, R/Quarto workflows, local analysis code, model outputs, diagnostic outputs, and manuscript-supporting figure/table artifacts. The active-population and active-device datasets are aggregated at the municipal level over a period of time and do not contain individual-level mobility records.

The main computational workflows can be found in:

- `code/01_measure_bias/measure_bias_full.qmd`
- `code/02_explain_bias/explain_bias_holdout_gridsearch_pvalue.qmd`
- `code/02_explain_bias/explain_bias_blocked_cv_gridsearch.qmd`
- `code/02_explain_bias/explain_bias_blocked_cv_randomsearch.qmd`

The final manuscript figures and tables were assembled manually from the generated outputs. The relevant computational outputs and supporting assets are included in `results/` and `manuscript/`.

Code is licensed under the MIT License. Data, documentation, figures, and other non-code materials are licensed under CC BY 4.0 unless otherwise noted.
