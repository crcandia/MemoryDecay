# MemoryDecay <img src="man/figures/logo.png" align="right" height="120" />

`MemoryDecay` is an R package for fitting and visualizing **forgetting curves** in the context of **collective memory, historical attention, and cultural recall**. It provides tools to model memory decay using:

- **Biexponential**, **Exponential**, and **Log-Normal Modulated Power-Law** models.
- Grouped and stratified model fitting.
- Critical time estimation.
- Model comparison (AIC/BIC).
- High-quality plotting for publication.

This package is grounded in recent research on collective memory dynamics, including:

- Candia et al. (2019) *Nature Human Behaviour* [doi:10.1038/s41562-018-0474-5](https://doi.org/10.1038/s41562-018-0474-5)  
- Candia & Uzzi (2021) *American Psychologist* [doi:10.1037/amp0000863](https://doi.org/10.1037/amp0000863)

---

## Installation

```r
# Install development version from local source
install.packages("./MemoryDecay", repos = NULL, type = "source")

# Or clone and install from GitHub (once available)
# remotes::install_github("crcandia/MemoryDecay")
```

---

## Example: Fitting a Forgetting Curve

```r
library(MemoryDecay)

# Load example dataset (scientific citation time series)
data("time_series_data")

# 1. Reshape to wide format (each column is a DOI)
wide_df <- reshape_citation_timeseries(time_series_data)

# 2. Compute cumulative citations
cumulative_df <- compute_cumulative_matrix(wide_df)

# 3. Assign decay bins (optional: use custom breakpoints)
breaks <- c(0, 1.1, 3.3, 11, 36, 60, Inf)
bin_df <- assign_decay_bins(cumulative_df, breaks = breaks)

# 4. Merge bins with original values
final_df <- merge_bins_with_original(wide_df, bin_df, replace_na_with_zero = TRUE)

# 5. Compute age since 1980 (year 0 in the dataset)
final_df$age <- final_df$time - 1980

# 6. Aggregate average attention by age and bin
temporal_data_agg <- aggregate_mean_response(
  final_df,
  age_var = "age",
  response_var = "value",
  group_var = "decay_bin"
)

# 7. Fit biexponential forgetting curves (weighted early points)
fitted_df <- fit_biexponential_model(
  temporal_data_agg,
  age_var = "age_metric",
  observed_col = "mean_response",
  group_var = "decay_bin",
  weight_early_points = TRUE
)

# 8. Plot publication-ready fitted decay curves (log scale on Y)
plot_fitted_decay_for_publication(
  fitted_df,
  observed_col = "mean_response",
  fitted_col = "fitted_correct",
  age_var = "age_metric",
  group_var = "decay_bin",
  log_y = TRUE,
  log_x = FALSE,
  export_path = "./Figures/scientific_citation"
)

```

<p align="center">
  <img src="man/figures/scientific_citation.pdf" width="500"/>
</p>

---

## Features

-  Curve fitting for time-series and survey-based memory
-  Model comparison tools: AIC, BIC, pseudo-R²
-  Critical time computation
-  Useful for researchers in memory, attention, and cultural studies
-  Tested on historical fame, Wikipedia, and citation decay datasets

---

## License

This package is released under the MIT license. See `LICENSE` file for details.

---

## Citation

If you use this package in academic work, please cite:

> Candia, C., & Uzzi, B. (2021). The memory of science: Using citation dynamics to understand memory decay. *American Psychologist*, 76(7), 1060–1075. https://doi.org/10.1037/amp0000863
> Candia, C., Jara-Figueroa, C., Rodriguez-Sickert, C., Barabási, A.-L., & Hidalgo, C. A. (2019).  The universal decay of collective memory and attention. *Nature Human Behaviour*, 3(1), 82–91. https://doi.org/10.1038/s41562-018-0474-5
---

Perfecto, aquí tienes un bloque para agregar al final del README bajo una nueva sección `## Learn More`, con enlaces a las viñetas del paquete:

---

## Learn More

To explore more use cases of the `MemoryDecay` package, see the following vignettes:

- **[Survey-Based Memory Decay](vignettes/survey_memory.html)**  
  Learn how to clean, smooth, and fit forgetting curves to noisy recall data from surveys (e.g., memory of historical figures).

- **[Scientific Citation Decay](vignettes/scientific_citations.html)**  
  Full pipeline from citation time series to grouped model fitting based on cumulative attention bins.

- **[Cross-Sectional Fame Decay](vignettes/cross_section_popularity.html)**  
  Quick modeling of fame decay using aggregated cross-sectional data with model comparisons.

You can also access the vignettes from R via:

```r
browseVignettes("MemoryDecay")
```

---

>  Tip: If you're viewing this on GitHub and the links above don't work, try installing the package locally and running `devtools::build_vignettes()`.


