# MemoryDecay <img src="man/figures/BLANCO.png" style="float: right; height: 110px; margin-top: -10px;" />

`MemoryDecay` is an  R package provides an end-to-end framework for **modeling memory decay** and **collective attention** over time. It is especially useful for analyzing how cultural knowledge fades, how citations evolve, or how survey responses about historical figures decline as a function of time.

This package is grounded in recent research on collective memory dynamics, including:

- Candia et al. (2019) *Nature Human Behaviour* [doi:10.1038/s41562-018-0474-5](https://doi.org/10.1038/s41562-018-0474-5)  
- Candia & Uzzi (2021) *American Psychologist* [doi:10.1037/amp0000863](https://doi.org/10.1037/amp0000863)

---

MemoryDecay combines **robust statistical models**, **publication-ready plots**, and **modular functions** to support social scientists, data analysts, and computational researchers working with memory-related time data.


## Core Features

- Fit **theoretical forgetting curves** to attention data using nonlinear least squares (NLS)
- Support for **grouped** and **faceted** model fitting (e.g., country, attention level)
- Works on **cross-sectional**, **survey-based**, and **time-series** data
- Supports **early decay weighting** to emphasize short-term memory loss
- Computes **model fit metrics**: AIC, BIC, pseudo R², critical time
- Provides **visual diagnostics and comparisons** for all models

---
Aquí tienes la sección **“Models Implemented”** reescrita para que se vea correctamente en GitHub (`README.md`), sin depender de LaTeX ni MathJax:

---

###  Models Implemented

#### 1. Biexponential Decay (Communicative + Cultural Memory)

```
S(t) = N * [ exp(-(p + r) * t) + (r / (p + r - q)) * (exp(-q * t) - exp(-(p + r) * t)) ]
```

- Models **dual systems** of memory: fast-decaying communicative and slow-decaying cultural
- Includes **critical time `t_c`** where both systems contribute equally
- Log-transformed for better numerical stability

---

#### 2. Exponential Decay (Simple Memory Loss)

```
S(t) = c * exp(-q * t)
```

- Assumes memory or attention fades at a constant exponential rate

---

#### 3. Log-normal Modulated Power Law

```
S(t) = exp(b) * t^b1 * exp(-b2 * (log(t))^2)
```

- Captures **initial attention bursts** and **long-tail decay**
- Common in **viral media**, **citations**, and **digital traces**


---

##  Main Functions

| Task | Function |
|------|----------|
| Fit models (all) | `fit_all_models_log()` |
| Individual fits | `fit_biexponential_model()`, `fit_exponential_log_model()`, `fit_lognormal_log_model()` |
| Compute critical time | `add_critical_time()` |
| Visualize raw vs smooth | `plot_raw_memory_decay()` |
| Compare all models | `plot_all_models()`, `compare_model_fits()` |
| High-quality export | `plot_fitted_decay_for_publication()` |

---

##  Data Preprocessing

MemoryDecay includes tools to process noisy **survey** and **time-series** data:

- `smooth_survey_decay()`: Smooths memory survey data using **LOESS** and **GAM**. Supports up to two grouping variables (`group_var`, `group_var2`) and returns raw, mean, and smoothed responses.
- `process_time_series_bins()`: Pipeline for turning **citation time series** into grouped decay bins via **log-space binning**, controlling for **preferential attachment**.

---

##  Built-in Datasets

| Dataset | Description |
|---------|-------------|
| `cross_section_data` | Popularity of songs on Billboard as of Oct 2016 with age and controls |
| `survey_data` | Cross-cultural memory survey of historical/cultural icons with recall accuracy, geographic match, and demographics |
| `time_series_data` | Citation time series from 1980–2003, with inflation-adjusted counts and semester resolution for anonymized scientific papers |

---

##  Visualization Utilities

### Compare observed and fitted curves
```r
plot_fitted_decay(model_output)
```

### Compare all models per group
```r
plot_all_models(model_outputs)
```

### Plot raw patterns

```r
plot_raw_memory_decay()
```

> Visualizes memory decay across **all data types** — survey, cross-sectional, and time-series — by plotting raw values, aggregated responses, or both.  
> Compatible with output from smoothing functions (`smooth_survey_decay()`), model fitting (`fit_*`), or custom aggregates.

```r
plot_raw_memory_decay(
  raw_df = survey_data,
  aggregated_df = smooth_survey_decay(...),
  age_var = "age_metric",
  response_var_raw = "performance_score",
  response_var_agg = "mean_response",
  group_var = "location_flag"
)
```

- Supports optional `group_var` and `group_var2` for color and faceting
- Can apply log-scales and custom axis limits
- Smart handling of missing or zero values for log-scale safety


### Compare model AIC/BIC scores
```r
compare_model_fits(model_outputs$model_comparison, metric = "AIC")
```

---

##  Use Cases

- **Collective memory decay** of historical or cultural figures
- **Attention fade** in citation networks or digital content
- Cross-country comparisons of **icon recall accuracy**
- Analyzing **early vs late memory contributions**
- Evaluating the **fit and interpretability** of theoretical decay models

---

# Installation

```r
# Install development version from local source
install.packages("./MemoryDecay", repos = NULL, type = "source")

# Or clone and install from GitHub (once available)
# remotes::install_github("crcandia/MemoryDecay")
```

---

# Example: Fitting a Forgetting Curve

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

# Features

-  Curve fitting for time-series and survey-based memory
-  Model comparison tools: AIC, BIC, pseudo-R²
-  Critical time computation
-  Useful for researchers in memory, attention, and cultural studies
-  Tested on historical fame, Wikipedia, and citation decay datasets

---

# License

This package is released under the MIT license. See `LICENSE` file for details.

---

# Citation

If you use this package in academic work, please cite:

> Candia, C., & Uzzi, B. (2021). The memory of science: Using citation dynamics to understand memory decay. *American Psychologist*, 76(7), 1060–1075. https://doi.org/10.1037/amp0000863
> Candia, C., Jara-Figueroa, C., Rodriguez-Sickert, C., Barabási, A.-L., & Hidalgo, C. A. (2019).  The universal decay of collective memory and attention. *Nature Human Behaviour*, 3(1), 82–91. https://doi.org/10.1038/s41562-018-0474-5
---

Perfecto, aquí tienes un bloque para agregar al final del README bajo una nueva sección `## Learn More`, con enlaces a las viñetas del paquete:

---

# Learn More

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


