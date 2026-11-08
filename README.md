# WorknoiseR <img src="man/figures/worknoiser_logo.png" align="right" height="180" alt="WorknoiseR logo"/>

**Occupational Noise Exposure Assessment and Spatial Mapping**

<!-- badges: start -->
[![GitHub release](https://img.shields.io/github/v/release/Aguilar-Elena/WorknoiseR?display_name=tag&sort=semver)](https://github.com/Aguilar-Elena/WorknoiseR/releases)
[![R-CMD-check](https://github.com/Aguilar-Elena/WorknoiseR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Aguilar-Elena/WorknoiseR/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

`WorknoiseR` is an R package for **occupational noise exposure assessment, hearing-protector evaluation and spatial noise mapping**.

It is designed for occupational hygienists, health and safety technicians, prevention practitioners, researchers and engineers who need to calculate worker noise exposure, evaluate hearing protection, interpret results against regulatory action and limit values, and model the spatial distribution of sound levels in workplaces.

Unlike isolated calculators or spreadsheets, `WorknoiseR` exposes the calculation core as small, independently callable and unit-tested R functions. This makes assessments reproducible, auditable and suitable for automation without depending on a graphical interface. An optional bilingual Shiny application provides a guided interactive workflow for practitioners who prefer not to write code.

<hr>

## Why WorknoiseR?

Occupational noise assessment usually requires several separate operations: energetic addition of sound levels, daily and weekly exposure calculations, hearing-protector assessment, regulatory interpretation and, in many workplaces, spatial modelling of noise sources and receivers.

These tasks are often distributed across spreadsheets, calculators and specialist software, making the workflow difficult to reproduce and audit.

`WorknoiseR` was created to bring these calculations together in a single open and reproducible R workflow.

It helps answer questions such as:

- What is the daily exposure level for a worker carrying out several tasks?
- What is the weekly exposure when daily exposure varies?
- Has a lower or upper exposure action value been reached?
- Is the exposure limit value exceeded?
- What protected level is obtained using APV, HML or SNR?
- How does sound level decay with distance?
- What level is expected at a specific receiver?
- How do several noise sources combine energetically?
- How do barriers and approximate reverberation affect the predicted sound field?
- What do the noise contours look like inside an irregular workplace boundary?
- Can source sound levels be estimated from field measurements?

<hr>

## What does WorknoiseR do?

`WorknoiseR` integrates four main areas:

1. **Occupational exposure** — logarithmic sound-level addition, daily and weekly exposure, noise dose, maximum admissible exposure time and regulatory interpretation.

2. **Hearing protection** — APV, HML and SNR methods for estimating protected noise exposure.

3. **Spatial noise modelling** — free-field propagation, point receivers, energetic combination of multiple sources, barriers, approximate reverberation, noise grids, contours and inverse source-level estimation.

4. **Interactive and data-driven workflows** — flexible CSV import and a bilingual Shiny application integrating exposure calculations, worker campaigns and 2D/3D noise mapping.

<hr>

## Regulatory framework

`WorknoiseR` separates the numerical calculation core from the jurisdiction-specific regulatory presentation layer.

- **Spanish interface:** assessment is presented with reference to **Real Decreto 286/2006**.
- **English interface:** assessment is presented with reference to **Directive 2003/10/EC**, using European terminology such as `L_EX,8h` and `L_EX,w`.

Changing language changes labels, messages and regulatory terminology only; it does not alter the underlying acoustic calculations.

<hr>

## Main functions

| Function | Area | What it does |
|---|---|---|
| `db_add()` | Exposure | Logarithmic addition of sound levels |
| `laeq_d()` | Exposure | Daily occupational noise exposure |
| `laeq_d_multi()` | Exposure | Daily exposure from multiple tasks or exposure periods |
| `laeq_s()` | Exposure | Weekly occupational noise exposure |
| `laeq_weekly()` | Exposure | Weekly exposure helper |
| `frequency_weighting()` | Exposure | Frequency-weighting correction |
| `noise_dose()` | Exposure | Noise dose calculation |
| `max_exposure_time()` | Exposure | Maximum admissible exposure time |
| `evaluate_rd286()` | Regulation | Spain-specific assessment under RD 286/2006 |
| `evaluate_noise_compliance()` | Regulation | Language-aware regulatory assessment |
| `apv_protection()` | Protection | Hearing-protector assessment using APV values |
| `hml_protection()` | Protection | Hearing-protector assessment using HML |
| `snr_protection()` | Protection | Hearing-protector assessment using SNR |
| `level_at_distance_free_field()` | Spatial | Free-field level at a given distance |
| `distance_for_level_free_field()` | Spatial | Distance corresponding to a target level |
| `receiver_level()` | Spatial | Predicted level at a point receiver |
| `noise_grid()` | Spatial | Spatial noise grid |
| `noise_contours()` | Spatial | Noise contour extraction |
| `estimate_source_levels()` | Spatial | Inverse estimation of source levels |
| `read_noise_csv()` | Import | Flexible occupational-noise CSV import |
| `set_worknoiser_language()` | Language | Sets the active language |
| `worknoiser_language()` | Language | Returns the active language |
| `run_noise_app()` | Shiny | Launches the interactive application |

<hr>

## Installation

```r
# Development version from GitHub:
# install.packages("remotes")
remotes::install_github("Aguilar-Elena/WorknoiseR")
```

Then:

```r
library(WorknoiseR)
```

A source archive can also be installed directly:

```r
install.packages(
  "WorknoiseR_0.5.0.tar.gz",
  repos = NULL,
  type = "source"
)
```

<hr>

## Language

`WorknoiseR` is fully bilingual in **English** and **Spanish**.

```r
worknoiser_language()             # query current language
set_worknoiser_language("es")     # switch to Spanish
set_worknoiser_language("en")     # switch back to English
```

The Shiny application includes an in-app **Idioma / Language** selector.

<hr>

## Minimal usage

### Logarithmic addition

```r
library(WorknoiseR)

db_add(c(80, 80))
```

### Daily exposure

```r
laeq_d(85, 8)
```

### Daily exposure from multiple tasks

```r
laeq_d_multi(
  levels = c(90, 85, 80),
  hours  = c(2, 3, 1)
)
```

### Weekly exposure

```r
laeq_s(c(80, 82, 84, 86, 88))
```

### Noise dose

```r
noise_dose(85, 4)
```

### Maximum admissible exposure time

```r
max_exposure_time(88)
```

<hr>

## Regulatory assessment

### Spain — Real Decreto 286/2006

```r
set_worknoiser_language("es")

evaluate_noise_compliance(
  laeq_d = 85,
  lcpeak = 137,
  language = "es"
)
```

### European framework — Directive 2003/10/EC

```r
set_worknoiser_language("en")

evaluate_noise_compliance(
  laeq_d = 85,
  lcpeak = 137,
  language = "en"
)
```

<hr>

## Hearing protection

### SNR

```r
snr_protection(
  lceq_t = 100,
  snr = 25
)
```

### HML

```r
hml_protection(
  laeq_t = 95,
  lceq_t = 100,
  H = 30,
  M = 25,
  L = 20
)
```

### APV

```r
apv_protection(
  levels_a = c(90, 92, 95, 96, 94, 91, 88),
  frequencies = c("125", "250", "500", "1000", "2000", "4000", "8000"),
  mf = c(20, 22, 24, 26, 28, 30, 32),
  sf = rep(3, 7)
)
```

<hr>

## Spatial modelling

### Free-field propagation

```r
level_at_distance_free_field(
  level_1m = 100,
  distance_m = 10
)
```

### Point receiver

```r
sources <- data.frame(
  x = 0,
  y = 0,
  level = 100
)

receiver_level(
  x = 10,
  y = 0,
  sources = sources
)
```

### Noise grid and contours

```r
grid <- noise_grid(
  sources = sources,
  domain = c(0, 20, 0, 20),
  resolution = 2
)

noise_contours(
  grid,
  breaks = c(75, 80, 85, 90)
)
```

<hr>

## Interactive application

```r
run_noise_app()                 # default application
run_noise_app(language = "es") # Spanish
run_noise_app(language = "en") # English
```

The Shiny application integrates occupational-hygiene calculations, hearing protection, worker exposure data, regulatory interpretation and 2D/3D spatial noise mapping.

<hr>

## CSV import

`WorknoiseR` includes flexible CSV import designed to accept common occupational-noise field names and European CSV conventions.

```r
data <- read_noise_csv("noise_measurements.csv")
```

The import layer supports alternative column names and common decimal/separator conventions.

<hr>

## Outputs

| Output | Description |
|---|---|
| Daily exposure | Daily occupational noise exposure |
| Weekly exposure | Weekly occupational noise exposure |
| Regulatory assessment | Action-value and exposure-limit interpretation |
| Noise dose | Percentage dose under the selected criterion |
| Maximum exposure time | Maximum admissible duration for the selected level |
| Protected level | Estimated protected level using APV, HML or SNR |
| Receiver level | Predicted sound level at a point |
| Noise grid | Spatial table of predicted sound levels |
| Noise contours | Contour coordinates for selected levels |
| Source estimates | Estimated source levels from measurement data |

<hr>

## Validation

Version **0.5.0** has been validated with:

- **39/39 automated tests passing**;
- **0 failures**;
- **0 warnings**;
- **0 skipped tests**;
- **R CMD check --as-cran: 0 errors, 0 warnings, 0 notes**.

The repository includes an automated **R-CMD-check** GitHub Actions workflow.

<hr>

## Methodological notes

**Daily and weekly exposure:** sound levels and exposure durations are combined energetically. Weekly occupational exposure is kept distinct from the descriptive energetic mean of the days actually worked.

**Hearing protection:** APV, HML and SNR are implemented as separate approaches. The appropriate method depends on the available measurement and protector data.

**Regulatory interpretation:** the Spanish interface uses RD 286/2006 terminology; the English interface uses Directive 2003/10/EC terminology. Regulatory interpretation remains jurisdiction-sensitive.

**Spatial modelling:** propagation, barrier and reverberation modules provide engineering estimates to support occupational-hygiene assessment and visualisation. They do not replace a detailed acoustic engineering study when one is required.

<hr>

## Limitations

`WorknoiseR` is a decision-support and calculation tool and does not replace professional judgement.

The quality of an occupational noise assessment depends on representative measurements, an appropriate sampling strategy, correct identification of tasks and exposure periods, valid hearing-protector data, suitable modelling assumptions and correct application of the legislation and technical standards applicable to the workplace.

Spatial predictions are model-based estimates and should be interpreted in light of workplace geometry, source directivity, absorption, reflections, barriers and measurement uncertainty.

<hr>

## Citation

If you use `WorknoiseR` in research or professional work, please cite:

```text
Aguilar-Elena, R., Delgado-Garcia, A. & Guillén Riquelme, A. (2026).
WorknoiseR: Occupational Noise Exposure Assessment and Spatial Mapping.
R package version 0.5.0.
```

Once a Zenodo DOI is assigned, the DOI-backed citation should be preferred.

In R:

```r
citation("WorknoiseR")
```

<hr>

## Authors

**PhD. Raúl Aguilar-Elena** &middot; raguilar@universidadviu.com  
Occupational Risk Prevention and Occupational Health Research Group  
Universidad Internacional de Valencia (VIU), Valencia, Spain  
ORCID: 0000-0001-8656-8155

**Ana Delgado-Garcia** &middot; a.delgado@usal.es  
BISITE Research Group  
Universidad de Salamanca (USAL), Salamanca, Spain  
ORCID: 0009-0003-7079-5507

**PhD. Alejandro Guillén Riquelme** &middot; aguillem@universidadviu.com  
Occupational Risk Prevention and Occupational Health Research Group  
Universidad Internacional de Valencia (VIU), Valencia, Spain  
ORCID: 0000-0002-6373-5237

<hr>

## License

MIT © 2026 Raúl Aguilar-Elena, Ana Delgado-Garcia & Alejandro Guillén Riquelme
