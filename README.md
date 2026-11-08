<p align="center">
  <img src="man/figures/worknoiser_logo.png" width="520" alt="WorknoiseR logo">
</p>

# WorknoiseR

**Español** | **English**

WorknoiseR es un paquete R y una aplicación Shiny bilingüe para la evaluación de la exposición laboral al ruido y la modelización espacial de niveles sonoros. / WorknoiseR is a bilingual R package and Shiny application for occupational noise exposure assessment and spatial noise modelling.

## Español

### Autores

- **Raúl Aguilar-Elena** — autor y mantenedor (`aut`, `cre`). ORCID: 0000-0001-8656-8155. Universidad Internacional de Valencia (VIU).
- **Ana Delgado-Garcia** — autora (`aut`). ORCID: 0009-0003-7079-5507. Universidad de Salamanca (USAL).
- **Alejandro Guillén Riquelme** — autor (`aut`). ORCID: 0000-0002-6373-5237. Universidad Internacional de Valencia (VIU).

La estructura de autoría coincide con `expoquimR`.

### Versión

Versión bilingüe: **0.5.0**.

La versión 0.5.0 incorpora internacionalización español/inglés sobre el núcleo acústico validado en la serie 0.4.x. La traducción no modifica las ecuaciones ni los resultados numéricos.

### Funciones principales

WorknoiseR incluye funciones reutilizables para suma logarítmica de niveles sonoros; `LAeq,d` diario y multitramos; `LAeq,s` semanal; dosis de ruido y tiempo máximo admisible; métodos APV, HML y SNR para protectores auditivos; apoyo a la evaluación según RD 286/2006 en español y según la Directiva 2003/10/CE en inglés; propagación en campo libre; receptores puntuales; barreras; reverberación; mallas espaciales; isófonas; dominios poligonales; importación flexible de CSV; y estimación inversa de niveles de fuente.

La aplicación Shiny integra cálculos higiénicos, campañas de trabajadores y mapas de ruido 2D/3D. El idioma puede cambiarse en cualquier momento mediante el selector **Idioma / Language**.

### Idioma

El núcleo numérico mantiene una API estable e independiente del idioma. Para mensajes, resúmenes y evaluación textual:

```r
library(WorknoiseR)

set_worknoiser_language("es")
worknoiser_language()
evaluate_rd286(85, 137)

set_worknoiser_language("en")
evaluate_rd286(85, 137)
```

La aplicación puede iniciarse directamente en español:

```r
run_noise_app(language = "es")
```

### Instalación desde el paquete fuente

```r
install.packages("WorknoiseR_0.5.0.tar.gz", repos = NULL, type = "source")
library(WorknoiseR)
```

### Ejemplos

```r
db_add(c(80, 80))
laeq_d(85, 8)
laeq_d_multi(c(90, 85, 80), c(2, 3, 1))
laeq_s(c(80, 82, 84, 86, 88))
noise_dose(85, 4)
max_exposure_time(88)
```

### Validación

Desde el directorio del proyecto:

```r
source("VALIDAR_RELEASE.R")
devtools::test()
devtools::check()
```

Los tests bilingües verifican que el cambio de idioma modifica únicamente las etiquetas y mensajes, no los valores acústicos calculados.

## English

### Authors

- **Raúl Aguilar-Elena** — author and maintainer (`aut`, `cre`). ORCID: 0000-0001-8656-8155. Universidad Internacional de Valencia (VIU).
- **Ana Delgado-Garcia** — author (`aut`). ORCID: 0009-0003-7079-5507. Universidad de Salamanca (USAL).
- **Alejandro Guillén Riquelme** — author (`aut`). ORCID: 0000-0002-6373-5237. Universidad Internacional de Valencia (VIU).

The authorship structure matches `expoquimR`.

### Version

Bilingual version: **0.5.0**.

Version 0.5.0 adds Spanish/English internationalisation on top of the validated 0.4.x acoustic core. Translation does not alter equations or numeric results.

### Main capabilities

WorknoiseR provides reusable functions for logarithmic sound-level addition; daily and multi-task `LAeq,d`; weekly `LAeq,s`; noise dose and maximum allowable exposure time; APV, HML and SNR hearing-protector calculations; support for assessment under Directive 2003/10/EC in English (using LEX,8h and LEX,w terminology) and Spanish RD 286/2006 in Spanish; free-field propagation; point receivers; barriers; reverberation; spatial grids; noise contours; polygonal domains; flexible CSV import; and inverse source-level estimation.

The Shiny application integrates occupational-hygiene calculations, worker campaigns and 2D/3D noise mapping. Language can be switched at any time using the **Idioma / Language** selector.

### Language

The numerical core keeps a stable, language-neutral API. For human-readable messages, summaries and textual assessment:

```r
library(WorknoiseR)

set_worknoiser_language("en")
worknoiser_language()
evaluate_rd286(85, 137)

set_worknoiser_language("es")
evaluate_rd286(85, 137)
```

Launch the application directly in English:

```r
run_noise_app(language = "en")
```

### Installation from source

```r
install.packages("WorknoiseR_0.5.0.tar.gz", repos = NULL, type = "source")
library(WorknoiseR)
```

### Examples

```r
db_add(c(80, 80))
laeq_d(85, 8)
laeq_d_multi(c(90, 85, 80), c(2, 3, 1))
laeq_s(c(80, 82, 84, 86, 88))
noise_dose(85, 4)
max_exposure_time(88)
```

### Validation

From the project directory:

```r
source("VALIDAR_RELEASE.R")
devtools::test()
devtools::check()
```

Bilingual tests verify that changing language affects labels and messages only, not calculated acoustic values.

## Citation / Cita

```r
citation("WorknoiseR")
```

## License / Licencia

MIT License.
