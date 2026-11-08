# WorknoiseR 0.5.0
* Fixed dynamic bilingual rendering in the Shiny application: `verbatimTextOutput`/`<pre>` content is now translated, and the compliance placeholder is generated server-side in the selected language.
* Expanded English translations for dynamic notifications, protector-method messages, weekly exposure messages and map/import warnings.


* Legal-framework internationalisation: Spanish UI uses RD 286/2006; English UI uses Directive 2003/10/EC, with EU peak values also reported in Pa.
* Added `evaluate_noise_compliance()`; retained `evaluate_rd286()` as a backward-compatible Spanish-framework wrapper.

## Español

- Incorporada una capa de internacionalización español/inglés sin duplicar el motor de cálculo.
- Añadidas `worknoiser_language()`, `set_worknoiser_language()` y `worknoiser_translate()`.
- `evaluate_rd286()` admite `language = "es"` o `language = "en"` y conserva idéntica interpretación numérica.
- La aplicación Shiny incorpora selector de idioma y traducción reactiva de navegación, controles, mensajes, tablas y resultados dinámicos.
- Las plantillas y exportaciones de campañas Excel adaptan nombres de hojas y etiquetas al idioma seleccionado.
- `run_noise_app()` permite fijar el idioma inicial.
- Añadidos tests específicos de equivalencia numérica entre español e inglés.
- Se mantiene la autoría alineada con `expoquimR`: Raúl Aguilar-Elena, Ana Delgado-Garcia y Alejandro Guillén Riquelme.

## English

- Added a Spanish/English internationalisation layer without duplicating the calculation engine.
- Added `worknoiser_language()`, `set_worknoiser_language()` and `worknoiser_translate()`.
- `evaluate_rd286()` supports `language = "es"` or `language = "en"` while preserving identical numerical interpretation.
- The Shiny application now includes a language selector and reactive translation of navigation, controls, messages, tables and dynamic results.
- Excel campaign templates and exports adapt sheet names and labels to the selected language.
- `run_noise_app()` can set the initial language.
- Added tests specifically checking numerical equivalence between Spanish and English.
- Authorship remains aligned with `expoquimR`: Raúl Aguilar-Elena, Ana Delgado-Garcia and Alejandro Guillén Riquelme.

# WorknoiseR 0.4.1

- Stable acoustic-core release validated with 18/18 unit tests and a clean `R CMD check --as-cran` result in the validated environment.
- Added package authorship, citation metadata and portability corrections.

### i18n stability fix

* Reworked browser-side translation so language changes are idempotent and reversible.
* Text and attributes are always rendered from the canonical Shiny source instead of from a prior translation.
* Added word-boundary-safe matching to prevent partial replacements such as `Result` inside `Resultados`.
