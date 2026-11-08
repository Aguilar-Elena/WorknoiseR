# WorknoiseR 0.5.0 — Bilingual release candidate / Candidato de versión bilingüe

## Español

Esta versión introduce internacionalización español/inglés sobre el núcleo acústico previamente validado. No se han rediseñado las fórmulas de exposición, protectores, propagación, barreras, reverberación, mallas, isófonas ni estimación inversa.

Antes de declarar la 0.5.0 como release estable deben ejecutarse nuevamente `devtools::test()` y `devtools::check()` en R, además de una comprobación visual de ambas lenguas en la aplicación Shiny.

## English

This version introduces Spanish/English internationalisation on top of the previously validated acoustic core. Exposure, hearing-protector, propagation, barrier, reverberation, grid, contour and inverse-estimation formulas have not been redesigned.

Before declaring 0.5.0 a stable release, run `devtools::test()` and `devtools::check()` again in R and visually verify both languages in the Shiny application.
