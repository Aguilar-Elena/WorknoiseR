## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* macOS Tahoe 26.3, R 4.6.1, aarch64
* GitHub Actions, Ubuntu Linux, R release

## New submission

This is the first CRAN submission of WorknoiseR.

WorknoiseR is a bilingual R package and Shiny application for occupational
noise exposure assessment, hearing-protector evaluation, regulatory
interpretation and spatial noise mapping.

The package provides a shared numerical core with Spanish and English
presentation layers. The Spanish interface uses terminology and regulatory
interpretation based on Real Decreto 286/2006, while the English interface
uses Directive 2003/10/EC terminology.

The package includes 39 automated tests. Tests of the internationalisation
layer verify that changing language does not alter the numerical acoustic
results.

There are no downstream dependencies.
