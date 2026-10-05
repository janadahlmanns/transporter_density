# transporter_density: vGlut transporter density in microscopy images

R tools for quantifying vesicular glutamate transporter (vGlut) immunofluorescence in two-channel microscopy images and reporting the results.

## How the quantification works

Illumination and imaging settings differ between recordings, so absolute fluorescence is not comparable across images. The analysis therefore:

1. lets the user set an **intensity threshold per channel** (green: vGlut, red: the neuronal marker MAP2); everything below is treated as background,
2. sums the green fluorescence above threshold over the field of view,
3. **normalizes it to the amount of neuronal tissue in the image**, either by the summed red intensity or, preferably, by the red above-threshold **area**.

## Contents

- `vGlut_Einzelmessung.R`: **Shiny app** for analysing a single `.tif` image. Choose the file, set the thresholds, assign an experimental condition, and the result object is saved as an `.rds` file.
- `results_report_vGlut_analysis.Rmd`: **R Markdown report** that collects all saved results, shows thumbnails of every analyzed image, explains each analysis step, compares the two normalizations per condition, and ends with a ready-to-use methods paragraph and a results table (also exportable as CSV). `reference_doc.docx` is the Word template for the output.

## Run

```r
install.packages(c("shiny", "shinythemes", "shinyjs", "tidyverse", "rmarkdown"))
BiocManager::install("EBImage")
shiny::runApp("vGlut_Einzelmessung.R")                    # analyse images one by one
rmarkdown::render("results_report_vGlut_analysis.Rmd")    # build the report
```

## Related

[mitochondria_alcohol](https://github.com/janadahlmanns/mitochondria_alcohol) uses the same human-in-the-loop approach for another image-analysis task.
