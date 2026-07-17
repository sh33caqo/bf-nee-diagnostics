# render_website.R

project_dir <- "~/Symobio/BF_NEE_Diagnostics_Website"

setwd(project_dir)

required_pages <- c(
  "index.qmd",
  "00_methodological_framework.qmd",
  "01_processing_status.qmd",
  "02_global_trends.qmd",
  "03_country_drivers.qmd",
  "04_commodity_drivers.qmd",
  "05_maps.qmd",
  "06_regional_regressions.qmd",
  "_quarto.yml"
)

missing <- required_pages[!file.exists(required_pages)]

if (length(missing) > 0) {
  stop(
    "Missing files: ",
    paste(missing, collapse = ", ")
  )
}

system(
  "quarto render",
  intern = FALSE
)