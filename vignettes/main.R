library(knitr)
library(rmarkdown)
render("vignette.Rmd", output_format = "md_document")
render("vignette.Rmd", output_format = "html_document")


