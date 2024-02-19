checkSuggests <- function() {
  suppressWarnings(all(unlist(lapply(
    c("ggplot2", "plotly", "ggraph", "DT", "magick", "withr", "cowplot", "knitr", "testthat"),
    require,
    character.only = TRUE,
    quietly = TRUE
  ))))
}
