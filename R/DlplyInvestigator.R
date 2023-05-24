DlplyInvestigator <- R6::R6Class(
  classname = "DlplyInvestigator",
  inherit = CallInvestigator,
  public = list(),
  private = list(
    pattern = "[plyr::]?dlply",

    getCallsFromLine = function(lines) {
      indices <- grep(pattern = private$pattern, lines)

      lapply(indices, function(index) {
        funCall <- paste0(private$getMultiLineFun(index, lines), collapse = " ")
        funCall %>%
          stringr::str_remove_all("\\s") %>%
          stringr::str_split_i(pattern = "dlply\\(", i = 2) %>%
          stringr::str_split_i(pattern = ",", i = 4) %>%
          stringr::str_extract(pattern = "\\=\\w+") %>%
          stringr::str_extract(pattern = "\\w+")
      })
    }
  )
)
