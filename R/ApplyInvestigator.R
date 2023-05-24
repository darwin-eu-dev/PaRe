ApplyInvestigator <- R6::R6Class(
  classname = "ApplyInvestigator",
  inherit = CallInvestigator,
  public = list(),
  private = list(
    pattern = "[\\w+]?[Aa]pply\\(",

    getCallsFromLine = function(lines) {
      indices <- grep(private$pattern, lines)

      unlist(lapply(indices, function(index) {
        funCall <- paste0(private$getMultiLineFun(index, lines), collapse = " ")

        if (!stringr::str_detect(string = funCall, pattern = "function[ ]?\\(")) {
          funCall <- funCall %>%
            stringr::str_remove_all(pattern = "(\\s)")

          if (grepl(pattern = "cluster", x = funCall)) {
            pat <- ",(?=[FUN=]?\\w+?\\w+\\))"
          } else {
            pat <- ",(?=[FUN=]?\\w+?\\w+)"
          }

          funCall <- funCall %>%
            stringr::str_split_i(pattern = pat, i = 2)

          if (grepl(pattern = "=", x = funCall)) {
            funCall <- funCall %>%
              stringr::str_split_i(pattern = "=", i = 2)
          }

          funCall <- funCall %>%
            stringr::str_remove_all(pattern = "[\\%\\(\\)\\\\>\\<]")
          return(funCall)
        }
      }))
    }
  )
)
