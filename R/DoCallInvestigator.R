DoCallInvestigator <- R6::R6Class(
  classname = "DoCallInvestigator",
  inherit = CallInvestigator,
  public = list(),
  private = list(
    pattern = "do\\.call\\(",

    getCallsFromLine = function(lines) {
      indices <- grep(private$pattern, lines)

      out <- unlist(lapply(indices, function(index) {
        funCall <- paste0(getMultiLineFun(index, lines), collapse = " ")

        funCall <- funCall %>%
          stringr::str_remove_all(pattern = "\\s") %>%
          stringr::str_split_i(pattern = private$pattern, i = 2) %>%
          stringr::str_split_i(pattern = ",", i = 1) %>%
          stringr::str_remove_all(pattern = "[\"\'\\\\]")

        if (grepl("=", funCall)) {
          funCall <- funCall %>%
            stringr::str_split_i(pattern = "=", i = 2)
        }
        return(funCall)
      }))
      return(out)
    }
  )
)
