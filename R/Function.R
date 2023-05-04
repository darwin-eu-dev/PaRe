#' @description
#' R6 <Function> class representing a function.
Function <- R6::R6Class(
  classname = "Function",
  inherit = Code,
  public = list(
    initialize = function(name, lineStart, lineEnd, lines) {
      private$name <- name
      private$lineStart <- lineStart
      private$lineEnd <- lineEnd
      private$lines <- lines
      private$nLines <- lineEnd - lineStart + 1
      private$nArgs <- private$getNArgs()
      private$cycloComp <- private$computeCycloComp()
      return(invisible(self))
    },

    getFunction = function() {
      return(data.frame(
        name = private$name,
        lineStart = private$lineStart,
        lineEnd = private$lineEnd,
        nArgs = private$nArgs,
        cycloComp = private$cycloComp
      ))
    }
  ),
  private = list(
    lineStart = 0,
    lineEnd = 0,
    nArgs = 0,
    cycloComp = 0,

    validate = function() {
      return(invisible(self))
    },

    getNArgs = function() {
      nArgs <- private$lines[1] %>%
        stringr::str_remove_all(pattern = "\\s") %>%
        stringr::str_split_i(pattern = "function\\(", i = 2) %>%
        stringr::str_split_i(pattern = "\\)\\{", i = 1) %>%
        stringr::str_remove_all(pattern = "\\w+\\(.+\\)") %>%
        stringr::str_split(pattern = ",") %>%
        unlist() %>%
        length()
    },

    computeCycloComp = function() {
      complexity <- NA
      tryCatch({
        cyclocomp::cyclocomp(eval(parse(text = private$lines)))
      }, error = function(cond) {
        complexity <- NA
      })
    }
  )
)
