#' @title
#' R6 Function class.
#'
#' @description
#' Class representing a function.
Function <- R6::R6Class(
  classname = "Function",
  inherit = Code,
  # Public ----
  public = list(
    #' @description
    #' Initializer for Function object.
    #'
    #' @param name (`character()`)\cr
    #' Name of Function.
    #' @param lineStart (`numeric()`)\cr
    #' Line number where function starts in File.
    #' @param lineEnd (`numeric()`)\cr
    #' Line number where function ends in File.
    #' @param lines (`c()`) of type (`character()`)\cr
    #' Lines of just the function in File.
    #'
    #' @return (`invisible(self)`)
    initialize = function(name, lineStart, lineEnd, lines) {
      super$initialize(name, lines)
      private$lineStart <- lineStart
      private$lineEnd <- lineEnd
      private$nLines <- lineEnd - lineStart + 1
      private$nArgs <- private$getNArgs()
      private$cycloComp <- private$computeCycloComp()
      return(invisible(self))
    },


    #' @description
    #' Get method to get defined functions in a File object.
    #'
    #' @return (`data.frame()`)\cr
    #' data.frame containing the function name, line start, line end, number
    #' of arguments and cyclomatic complexity.
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
  # Private ----
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
