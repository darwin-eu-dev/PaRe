#' @title
#' R6 Code class
#' @description
#' Class representing a piece of code.
Code <- R6::R6Class(
  classname = "Code",
  # Public ----
  public = list(
    #' @description
    #' Initializer method
    #'
    #' @param name
    #' <character> Name of Code object.
    #' @param lines
    #' <character> Vector of lines Code object.
    #'
    #' @return invisible(self)
    initialize = function(name, lines) {
      private$name <- name
      private$lines <- lines
      private$nLines <- length(lines)
      return(invisible(self))
    },

    #' @description
    #' Overload generic print, to print Code object.
    #'
    #' @param ...
    #' further arguments passed to or from other methods.
    #'
    #' @return (`character()`)
    print = function(...) {
      classTypes <- class(self)
      classTypes <- paste0(glue::glue("<{classTypes}>"), collapse = " ")

      cat(
        glue::glue(
        "{classTypes}
        Name: {private$name}
        # Lines: {private$nLines}
        ")
      )
    },

    #' @description
    #' Get method for lines.
    #'
    #' @return
    #' <character> Vector of lines in the Code object.
    getLines = function() {
      return(private$lines)
    },

    #' @description
    #' Get method for number of lines.
    #'
    #' @return
    #' <numeric> Number of lines in the Code object.
    getNLines = function() {
      return(private$nLines)
    },

    #' @description
    #' Get method for Name.
    #'
    #' @return
    #' <character> name of the Code object.
    getName = function() {
      return(private$name)
    }
  ),
  # Private ----
  private = list(
    name = "",
    lines = c(),
    nLines = 0
  )
)
