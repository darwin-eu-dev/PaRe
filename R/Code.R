#' @description
#' R6 <Code> class representing a piece of code.
Code <- R6::R6Class(
  classname = "Code",
  public = list(
    initialize = function(name, lines) {
      private$name <- name
      private$lines <- lines
      private$nLines <- length(lines)
      private$type <- stringr::str_split_i(string = private$name, pattern = "\\.", i = 2)
      private$comment <- private$commentSwitch()
      return(invisible(self))
    },

    print = function(...) {
      classTypes <- class(self)
      classTypes <- paste0(glue::glue("<{classTypes}>"), collapse = " ")

      cat(
        glue::glue(
        "{classTypes}
        Name: {private$name}
        # Lines: {private$nLines}
        Code type: {private$type}
        Comment symbols: {private$comment}"
        )
      )
    },

    getLines = function() {
      return(private$lines)
    },

    getNLines = function() {
      return(private$nLines)
    },

    getName = function() {
      return(private$name)
    }
  ),
  private = list(
    name = "",
    lines = c(),
    nLines = 0,
    type = "",
    comment = c(),

    commentSwitch = function() {
      return(
        switch(
          EXPR = private$type,
          R = c("#"),
          cpp = c("//"),
          java = c("//"),
          sql = c("#")
        )
      )
    }
  )
)
