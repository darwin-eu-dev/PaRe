# Copyright 2024 DARWIN EU®
#
# This file is part of PaRe
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' @title
#' R6 Code class
#'
#' @description
#' Class representing a piece of code.
#'
#' @family
#' Representations
Code <- R6::R6Class(
  classname = "Code",
  # Public ----
  public = list(
    #' @description
    #' Initializer method
    #'
    #' @param name (`character(1)`)\cr
    #' Name of Code object.
    #' @param lines (`character(n)`)\cr
    #' Vector of lines Code object.
    #'
    #' @return `invisible(self)`
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
    #' further arguments passed to or from other methods. See \link[base]{print}.
    #'
    #' @return (`character(n)`)
    print = function(...) {
      classTypes <- class(self)
      classTypes <- paste0(glue::glue("<{classTypes}>"), collapse = " ")

      cat(
        glue::glue(
          "{classTypes}
        Name: {private$name}
        # Lines: {private$nLines}
        "
        )
      )
    },

    #' @description
    #' Get method for lines.
    #'
    #' @return (`character(n)`) Vector of lines in the Code object.
    getLines = function() {
      return(private$lines)
    },

    #' @description
    #' Get method for number of lines.
    #'
    #' @return (`numeric(1)`) Number of lines in the Code object.
    getNLines = function() {
      return(private$nLines)
    },

    #' @description
    #' Get method for Name.
    #'
    #' @return (`character(1)`) Name of the Code object.
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
