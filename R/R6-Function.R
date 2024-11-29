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
#' R6 Function class.
#'
#' @description
#' Class representing a function.
#'
#' @export
#'
#' @include
#' R6-Code.R
#'
#' @family
#' Representations
#'
#' @examples
#' fetchedRepo <- tryCatch(
#'   {
#'     # Set dir to clone repository to.
#'     tempDir <- tempdir()
#'     pathToRepo <- file.path(tempDir, "glue")
#'
#'     # Clone repo
#'     git2r::clone(
#'       url = "https://github.com/tidyverse/glue.git",
#'       local_path = pathToRepo
#'     )
#'
#'     # Create instance of Repository object.
#'     repo <- PaRe::Repository$new(path = pathToRepo)
#'
#'     # Set fetchedRepo to TRUE if all goes well.
#'     TRUE
#'   },
#'   error = function(e) {
#'     # Set fetchedRepo to FALSE if an error is encountered.
#'     FALSE
#'   },
#'   warning = function(w) {
#'     # Set fetchedRepo to FALSE if a warning is encountered.
#'     FALSE
#'   }
#' )
#'
#' if (fetchedRepo) {
#'   files <- repo$getRFiles()
#'   file <- files[[1]]
#'   funs <- file$getFunctions()
#'   funs[[1]]
#' }
Function <- R6::R6Class(
  classname = "Function",
  inherit = Code,
  # Public ----
  public = list(
    #' @description
    #' Initializer for Function object.
    #'
    #' @param name (\link[base]{character})\cr
    #' Name of Function.
    #' @param lineStart (\link[base]{numeric})\cr
    #' Line number where function starts in File.
    #' @param lineEnd (\link[base]{numeric})\cr
    #' Line number where function ends in File.
    #' @param lines (\link[base]{c})\cr
    #' Vector of type \link[base]{character} Lines of just the function in File.
    #'
    #' @return `invisible(self)`
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
    #' @return (\link[base]{data.frame})
    #' |    column |                data type |
    #' | --------- | ------------------------ |
    #' |      name | (\link[base]{character}) |
    #' | lineStart |   (\link[base]{integer}) |
    #' |   lineEnd |   (\link[base]{numeric}) |
    #' |     nArgs |   (\link[base]{integer}) |
    #' | cycloComp |   (\link[base]{integer}) |
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
      nArgs <- getMultiLineFun(line = 1, lines = private$lines) %>%
        stringr::str_replace_all(pattern = "[ ]?=.+", replacement = ",") %>%
        paste0(collapse = "") %>%
        stringr::str_split_i(pattern = "function\\(", 2) %>%
        stringr::str_split_i(pattern = "\\)[ ]?\\{", 1) %>%
        stringr::str_split(pattern = ",") %>%
        unlist() %>%
        length()
    },
    computeCycloComp = function() {
      complexity <- NA
      tryCatch(
        {
          cyclocomp::cyclocomp(eval(parse(text = private$lines)))
        },
        error = function(cond) {
          complexity <- NA
        }
      )
    }
  )
)
