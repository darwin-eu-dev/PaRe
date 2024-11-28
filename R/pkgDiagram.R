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

#' makeGraph
#'
#' Makes the graph
#'
#' @param funsPerDefFun (\link[base]{data.frame})\cr
#' Functions per defined function data.frame.
#' @param pkgName (\link[base]{character})\cr
#' Name of package.
#' @param expFuns (\link[base]{data.frame})\cr
#' Exported functinos data.frame.
#' @param ...
#' Optional other parameters for \link[DiagrammeR]{grViz}.
#'
#' @return (`htmlwidget`)\cr
#' Diagram of the package. See \link[DiagrammeR]{grViz}.
makeGraph <- function(funsPerDefFun, pkgName, expFuns, ...) {
  syntax <- glue::glue("\'{funsPerDefFun$from}\' -> \'{funsPerDefFun$to}\'")

  DiagrammeR::grViz(
    diagram = paste0(
      "digraph {
  graph [layout = dot, rankdir = LR]",
      "subgraph cluster0 {node [style = filled fillcolor = lightgrey] label = <<B>Legend</B>> Exported -> Non_exported}",
      "subgraph cluster1 {node [style = filled fillcolor = lightgrey] Exported [fillcolor = white] label = <<B>", pkgName, "</B>> ",
      paste0(paste0(expFuns, " [fillcolor = white]"), collapse = "\n"),
      paste0(syntax, collapse = "\n"), "}",
      "}",
      collapse = "\n"
    ),
    ...
  )
}

#' getFunsPerDefFun
#'
#' @param files (\link[base]{list})\cr
#' List of \link[PaRe]{File} objects.
#' @param defFuns (\link[base]{data.frame})\cr
#' See \link[PaRe]{getDefinedFunctions}.
#'
#' @return \link[base]{data.frame}
#' | column |              data type |
#' | ------ | ---------------------- |
#' |   from | \link[base]{character} |
#' |     to | \link[base]{character} |
getFunsPerDefFun <- function(files, defFuns) {
  dplyr::bind_rows(lapply(files, function(file) {
    funs <- file$getFunctions()
    dplyr::bind_rows(lapply(funs, function(fun) {
      funCall <- getFunCall(fun = fun, defFuns = defFuns)
      doCall <- getDoCall(fun = fun, defFuns = defFuns)
      applyCall <- getApplyCall(fun = fun, defFuns = defFuns)
      dlplyCall <- getDlplyCall(fun = fun, defFuns = defFuns)
      return(dplyr::bind_rows(
        funCall,
        doCall,
        applyCall,
        dlplyCall
      ))
    }))
  }))
}

#' getExportedFunctions
#'
#' Gets all the exported functions of a package, from NAMESPACE.
#'
#' @param path (\link[base]{character})\cr
#' Path to package
#'
#' @return (\link[base]{c})
#' Vector of \link[base]{character}  exported functions.
getExportedFunctions <- function(path) {
  expFuns <- readLines(glue::glue("{path}/NAMESPACE"))

  expFuns <- unlist(stringr::str_extract_all(
    string = expFuns,
    pattern = "export\\(.+\\)"
  ))

  expFuns <- unlist(stringr::str_extract_all(
    string = expFuns,
    pattern = "\\(\\w+\\)"
  ))

  expFuns <- unlist(stringr::str_extract_all(
    string = expFuns,
    pattern = "\\w+"
  ))

  return(expFuns)
}

#' pkgDiagram
#'
#' Creates a diagram of all defined functions in a package.
#'
#' @export
#'
#' @param repo (\link[PaRe]{Repository})\cr
#' Repository object.
#' @param verbose (\link[base]{logical})\cr
#' Turn verbose messages on or off.
#' @param ...
#' Optional other parameters for \link[DiagrammeR]{grViz}.
#'
#' @return (`htmlwidget`)\cr
#' Diagram `htmlwidget` object. See \link[htmlwidgets]{createWidget}
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
#'   # Run pkgDiagram on the Repository object.
#'   pkgDiagram(repo = repo)
#' }
pkgDiagram <- function(repo, verbose = FALSE, ...) {
  path <- repo$getPath()

  rPath <- file.path(path, "R")

  files <- repo$getRFiles()

  expFuns <- getExportedFunctions(path)

  defFuns <- getDefinedFunctions(repo)

  funsPerDefFun <- getFunsPerDefFun(files = files, defFuns = defFuns)
  # print(funsPerDefFun, n = 300)
  makeGraph(funsPerDefFun, basename(path), expFuns, ...)
}


#' exportDiagram
#'
#' Exports the diagram from `pkgDiagram` to a PDF-file.
#'
#' @export
#'
#' @param diagram (\link[DiagrammeR]{grViz})\cr
#' Graph object from \link[PaRe]{pkgDiagram}.
#' @param fileName (\link[base]{character})\cr
#' Path to save the diagram to, as PDF.
#'
#' @return (`NULL`)
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
#'   # Run pkgDiagram on the Repository object.
#'   pkgDiagram(repo = repo) %>%
#'     # Export the diagram to a temp file.
#'     exportDiagram(fileName = tempfile())
#' }
exportDiagram <- function(diagram, fileName) {
  diagram %>%
    DiagrammeRsvg::export_svg() %>%
    charToRaw() %>%
    rsvg::rsvg_pdf(fileName)
}
