#' makeGraph
#'
#' Makes the graph
#'
#' @param funsPerDefFun
#' <\link[base]{data.frame}> Functions per defined function data.frame.
#' @param pkgName
#' <\link[base]{character}> Name of package.
#' @param expFuns
#' <\link[base]{data.frame}> Exported functinos data.frame.
#' @param ...
#'   Optional other parameters for \link[DiagrammeR]{grViz}.
#'
#' @return
#'   <`htmlwidget`> Diagram of the package. See \link[DiagrammeR]{grViz}.
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
      collapse = "\n"),
    ...)
}

#' Title
#'
#' @param files
#' <\link[base]{list}> of <\link[PaRe]{File}> objects.
#' @param defFuns
#' <\link[base]{data.frame}>
#'
#' @return
#' <\link[base]{data.frame}>
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
#' @param path
#' <\link[base]{character}> Path to package
#'
#' @return
#' <\link[base]{c}> of <\link[base]{character}> vector of exported functions
getExportedFunctions <- function(path) {
  expFuns <- readLines(glue::glue("{path}/NAMESPACE"))

  expFuns <- unlist(stringr::str_extract_all(
    string = expFuns,
    pattern = "export\\(.+\\)"))

  expFuns <- unlist(stringr::str_extract_all(
    string = expFuns,
    pattern = "\\(\\w+\\)"))

  expFuns <- unlist(stringr::str_extract_all(
    string = expFuns,
    pattern = "\\w+"))

  return(expFuns)
}

#' pkgDiagram
#'
#' Creates a diagram of all defined functions in a package.
#'
#' @param repo
#' <\link[PaRe]{Repository}> object.
#' @param verbose
#'   <\link[base]{logical}> Verbose messages
#' @param ...
#'   Optional other parameters for \link[DiagrammeR]{grViz}.
#'
#' @return `htmlwidget`
#'   Diagram `htmlwidget` object. See \link[htmlwidgets]{createWidget}
#' @export
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
#' @param diagram
#' <\link[DiagrammeR]{grViz}> Graph object from the `pkgDiagram` function.
#' @param fileName
#' <\link[base]{character}> Path to file, where to save the diagram to.
#'
#' @return
#' `NULL`
#' @export
exportDiagram <- function(diagram, fileName) {
  diagram %>%
    DiagrammeRsvg::export_svg() %>%
    charToRaw() %>%
    rsvg::rsvg_pdf(fileName)
}
