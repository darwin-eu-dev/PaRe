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
  graphSyntx <- unique(unlist(lapply(seq_len(nrow(funsPerDefFun)), function(i) {
    glue::glue("'{funsPerDefFun[i, ]$name}' -> '{funsPerDefFun[i, ]$fun}'")
  })))

  DiagrammeR::grViz(
    diagram = paste0(
      "digraph {
  graph [layout = dot, rankdir = LR]",
      "subgraph cluster0 {node [style = filled fillcolor = lightgrey] label = <<B>Legend</B>> Exported -> Non_exported}",
      "subgraph cluster1 {node [style = filled fillcolor = lightgrey] Exported [fillcolor = white] label = <<B>", pkgName, "</B>> ",
      paste0(paste0(expFuns, " [fillcolor = white]"), collapse = "\n"),
      paste0(graphSyntx, collapse = "\n"), "}",
      "}",
      collapse = "\n"),
    ...)
}

#' getFunsPerDefFun
#'
#' Gets all function calls per defined function in the package.
#'
#' @param files (`list()`)
#' <\link[base]{list}> of <\link[PaRe]{File}> objects to investigate.
#' @param allFuns
#' <\link[base]{data.frame}> of allFunctions.
#' @param verbose
#' <\link[base]{logical}> to turn verbose messaging on/off.
#'
#' @return
#' <\link[base]{data.frame}> of all functions per defined function of package.
getFunsPerDefFun <- function(files, allFuns, verbose) {
  dplyr::bind_rows(lapply(files, function(file) {

    defFuns <- file$getFunctionTable()
    if (!is.null(nrow(defFuns))) {
      dplyr::bind_rows(lapply(seq_len(nrow(defFuns)), function(i) {
        x <- allFuns %>%
          dplyr::filter(.data$fun %in% defFuns$name) %>%
          dplyr::filter(
            .data$line >= defFuns$lineStart[i] &
              .data$line <= defFuns$lineEnd[i]) %>%
          dplyr::mutate(name = defFuns$name[i])
        return(x)
      }))}
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

  allFuns <- PaRe::getFunctionUse(repo)

  funsPerDefFun <- getFunsPerDefFun(files = files, allFuns = allFuns)

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
