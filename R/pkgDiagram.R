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
  DiagrammeR::grViz(
    diagram = paste0(
      "digraph {
  graph [layout = dot, rankdir = LR]",
      "subgraph cluster0 {node [style = filled fillcolor = lightgrey] label = <<B>Legend</B>> Exported -> Non_exported}",
      "subgraph cluster1 {node [style = filled fillcolor = lightgrey] Exported [fillcolor = white] label = <<B>", pkgName, "</B>> ",
      paste0(paste0(expFuns, " [fillcolor = white]"), collapse = "\n"),
      paste0(funsPerDefFun, collapse = "\n"), "}",
      "}",
      collapse = "\n"),
    ...)
}

getDlplyCallFromLines <- function(lines) {
  indices <- grep(pattern = "[plyr::]?dlply", lines)
  lapply(indices, function(index) {
    funCall <- paste0(getMultiLineFun(index, lines), collapse = " ")
    funCall %>%
      stringr::str_remove_all("\\s") %>%
      stringr::str_split_i(pattern = "dlply\\(", i = 2) %>%
      stringr::str_split_i(pattern = ",", i = 4) %>%
      stringr::str_extract(pattern = "\\=\\w+") %>%
      stringr::str_extract(pattern = "\\w+")
  })
}


getDlplyCall <- function(fun, defFuns) {
  dlplyFuns <- getDlplyCallFromLines(fun$getLines())
  if (length(dlplyFuns) > 0) {
    lapply(dlplyFuns, function(dcFun) {
      if (dcFun %in% defFuns$name) {
        glue::glue("\'{fun$getName()}\' -> \'{dcFun}\' [color = '#00000020']")
      }
    })
  }
}

getApplyFromLines <- function(lines) {
  pattern <- "[\\w+]?[Aa]pply\\("
  indices <- grep(pattern, lines)
  unlist(lapply(indices, function(index) {
    funCall <- paste0(getMultiLineFun(index, lines), collapse = " ")
    if (!stringr::str_detect(string = funCall, pattern = "function[ ]?\\(")) {
      funCall <- funCall %>%
        stringr::str_remove_all(pattern = "(\\s)")

      if (grepl(pattern = "cluster", x = funCall)) {
        pat <- ",(?=[FUN=]?\\w+?\\w+\\))"
      } else {
        pat <- ",(?=[FUN=]?\\w+?\\w+)"
      }

      funCall <- funCall %>%
        stringr::str_split_i(pattern = pat, i = 2)

      if (grepl(pattern = "=", x = funCall)) {
        funCall <- funCall %>%
          stringr::str_split_i(pattern = "=", i = 2)
      }

      funCall <- funCall %>%
        stringr::str_remove_all(pattern = "[\\%\\(\\)\\\\>\\<]")
      return(funCall)
    }
  }))
}

getApplyCall <- function(fun, defFuns) {
  applyFuns <- getApplyFromLines(fun$getLines())
  if (length(applyFuns) > 0) {
    lapply(applyFuns, function(dcFun) {
      if (dcFun %in% defFuns$name) {
        glue::glue("\'{fun$getName()}\' -> \'{dcFun}\' [color = '#00000020']")
      }
    })
  }
}

getDoCallFromLines <- function(lines) {
  pattern <- "do\\.call\\("
  indices <- grep(pattern, lines)

  unlist(lapply(indices, function(index) {
    funCall <- paste0(getMultiLineFun(index, lines), collapse = " ")

    funCall <- funCall %>%
      stringr::str_remove_all(pattern = "\\s") %>%
      stringr::str_split_i(pattern = pattern, i = 2) %>%
      stringr::str_split_i(pattern = ",", i = 1) %>%
      stringr::str_remove_all(pattern = "[\"\'\\\\]")

    if (grepl("=", funCall)) {
      funCall <- funCall %>%
        stringr::str_split_i(pattern = "=", i = 2)
    }
    return(funCall)
  }))
}

getDoCall <- function(fun, defFuns) {
  dcFuns <- getDoCallFromLines(fun$getLines())
  if (length(dcFuns) > 0) {
    lapply(dcFuns, function(dcFun) {
      if (dcFun %in% defFuns$name) {
        glue::glue("\'{fun$getName()}\' -> \'{dcFun}\' [color = '#00000020']")
      }
    })
  }
}

getFunCall <- function(fun, defFuns) {
  lapply(defFuns$name, function(name) {
    indices <- grep(paste0(name, "\\("), fun$getLines())
    if (length(indices) > 0) {
      return(glue::glue("\'{fun$getName()}\' -> \'{name}\' [color = '#00000020']"))
    }
  })
}

getFunsPerDefFun <- function(files, defFuns) {
  unique(unlist(lapply(files, function(file) {
    funs <- file$getFunctions()
    lapply(funs, function(fun) {

      funCall <- getFunCall(fun = fun, defFuns = defFuns)
      doCall <- getDoCall(fun = fun, defFuns = defFuns)
      applyCall <- getApplyCall(fun = fun, defFuns = defFuns)
      dlplyCall <- getDlplyCall(fun = fun, defFuns = defFuns)
      return(c(
        funCall,
        doCall,
        applyCall,
        dlplyCall
      ))
    })
  })))
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
