#' forward
#'
#' @param c
#' <\link[base]{character}> Formatted function name.
#' @param nodes
#' <\link[base]{c}> Vector of all individual paths.
#'
#' @return
#' <\link[base]{vector}> Vector of nodes forward in the graph.
forward <- function(c, nodes) {
  unlist(lapply(c, function(node) {
    start <- nodes[startsWith(x = nodes, prefix = node)]
    stringr::str_split_i(string = start, pattern = " -> ", i = 2)
  }))
}

#' backward
#'
#' @param c
#' <\link[base]{character}> Formatted function name.
#' @param nodes
#' <\link[base]{c}> Vector of all individual paths.
#'
#' @return
#' <\link[base]{vector}> Vector of nodes forward in the graph.
backward <- function(c, nodes) {
  unlist(lapply(c, function(node) {
    start <- nodes[endsWith(x = nodes, suffix = c)]
    stringr::str_split_i(string = start, pattern = " -> ", i = 1)
  }))
}

#' recForward
#'
#' @param c
#' <\link[base]{character}> Formatted function name.
#' @param nodes
#' <\link[base]{c}> Vector of all individual paths.
#' @param n
#' <\link[base]{numeric}> Internal counter of n iterations
#' @param maxIter
#' <\link[base]{numeric}> Maximum iterations specified by user.
#'
#' @return
#' <\link[base]{character}>
recForward <- function(c, nodes, n, maxIter) {
  n <- n + 1
  f <- forward(c, nodes)
  if (n < maxIter) {
    return(unlist(lapply(f, recForward, nodes = nodes, n = n, maxIter = maxIter)))
  } else {
    return(paste(c, "->", f))
  }
}

#' recBackward
#'
#' @param c
#' <\link[base]{character}> Formatted function name.
#' @param nodes
#' <\link[base]{c}> Vector of all individual paths.
#' @param n
#' <\link[base]{numeric}> Internal counter of n iterations
#' @param maxIter
#' <\link[base]{numeric}> Maximum iterations specified by user.
#'
#' @return
#' <\link[base]{character}>
recBackward <- function(c, nodes, n, maxIter) {
  n <- n + 1
  f <- backward(c, nodes)
  if (n < maxIter) {
    unlist(lapply(f, recBackward, nodes = nodes, n = n, maxIter = maxIter))
  } else {
    return(paste(f, "->", c))
  }
}


#' funDiagram
#'
#' @param funName
#' <\link[base]{character}> Name of function to get path of.
#' @param nodes
#' <\link[base]{c}> Vector of all individual paths.
#' @param n
#' <\link[base]{numeric}> (0) Internal counter of n iterations
#' @param maxIter
#' <\link[base]{numeric}> Maximum iterations specified by user.
#'
#' @return
#' Diagram `htmlwidget` object. See \link[htmlwidgets]{createWidget}
funDiagram <- function(funName, nodes, maxIter, n = 0) {
  subFor <- recForward(c = funName, nodes = nodes, n = n, maxIter = maxIter)
  subBack <- recBackward(c = funName, nodes = nodes, n = n, maxIter = maxIter)

  paths <- c(subFor, subBack)
  paths <- unique(paths)
  paths <- paths[!endsWith(paths, "-> ")]
  paths <- paste0(paths, collapse = "\n")

  DiagrammeR::grViz(
    paste0(
      "digraph {graph [layout = dot, rankdir = LR]",
      paths,
      "}"))
}

#' getFunctionDiagram
#'
#' @param repo
#' <\link[PaRe]{Repository}> object.
#' @param funName
#' <\link[base]{character}> Name of function to get path of.
#' @param maxIter
#' <\link[base]{numeric}> (10) Maximum iterations specified by user.
#'
#' @return
#' Diagram `htmlwidget` object. See \link[htmlwidgets]{createWidget}
#'
#' @export
getFunctionDiagram <- function(repo, funName, maxIter = 10) {
  files <- repo$getRFiles()
  allFuns <- PaRe::getFunctionUse(repo)

  funsPerDefFun <- PaRe:::getFunsPerDefFun(files = files, allFuns = allFuns)

  nodes <- unique(unlist(lapply(seq_len(nrow(funsPerDefFun)), function(i) {
    glue::glue("'{funsPerDefFun[i, ]$name}' -> '{funsPerDefFun[i, ]$fun}'")
  })))

  funDiagram(funName = glue::glue("\'{funName}\'"), nodes = nodes, maxIter = maxIter, n = 0)
}
