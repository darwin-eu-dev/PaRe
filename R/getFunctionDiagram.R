#' functionUseGraph
#'
#' @param repo
#' <\link[PaRe]{Repository}> Repository object.
#'
#' @return
#' <\link[igraph]{graph}> Graph object crated by \link[igraph]{graph_from_data_frame}.
functionUseGraph <- function(repo) {
  defFuns <- PaRe::getDefinedFunctions(repo)
  rFiles <- repo$getRFiles()
  funsPerDefFun <- getFunsPerDefFun(rFiles, defFuns)
  return(igraph::graph_from_data_frame(funsPerDefFun, directed = TRUE))
}

#' graphToDot
#'
#' @param graph
#' <\link[igraph]{graph}> Graph object crated by \link[igraph]{graph_from_data_frame}.
#'
#' @return
#' `htmlwidgets` Substted diagram. See \link[DiagrammeR]{grViz}.
graphToDot <- function(graph) {
  # Set label because DiagrammeR expects label to be there
  igraph::V(graph)$label <- igraph::V(graph)$name

  tmpFile <- tempfile()
  igraph::write_graph(graph = graph, file = tmpFile, format = "dot")

  dotLines <- readLines(tmpFile)
  dotLines <- stringr::str_replace_all(string = dotLines, pattern = "\\.", "_")
  # Inject Left to Right
  dotLines <- c(
    dotLines[1:2],
    "graph [layout = dot, rankdir = LR]",
    dotLines[-c(1:2)])
  return(DiagrammeR::grViz(dotLines))
}

#' subsetGraph
#'
#' @param repo
#' <\link[PaRe]{Repository}> Repository object.
#' @param functionName
#' <\link[base]{character}> Name of function.
#'
#' @return
#' `htmlwidgets` Substted diagram. See \link[DiagrammeR]{grViz}
#' @export
getFunctionDiagram <- function(repo, functionName) {
  graph <- functionUseGraph(repo = repo)

  pathsIn <- igraph::all_simple_paths(
    graph = graph,
    from = functionName,
    mode = "in")

  pathsOut <- igraph::all_simple_paths(
    graph = graph,
    from = functionName,
    mode = "out")

  paths <- append(pathsIn, pathsOut)

  graphSub <- lapply(paths, igraph::induced_subgraph, graph = graph)
  graphUnion <- do.call(igraph::union, graphSub)
  graphToDot(graphUnion)
}
