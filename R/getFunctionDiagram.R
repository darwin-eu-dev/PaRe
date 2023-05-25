#' functionUseGraph
#'
#' @param repo (\link[PaRe]{Repository})\cr
#'
#' @return (\link[igraph]{graph})\cr
functionUseGraph <- function(repo) {
  defFuns <- PaRe::getDefinedFunctions(repo)
  rFiles <- repo$getRFiles()
  funsPerDefFun <- getFunsPerDefFun(rFiles, defFuns)
  return(igraph::graph_from_data_frame(funsPerDefFun, directed = TRUE))
}

#' graphToDot
#'
#' @param graph (\link[igraph]{graph})\cr
#'
#' @return `htmlwidgets`\cr
#' See \link[DiagrammeR]{grViz}.
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
    dotLines[-c(1:2)]
  )
  return(DiagrammeR::grViz(dotLines))
}

#' subsetGraph
#'
#' Create a subset of the package diagram containing all in comming and out
#' going paths from a specified function.
#'
#' @export
#'
#' @param repo (\link[PaRe]{Repository})
#' Repository object.
#' @param functionName (\link[base]{character})
#' Name of the function to get all paths from.
#'
#' @return (`htmlwidgets`)
#' Substted diagram. See \link[DiagrammeR]{grViz}
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
#'   # Run getFunctionDiagram on the Repository object.
#'   getFunctionDiagram(repo = repo, functionName = "glue")
#' }
getFunctionDiagram <- function(repo, functionName) {
  graph <- functionUseGraph(repo = repo)

  pathsIn <- igraph::all_simple_paths(
    graph = graph,
    from = functionName,
    mode = "in"
  )

  pathsOut <- igraph::all_simple_paths(
    graph = graph,
    from = functionName,
    mode = "out"
  )

  paths <- append(pathsIn, pathsOut)

  graphSub <- lapply(paths, igraph::induced_subgraph, graph = graph)
  graphUnion <- do.call(igraph::union, graphSub)
  graphToDot(graphUnion)
}
