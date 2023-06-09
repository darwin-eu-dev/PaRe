% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getFunctionDiagram.R
\name{getFunctionDiagram}
\alias{getFunctionDiagram}
\title{subsetGraph}
\usage{
getFunctionDiagram(repo, functionName)
}
\arguments{
\item{repo}{(\link[PaRe]{Repository})
Repository object.}

\item{functionName}{(\link[base]{character})
Name of the function to get all paths from.}
}
\value{
(\code{htmlwidgets})\cr
Subsetted diagram. See \link[DiagrammeR]{grViz}
}
\description{
Create a subset of the package diagram containing all in comming and out
going paths from a specified function.
}
\examples{
fetchedRepo <- tryCatch(
  {
    # Set dir to clone repository to.
    tempDir <- tempdir()
    pathToRepo <- file.path(tempDir, "glue")

    # Clone repo
    git2r::clone(
      url = "https://github.com/tidyverse/glue.git",
      local_path = pathToRepo
    )

    # Create instance of Repository object.
    repo <- PaRe::Repository$new(path = pathToRepo)

    # Set fetchedRepo to TRUE if all goes well.
    TRUE
  },
  error = function(e) {
    # Set fetchedRepo to FALSE if an error is encountered.
    FALSE
  },
  warning = function(w) {
    # Set fetchedRepo to FALSE if a warning is encountered.
    FALSE
  }
)

if (fetchedRepo) {
  # Run getFunctionDiagram on the Repository object.
  getFunctionDiagram(repo = repo, functionName = "glue")
}
}
