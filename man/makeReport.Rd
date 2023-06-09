% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeReport.R
\name{makeReport}
\alias{makeReport}
\title{makeReport}
\usage{
makeReport(repo, outputFile, showCode = FALSE)
}
\arguments{
\item{repo}{(\link[PaRe]{Repository})\cr
Repository object.}

\item{outputFile}{(\link[base]{character})\cr
Path to html-file.}

\item{showCode}{(\link[base]{logical}: FALSE)\cr
Logical to show code or not in the report.}
}
\value{
(\code{NULL})
}
\description{
Uses rmarkdown's render function to render a html-report of the given package.
}
\examples{
\donttest{
fetchedRepo <- tryCatch(
  {
    # Set dir to clone repository to.
    tempDir <- tempdir()
    pathToRepo <- file.path(tempDir, "glue")

    # Clone repo
    git2r::clone(
      url = "https://github.com/darwin-eu/IncidencePrevalence.git",
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
  # Run makeReport on the Repository object.
  makeReport(repo = repo, outputFile = tempfile())
}
}
}
