% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R6-Function.R
\name{Function}
\alias{Function}
\title{R6 Function class.}
\description{
Class representing a function.
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
  files <- repo$getRFiles()
  file <- files[[1]]
  funs <- file$getFunctions()
  funs[[1]]
}
}
\seealso{
Other Representations: 
\code{\link{Code}},
\code{\link{File}},
\code{\link{Repository}}
}
\concept{Representations}
\section{Super class}{
\code{\link[PaRe:Code]{PaRe::Code}} -> \code{Function}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Function-new}{\code{Function$new()}}
\item \href{#method-Function-getFunction}{\code{Function$getFunction()}}
\item \href{#method-Function-clone}{\code{Function$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="PaRe" data-topic="Code" data-id="getLines"><a href='../../PaRe/html/Code.html#method-Code-getLines'><code>PaRe::Code$getLines()</code></a></span></li>
<li><span class="pkg-link" data-pkg="PaRe" data-topic="Code" data-id="getNLines"><a href='../../PaRe/html/Code.html#method-Code-getNLines'><code>PaRe::Code$getNLines()</code></a></span></li>
<li><span class="pkg-link" data-pkg="PaRe" data-topic="Code" data-id="getName"><a href='../../PaRe/html/Code.html#method-Code-getName'><code>PaRe::Code$getName()</code></a></span></li>
<li><span class="pkg-link" data-pkg="PaRe" data-topic="Code" data-id="print"><a href='../../PaRe/html/Code.html#method-Code-print'><code>PaRe::Code$print()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Function-new"></a>}}
\if{latex}{\out{\hypertarget{method-Function-new}{}}}
\subsection{Method \code{new()}}{
Initializer for Function object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Function$new(name, lineStart, lineEnd, lines)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{(\link[base]{character})\cr
Name of Function.}

\item{\code{lineStart}}{(\link[base]{numeric})\cr
Line number where function starts in File.}

\item{\code{lineEnd}}{(\link[base]{numeric})\cr
Line number where function ends in File.}

\item{\code{lines}}{(\link[base]{c})\cr
Vector of type \link[base]{character} Lines of just the function in File.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{invisible(self)}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Function-getFunction"></a>}}
\if{latex}{\out{\hypertarget{method-Function-getFunction}{}}}
\subsection{Method \code{getFunction()}}{
Get method to get defined functions in a File object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Function$getFunction()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
(\link[base]{data.frame})\tabular{ll}{
   column \tab data type \cr
   name \tab (\link[base]{character}) \cr
   lineStart \tab (\link[base]{integer}) \cr
   lineEnd \tab (\link[base]{numeric}) \cr
   nArgs \tab (\link[base]{integer}) \cr
   cycloComp \tab (\link[base]{integer}) \cr
}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Function-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Function-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Function$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
