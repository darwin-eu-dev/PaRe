#' maintainabilityIndex
#'
#' Computes the maintainability index accourding to:\cr
#' \eqn{max(0, \frac{171 - 5.2 \times ln\ V - 0.23 \times C - 16.2 \times ln\ L \times 100}{171})}
#' \cr\cr
#' Where:\cr
#' \eqn{C}: Cyclomatic Complexity\cr
#' \eqn{V}: Halstead Volume\cr
#' \eqn{L}: Lines of Code
#' \cr\cr
#' \href{https://learn.microsoft.com/en-us/visualstudio/code-quality/code-metrics-maintainability-index-range-and-meaning?view=vs-2022}{Code metrics - Maintainability index range and meaning, microsoft.com (04-30-2022)}
#' describes the following ranges:\cr
#' 0-9: Red\cr
#' 10-19: Yellow\cr
#' 20-100: Green
#'
#' @param fun \link[PaRe]{Function}\cr
#' Function object.
#'
#' @return \link[base]{numeric}
#' @export
maintainabilityIndex <- function(fun) {
  halsteadStats <- getHalstead(fun)
  funStats <- fun$getFunction()
  return(
    max(
      0,
      (171 -
         5.2 * log(halsteadStats$volume) -
         0.23 * (funStats$cycloComp) -
         16.2 * log(funStats$lineEnd - funStats$lineStart)
      )
    )
  )
}
