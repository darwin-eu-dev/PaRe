#' getHalstead
#'
#' Halstead complexity measures are measureable properties of a piece of code.
#' \cr\cr
#' Number of Operators: \eqn{N_1}\cr
#' Number of Operands: \eqn{N_2}\cr
#' Total number of Operators: \eqn{\eta_1}\cr
#' Total number of Operands: \eqn{\eta_2}
#' \cr\cr
#' Program Vocabulary (vocabulary):\cr
#' \eqn{V = \eta_1 + \eta_2}
#' \cr\cr
#' Program Length (progLength):\cr
#' \eqn{N = N_1 + N_2}
#' \cr\cr
#' Calculated estimated program length  (calEstProgLength):\cr
#' \eqn{\hat{N} = \eta_1 \times log_2\ \eta_1 + \eta_2 \times log_2\ \eta_2}
#' \cr\cr
#' Volume (volume):\cr
#' \eqn{V = N \times log_2\ \eta}
#' \cr\cr
#' Difficulty (difficulty):\cr
#' \eqn{D = \frac{\eta_1}{2} \times \frac{N_2}{\eta_2}}
#' \cr\cr
#' Effort (effort):\cr
#' \eqn{E = D \times V}
#' \cr\cr
#' \href{`https://www.researchgate.net/publication/235978157_Elements_Of_Software_Science`}{Elements Of Software Science, Halstead M.H (1977)}
#' \cr
#' \href{`https://en.wikipedia.org/wiki/Halstead_complexity_measures`}{Wiki}
#'
#' @param fun \link[PaRe]{Function}\cr
#' Function object.
#'
#' @return \link[base]{data.frame}
#' Data frame with the following columns:
#'
#' | column            | data type             |
#' | ----------------- | --------------------- |
#' | funName           | \link[base]{character}|
#' | vocabulary        | \link[base]{integer}  |
#' | progLength        | \link[base]{integer}  |
#' | callEstProgLength | \link[base]{numeric}  |
#' | volume            | \link[base]{numeric}  |
#' | difficulty        | \link[base]{numeric}  |
#' | effort            | \link[base]{numeric}  |
#'
#' @export
getHalstead <- function(fun) {
  mergedStrings <- paste0(fun$getLines(), collapse = "\n")

  tokens <- sourcetools::tokenize_string(mergedStrings) %>%
    dplyr::filter(!type %in% c("comment", "whitespace"))

  operators <- tokens %>%
    filter(type %in% c("keyword", "bracket", "comma"))

  operands <- tokens %>%
    filter(type %in% c("symbol","number","string"))

  eta1 <- length(unique(operators$type))
  eta2 <- length(unique(operands$type))

  n1 <- nrow(operators)
  n2 <- nrow(operands)

  eta <- eta1 + eta2
  n <- n1 + n2
  nHat <- n1 * log2(n1) + n2 * log2(n2)
  v <- n / log2(eta)
  d <- eta1 / 2 * n2 / eta2
  e <- d * v

  return(
    data.frame(
      funName = fun$getName(),
      vocabulary = eta,
      progLength = n,
      callEstProgLength = nHat,
      volume = v,
      difficulty = d,
      effort = e
    )
  )
}
