#' getMultiLineFun
#'
#' @param line
#' <\link[base]{numeric}> Current line number.
#' @param lines
#' <\link[base]{c}> of <\link[base]{character}> lines.
#'
#' @return
#' <\link[base]{character}>
getMultiLineFun <- function(line, lines) {
  nLine <- line

  # Init
  doCallVec <- c()
  bracOpen <- 0
  bracClose <- 0

  while (bracOpen != bracClose || bracOpen < 1 && bracClose < 1) {
    if (!is.na(lines[nLine])) {
      bracOpen <- bracOpen + stringr::str_count(string = lines[nLine], pattern = "\\(")
      bracClose <- bracClose + stringr::str_count(string = lines[nLine], pattern = "\\)")

      doCallVec <- append(doCallVec, lines[nLine])
    }
    nLine <- nLine + 1

    if (nLine > length(lines)) {
      break
    }
  }
  return(doCallVec)
}


#' getDlplyCallFromLines
#'
#' @param lines
#' <\link[base]{c}> Vector of <\link[base]{character}>.
#'
#' @return
#' <\link[base]{character}>.
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


#' getDlplyCall
#'
#' @param fun
#' <\link[PaRe]{Function}> Function object.
#' @param defFuns
#' <\link[base]{data.frame}>
#'
#' @return
#' <\link[base]{data.frame}>
getDlplyCall <- function(fun, defFuns) {
  dlplyFuns <- getDlplyCallFromLines(fun$getLines())
  if (length(dlplyFuns) > 0) {
    lapply(dlplyFuns, function(dcFun) {
      if (dcFun %in% defFuns$name) {
        data.frame(
          from = fun$getName(),
          to = dcFun
        )
      }
    })
  }
}

#' getApplyFromLines
#'
#' @param lines
#' <\link[base]{c}> Vector of <\link[base]{character}>.
#'
#' @return
#' <\link[base]{character}>.
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

#' getApplyCall
#'
#' @param fun
#' <\link[PaRe]{Function}> Function object.
#' @param defFuns
#' <\link[base]{data.frame}>
#'
#' @return
#' <\link[base]{data.frame}>
getApplyCall <- function(fun, defFuns) {
  applyFuns <- getApplyFromLines(fun$getLines())
  if (length(applyFuns) > 0) {
    lapply(applyFuns, function(dcFun) {
      if (dcFun %in% defFuns$name) {
        data.frame(
          from = fun$getName(),
          to = dcFun
        )
      }
    })
  }
}

#' getDoCallFromLines
#'
#' @param lines
#' <\link[base]{c}> Vector of <\link[base]{character}>.
#'
#' @return
#' <\link[base]{character}>.
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


#' getDoCall
#'
#' @param fun
#' <\link[PaRe]{Function}> Function object.
#' @param defFuns
#' <\link[base]{data.frame}>
#'
#' @return
#' <\link[base]{data.frame}>
getDoCall <- function(fun, defFuns) {
  dcFuns <- getDoCallFromLines(fun$getLines())
  if (length(dcFuns) > 0) {
    lapply(dcFuns, function(dcFun) {
      if (dcFun %in% defFuns$name) {
        data.frame(
          from = fun$getName(),
          to = dcFun
        )
      }
    })
  }
}

#' getFunCall
#'
#' @param fun
#' <\link[PaRe]{Function}> Function object.
#' @param defFuns
#' <\link[base]{data.frame}>
#'
#' @return
#' <\link[base]{data.frame}>
getFunCall <- function(fun, defFuns) {
  lapply(defFuns$name, function(name) {
    indices <- grep(paste0("[^a-zA-Z\\.\\d]", name, "\\("), fun$getLines())
    if (length(indices) > 0) {
      df <- data.frame(
        from = fun$getName(),
        to = name
      )
      return(df)
    }
  })
}
