# Copyright 2024 DARWIN EU®
#
# This file is part of PaRe
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' getMultiLineFun
#'
#' @param line (\link[base]{numeric})\cr
#' Current line number.
#' @param lines (\link[base]{c})\cr
#' Vector of (\link[base]{character}) lines.
#'
#' @return (\link[base]{character})
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
#' @param lines (\link[base]{c})\cr
#' Vector of (\link[base]{character}).
#'
#' @return (\link[base]{character})
getDlplyCallFromLines <- function(lines) {
  indices <- grep(pattern = "[plyr::]?dlply", lines)
  lapply(indices, function(index) {
    funCall <- paste0(getMultiLineFun(index, lines), collapse = " ")
    funCall %>%
      stringr::str_remove_all("\\s") %>%
      stringr::str_split_i(pattern = "dlply\\(", i = 2) %>%
      stringr::str_split_i(pattern = ",", i = 3) %>%
      stringr::str_extract(pattern = "[\\=]?\\w+") %>%
      stringr::str_extract(pattern = "\\w+")
  })
}


#' getDlplyCall
#'
#' @param fun (\link[PaRe]{Function})\cr
#' Function object.
#' @param defFuns (\link[base]{data.frame})\cr
#' See \link[PaRe]{getDefinedFunctions}
#'
#' @return (\link[base]{data.frame})
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
#' @param lines (\link[base]{c})\cr
#' Vector of (\link[base]{character}). See \link[PaRe]{getDefinedFunctions}
#'
#' @return (\link[base]{character})
getApplyFromLines <- function(lines) {
  pattern <- "[\\w+]?[Aa]pply(LB)?\\("
  indices <- grep(pattern, lines)
  unlist(lapply(indices, function(index) {
    funCall <- paste0(getMultiLineFun(index, lines), collapse = " ")
    if (!stringr::str_detect(string = funCall, pattern = "function[ ]?\\(")) {
      funCall <- funCall %>%
        stringr::str_remove_all(pattern = "(\\s)")

      pat <- ",(?=[FUN=]?\\w+?\\w+)"

      if (grepl(pattern = "cluster", x = funCall)) {
        pat <- ",(?=[FUN=]?\\w+?\\w+\\))"
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
#' @param fun (\link[PaRe]{Function})\cr
#' Function object.
#' @param defFuns (\link[base]{data.frame})\cr
#' See \link[PaRe]{getDefinedFunctions}
#'
#' @return (\link[base]{data.frame})
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
#' @param lines (\link[base]{c})\cr
#' Vector of (\link[base]{character}). See \link[PaRe]{getDefinedFunctions}
#'
#' @return (\link[base]{character})
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
#' @param fun (\link[PaRe]{Function})\cr
#' Function object.
#' @param defFuns (\link[base]{data.frame})\cr
#' See \link[PaRe]{getDefinedFunctions}
#'
#' @return (\link[base]{data.frame})
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
#' @param fun (\link[PaRe]{Function})\cr
#' Function object.
#' @param defFuns (\link[base]{data.frame})\cr
#' See \link[PaRe]{getDefinedFunctions}.
#'
#' @return (\link[base]{data.frame})
getFunCall <- function(fun, defFuns) {
  lapply(defFuns$name, function(name) {
    indices <- grep(paste0("[^a-zA-Z\\.\\d](::{1,3})?", name, "\\("), fun$getLines())
    if (length(indices) > 0) {
      df <- data.frame(
        from = fun$getName(),
        to = name
      )
      return(df)
    }
  })
}
