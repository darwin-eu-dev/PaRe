#' getMultiLineFun
#'
#' @param line Current line number
#' @param lines All lines
#'
#' @return Returns vector of functions found in do.call function call.
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

#' getDlply
#'
#' @param line All lines
#' @param lines Current line number
#'
#' @return Returns function in plyr::dlply function call.
getDlply <- function(line, lines) {
  funVec <- paste0(getMultiLineFun(line, lines), collapse = "")

  fun <- unlist(stringr::str_remove_all(string = funVec, pattern = "\\s"))
  fun <- unlist(stringr::str_split(fun, "dlply"))[2]
  fun <- unlist(stringr::str_split(fun, ","))[4]
  fun <- unlist(stringr::str_extract(fun, "\\=\\w+"))
  fun <- unlist(stringr::str_extract(fun, "\\w+"))

  return(fun)
}


#' getApplyFun
#'
#' @param line All lines
#' @param lines Current line number
#'
#' @return Returns function in (vsl)apply function call.
getApplyFun <- function(line, lines) {
  applyVec <- getMultiLineFun(line, lines)

  applyFun <- unlist(stringr::str_split(
    string = paste0(applyVec, collapse = ""),
    pattern = "[\\w]+?[Aa]pply"))[2]

  applyFun <- applyFun[!stringr::str_detect(string = applyFun, pattern = "function[ ]?\\(")]
  applyFun <- unlist(stringr::str_remove_all(string = applyFun, pattern = "\\s"))
  applyFun <- unlist(stringr::str_remove_all(string = applyFun, pattern = ",\\w+=\\w+"))
  applyFun <- unlist(stringr::str_extract_all(string = applyFun, pattern = "[\\w\\.]+(::)?[\\w\\.]+\\)"))
  applyFun <- stringr::str_extract_all(string = applyFun, pattern = "[\\w\\.]+(::)?[\\w\\.]+")
  return(applyFun)
}


#' getDoCallFun
#'
#' @param line Current line number
#' @param lines All lines
#'
#' @return Returns function used in do.call function call.
getDoCallFun <- function(line, lines) {
  doCallVec <- getMultiLineFun(line, lines)

  doCallFun <- unlist(stringr::str_split(
    string = paste0(doCallVec, collapse = ""),
    pattern = "do\\.call"))[2]

  fun <- unlist(stringr::str_remove_all(string = doCallFun, pattern = "\\s"))
  fun <- unlist(stringr::str_extract_all(string = fun, pattern = "\\([\\w\\.]+(::)?[\\w\\.]+"))
  fun <- stringr::str_extract_all(string = fun, pattern = "[\\w\\.]+(::)?[\\w\\.]+")
  return(fun)
}


#' funsUsedInLine
#'
#' Support function for funsUsedInFile.
#'
#' @param file_txt file to use
#' @param file_name name of file
#' @param i line
#' @param verbose Prints message when no function found
#'
#' @return data.frame of 3 colums: Package (pkg); Function (fun); Line in
#' script (line)
funsUsedInLine <- function(lines, name, i, verbose = FALSE) {
  line <- lines[i]

  if (!startsWith(line, "#") && !is.na(line) && length(line) >= 0 && line != "NA") {
    line <- paste(
      stringr::str_split(
        string = line,
        pattern = "\\w+\\$",
        simplify = TRUE
      ),
      collapse = ""
    )

    # Remove strings
    line <- stringr::str_replace_all(line, "[\"\'\`].+[\"\'\`]+", "")

    funVec <- unlist(stringr::str_extract_all(
      string = line,
      pattern = "[\\w\\.]+(::)?[\\w\\.]+\\("
    ))

    funVec <- stringr::str_remove_all(
      string = funVec,
      pattern = "\\("
    )

    if ("do.call" %in% funVec) {
      funVec <- append(funVec, getDoCallFun(i, lines))
    }

    if (any(stringr::str_detect(string = funVec, pattern = "[\\w]+?[Aa]pply"))) {
      funVec <- append(funVec, getApplyFun(i, lines))
    }

    if ("plyr::dlply" %in% funVec) {
      funVec <- append(funVec, getDlply(i, lines))
    }

    funVec <- stringr::str_split(
      string = funVec,
      pattern = "::"
    )

    if (length(funVec) > 0) {
      funVec <- lapply(
        X = funVec,
        FUN = function(x) {
          if (length(x) == 1) {
            x <- list("unknown", x)
          } else {
            list(x)
          }
        }
      )

      df <- data.frame(t(sapply(funVec, unlist)))
      names(df) <- c("pkg", "fun")

      df <- df %>%
        dplyr::mutate(
          file = name,
        line = i) %>%
        dplyr::tibble()

      return(df)
    } else {
      if(verbose == TRUE) {
        message(paste0("No functions found for line: ", i))
      }
    }
  }
}


#' funsUsedInFile
#'
#' Support function
#'
#' @param files Files to get functions from
#' @param verbose Verbosity
#'
#' @return table
funsUsedInFile <- function(files, verbose = FALSE) {
  lapply(X = files, FUN = function(file) {
    if (verbose) {
      message(paste0("Started on file: ", file$getName()))
    }

    lines <- file$getLines()

    out <- lapply(
      X = seq_len(length(lines)),
      FUN = function(i) {
        funsUsedInLine(lines = file$getLines(), name = file$getName(), i = i)
      }
    )
  })
}

#' summariseFunctionUse
#'
#' Summarise functions used in R package
#'
#' @param r_files Complete path(s) to files to be investigated
#' @param verbose Default: FALSE; prints message to console which file is
#' currently being worked on.
#'
#' @return tibble
#'
#' @export
getFunctionUse <- function(repo, verbose = FALSE) {
  files <- repo$getFiles()

  funUse <- funsUsedInFile(files, verbose)

  if (length(funUse) == 0) {
    warning("No functions found, output will be empty")
    funUse <- dplyr::tibble(
      file = character(0),
      line = numeric(0),
      pkg = character(0),
      fun = character(0)
    )
  }

  funUse <- dplyr::bind_rows(funUse) %>%
    dplyr::relocate("file", "line", "pkg", "fun") %>%
    dplyr::arrange(.data$file, .data$line, .data$pkg, .data$fun)

  funUse$pkg[funUse$fun %in% ls("package:base")] <- "base"
  return(funUse)
}
