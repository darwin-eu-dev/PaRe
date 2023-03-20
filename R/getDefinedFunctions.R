#' getDefinedFunctionsPkg
#'
#' Gets defined functions of the package
#'
#' @param path Path to package
#' @param verbose Verbose messages
#'
#' @return tibble
#' @export
getDefinedFunctionsPkg <- function(path, verbose = FALSE) {
  if (stringr::str_ends(path, "\\w")) {
    path <- paste0(path, "/")
  }
  dplyr::bind_rows(lapply(
    list.files(paste0(path, "R"), full.names = TRUE, recursive = TRUE),
    getDefinedFunctionsFile,
    verbose = verbose))
}

#' getDefinedFunctionsFile
#'
#' Gets all the defined functions in a file, stored in a a tibble, with the
#' following columns: file (filename), start (start line in file), size (Amount
#' of lines occupied by function), fun (function name).
#'
#' @param filePath File path to the R-file to be investigated
#' @param verbose Prints message as to what file is currently being worked on.
#' Usefull if used in an apply funciton, investigating alot of different files.
#'
#' @return Returns a tibble object.
#' @export
#'
#' @examples
#' filePath <- system.file(package = "PaRe", "testScript.R")
#' df <- getDefinedFunctionsFile(filePath)
getDefinedFunctionsFile <- function(filePath, verbose = FALSE) {
  # Read lines
  lines <- readLines(filePath, warn = FALSE)

  if (verbose) {
    message(glue::glue("working on file: {basename(filePath)}"))
  }

  # Get defined functions
  constructorIndices <- grep(
    pattern = "\\w+[ ]?<\\-[ ]?function\\(",
    x = paste0(lines))

  funsRaw <- lines[constructorIndices]
  funNames <- stringr::str_extract(string = funsRaw, pattern = "[\\w\\d\\.]+")

  # Per function, get indices of body
  dplyr::bind_rows(lapply(
    X = seq_len(length(funNames)),
    FUN = function(i) {
      df <- getBodyIndices(constructorIndices[i], lines)
      df["fun"] <- funNames[i]
      df["size"] <- df["end"] - df["start"]
      df["file"] <- tail(unlist(stringr::str_split(filePath, "/")), 1)
      df <- df %>% dplyr::select(.data$file, .data$start, .data$size, .data$fun, .data$nArgs, .data$cycloComp)
      return(df)
    }))
}

#' getBodyIndices
#'
#' Helper function for getDefinedFunctions, retrieves offset indeces for where
#' the body of the function starts.
#'
#' @param line Line index of the function constructor.
#' @param lines All lines of the R-file to be investigated.
#'
#' @return Returns a data.frame with the start and end indices of lines.
getBodyIndices <- function(line, lines) {
  # Parameters
  switchOff <- FALSE

  # Get start of body
  startFunLine <- goToBody(line, lines)

  mergedConstructor <- paste0(lines[line:startFunLine], collapse = "")
  args <- unlist(stringr::str_remove_all(mergedConstructor, "\\s"))
  args <- unlist(stringr::str_split(args, "function\\("))[2]
  args <- unlist(stringr::str_split(args, "\\)\\{"))[1]
  args <- unlist(stringr::str_remove_all(args, "\\w+\\(.+\\)"))
  nArgs <- length(unlist(stringr::str_split(args, ",")))

  endFunLine <- startFunLine
  cntOpen <- 0
  cntClosed <- 0

  while (switchOff == FALSE) {
    checkOpen <- stringr::str_detect(string = lines[endFunLine], "\\{")
    checkClose <- stringr::str_detect(string = lines[endFunLine], "\\}")

    if (length(checkOpen) == 0 || length(checkClose) == 0) {
      checkOpen <- FALSE
      checkClose <- FALSE
    }

    if (is.na(checkOpen) || is.na(checkClose)) {
      cntOpen <- max(c(cntOpen, cntClosed))
      cntClosed <- max(c(cntOpen, cntClosed))
    } else {
      if (checkOpen) {
        cntOpen <- cntOpen + 1
      }

      if (checkClose) {
        cntClosed <- cntClosed + 1
      }
    }

    if (cntOpen == cntClosed) {
      endFunLine <- endFunLine
      switchOff <- TRUE
    } else {
      endFunLine <- endFunLine + 1
    }
  }

  complexity <- NA

  tryCatch({
    complexity <- cyclocomp::cyclocomp(eval(parse(text = lines[line:endFunLine])))
  }, error = function(cond) {
    complexity <- NA
  })

  outDf <- data.frame(
    start = startFunLine,
    end = endFunLine,
    nArgs = nArgs,
    cycloComp = complexity)

  return(outDf)
}

#' goToBody
#'
#' Helper function for getBodyIndices and getDefinedFunctions. Computes the
#' starting index of the function body.
#'
#' @param line Line number of the constructor of the function.
#' @param lines Lines of the R-file to be investigated.
#'
#' @return Returns a numeric index.
goToBody <- function(line, lines) {
  startFun <- FALSE
  line <- line
  while (startFun == FALSE) {
    checkOpen <- stringr::str_detect(string = lines[line], "\\{")

    if (is.na(checkOpen)) {
      return(line)
    }
    if (checkOpen) {
      startFun <- TRUE
    } else {
      line <- line + 1
    }
  }
  return(line)
}
