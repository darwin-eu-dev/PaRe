#' funsUsedInLine
#'
#' Support function for funsUsedInFile.
#'
#' @param lines (\link[base]{c}) of (\link[base]{character})
#' @param name (\link[base]{character})
#' @param i (\link[base]{numeric})
#' @param verbose (\link[base]{logical}: FALSE)
#'
#' @return (\link[base]{data.frame})
#' | column |              data type |
#' | ------ | ---------------------- |
#' |    pkg | \link[base]{character} |
#' |    fun | \link[base]{character} |
#' |   line |   \link[base]{numeric} |
funsUsedInLine <- function(lines, name, i, verbose = FALSE) {
  line <- lines[i]

  if (!startsWith(line, "#") && !is.na(line) && length(line) >= 0 && line != "NA") {
    funVec <- line |>
      stringr::str_split(pattern = "\\w+\\$", simplify = TRUE) |>
      paste(collapse = "") |>
      stringr::str_replace_all("[\"\'\`].+[\"\'\`]+", "") |>
      stringr::str_extract_all(pattern = "[\\w\\.]+(::)?[\\w\\.]+\\(") |>
      unlist() |>
      stringr::str_remove_all(pattern = "\\(")

    if ("do.call" %in% funVec) {
      funVec <- funVec |>
        append(getDoCallFromLines(lines))
    }

    if (any(stringr::str_detect(string = funVec, pattern = "[\\w]+?[Aa]pply"))) {
      funVec <- funVec |>
        append(getApplyFromLines(lines))
    }

    if ("plyr::dlply" %in% funVec) {
      funVec <- funVec |>
        append(getDlplyCallFromLines(lines))
    }

    funVec <- funVec |>
      stringr::str_split(pattern = "::")

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

      dt <- data.table::data.table(t(sapply(funVec, unlist)))
      names(dt) <- c("pkg", "fun")

      return(dt[, file := name][, line := i])
    } else {
      if (verbose == TRUE) {
        message(paste0("No functions found for line: ", i))
      }
    }
  }
}


#' funsUsedInFile
#'
#' Support function
#'
#' @param files (\link[base]{list}) of (\link[PaRe]{File})
#' @param verbose (\link[base]{logical})
#'
#' @return (\link[base]{list})
funsUsedInFile <- function(files, verbose = FALSE) {
  lapply(X = files, FUN = function(file) {
    if (verbose) {
      message(paste0("Started on file: ", file$getName()))
    }

    lines <- file$getLines()

    out <- lapply(seq_len(length(lines)), function(i) {
        funsUsedInLine(lines = file$getLines(), name = file$getName(), i = i)
      }) |>
      data.table::rbindlist()
  })
}

#' summariseFunctionUse
#'
#' Summarise functions used in R package.
#'
#' @export
#'
#' @param repo (\link[PaRe]{Repository})\cr
#' Repository object.
#' @param verbose (\link[base]{logical}: FALSE)\cr
#' Prints message to console which file is currently being worked on.
#'
#' @return (\link[data.table]{data.table})
#' | column |              data type |
#' | ------ | ---------------------- |
#' |   file | \link[base]{character} |
#' |   line |   \link[base]{numeric} |
#' |    pkg | \link[base]{character} |
#' |    fun | \link[base]{character} |
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
#'   # Run getFunctionUse on the Repository object.
#'   getFunctionUse(repo = repo, verbose = TRUE)
#' }
getFunctionUse <- function(repo, verbose = FALSE) {
  files <- repo$getRFiles()

  funUse <- funsUsedInFile(files, verbose)

  if (length(funUse) == 0) {
    warning("No functions found, output will be empty")
    funUse <- data.table::data.table(
      file = character(0),
      line = numeric(0),
      pkg = character(0),
      fun = character(0)
    )
  }

  funUse <- funUse |>
    data.table::rbindlist(fill = TRUE)

  funUse <- funUse[
    j = .(file, line, pkg, fun)] |>
    data.table::setorder(file, line, pkg, fun)

  funUse$pkg[funUse$fun %in% ls("package:base")] <- "base"

  defFuns <- getDefinedFunctions(repo)
  description <- repo$getDescription()
  funUse <- funUse[fun %in% defFuns$name, pkg := description$get_field("Package")]
  return(funUse)
}
